# Callback Hell

## Chapter Goals

In this chapter, we will see how the tools we have seen so far - namely monad transformers and applicative functors - can be put to use to solve real-world problems. In particular, we will see how we can solve the problem of _callback hell_.

## Project Setup

The source code for this chapter can be compiled and run using `pulp run`. It is also necessary to install the `request` module using NPM:

```text
npm install
```

## The Problem

Asynchronous code in JavaScript typically uses _callbacks_ to structure program flow. For example, to read text from a file, the preferred approach is to use the `readFile` function and to pass a callback - a function that will be called when the text is available:

```javascript
function readText(onSuccess, onFailure) {
  var fs = require('fs');
  fs.readFile('file1.txt', { encoding: 'utf-8' }, function (error, data) {
    if (error) {
      onFailure(error.code);
    } else {
      onSuccess(data);
    }   
  });
}
```

However, if multiple asynchronous operations are involved, this can quickly lead to nested callbacks, which can result in code which is difficult to read:

```javascript
function copyFile(onSuccess, onFailure) {
  var fs = require('fs');
  fs.readFile('file1.txt', { encoding: 'utf-8' }, function (error, data1) {
    if (error) {
      onFailure(error.code);
    } else {
      fs.writeFile('file2.txt', data, { encoding: 'utf-8' }, function (error) {
        if (error) {
          onFailure(error.code);
        } else {
          onSuccess();
        }
      });
    }   
  });
}
```

One solution to this problem is to break out individual asynchronous calls into their own functions:

```javascript
function writeCopy(data, onSuccess, onFailure) {
  var fs = require('fs');
  fs.writeFile('file2.txt', data, { encoding: 'utf-8' }, function (error) {
    if (error) {
      onFailure(error.code);
    } else {
      onSuccess();
    }
  });
}

function copyFile(onSuccess, onFailure) {
  var fs = require('fs');
  fs.readFile('file1.txt', { encoding: 'utf-8' }, function (error, data) {
    if (error) {
      onFailure(error.code);
    } else {
      writeCopy(data, onSuccess, onFailure);
    }   
  });
}
```

This solution works but has some issues:

- It is necessary to pass intermediate results to asynchronous functions as function arguments, in the same way that we passed `data` to `writeCopy` above. This is fine for small functions, but if there are many callbacks involved, the data dependencies can become complex, resulting in many additional function arguments.
- There is a common pattern - the callbacks `onSuccess` and `onFailure` are usually specified as arguments to every asynchronous function - but this pattern has to be documented in module documentation which accompanies the source code. It is better to capture this pattern in the type system, and to use the type system to enforce its use.

Next, we will see how to use the techniques we have learned so far to solve these issues.

## The Continuation Monad

Let's translate the `copyFile` example above into PureScript by using the FFI. In doing so, the structure of the computation will become apparent, and we will be led naturally to a monad transformer which is defined in the `purescript-transformers` package - the continuation monad transformer `ContT`.

_Note_: in practice, it is not necessary to write these functions by hand every time. Asynchronous file IO functions can be found in the `purescript-node-fs` and `purescript-node-fs-aff` libraries.

First, we need to gives types to `readFile` and `writeFile` using the FFI. Let's start by defining some type synonyms, and a new effect for file IO:

```haskell
foreign import data FS :: !

type ErrorCode = String
type FilePath = String
```

`readFile` takes a filename and a callback which takes two arguments. If the file was read successfully, the second argument will contain the file contents, and if not, the first argument will be used to indicate the error.

In our case, we will wrap `readFile` with a function which takes two callbacks: an error callback (`onFailure`) and a result callback (`onSuccess`), much like we did with `copyFile` and `writeCopy` above. Using the multiple-argument function support from `Data.Function` for simplicity, our wrapped function `readFileImpl` might look like this:

```haskell
foreign import readFileImpl
  :: forall eff
   . Fn3 FilePath
         (String -> Eff (fs :: FS | eff) Unit)
         (ErrorCode -> Eff (fs :: FS | eff) Unit)
         (Eff (fs :: FS | eff) Unit)
```

In the foreign Javascript module, `readFileImpl` would be defined as:

```javascript
exports.readFileImpl = function(path, onSuccess, onFailure) {
  return function() {
    require('fs').readFile(path, {
      encoding: 'utf-8'
    }, function(error, data) {
      if (error) {
        onFailure(error.code)();
      } else {
        onSuccess(data)();
      }
    });
  };
};
```

This type signature indicates that `readFileImpl` takes three arguments: a file path, a success callback and an error callback, and returns an effectful computation which returns an empty (`Unit`) result. Notice that the callbacks themselves are given types which use the `Eff` monad to track their effects.

You should try to understand why this implementation has the correct runtime representation for its type.

`writeFileImpl` is very similar - it is different only in that the file content is passed to the function itself, not to the callback. Its implementation looks like this:

```haskell
foreign import writeFileImpl
  :: forall eff
   . Fn4 FilePath
         String
         (Eff (fs :: FS | eff) Unit)
         (ErrorCode -> Eff (fs :: FS | eff) Unit)
         (Eff (fs :: FS | eff) Unit)
```

```javascript
exports.writeFileImpl = function(path, data, onSuccess, onFailure) {
  return function() {
    require('fs').writeFile(path, data, {
      encoding: 'utf-8'
    }, function(error) {
      if (error) {
        onFailure(error.code)();
      } else {
        onSuccess();
      }
    });
  };
};
```

Given these FFI declarations, we can write the implementations of `readFile` and `writeFile`. These will use the `Data.Function.Uncurried` module to turn the multiple-argument FFI bindings into regular (curried) PureScript functions, and therefore have slightly more readable types.

In addition, instead of requiring two callbacks, one for successes and one for failures, we can require only a single callback which responds to _either_ successes or failures. That is, the new callback takes a value in the `Either ErrorCode` monad as its argument:

```haskell
readFile
  :: forall eff
   . FilePath
  -> (Either ErrorCode String -> Eff (fs :: FS | eff) Unit)
  -> Eff (fs :: FS | eff) Unit
readFile path k =
  runFn3 readFileImpl
         path
         (k <<< Right)
         (k <<< Left)

writeFile
  :: forall eff
   . FilePath
  -> String
  -> (Either ErrorCode Unit -> Eff (fs :: FS | eff) Unit)
  -> Eff (fs :: FS | eff) Unit
writeFile path text k =
  runFn4 writeFileImpl
         path
         text
         (k $ Right unit)
         (k <<< Left)
```

Now we can spot an important pattern. Each of these functions takes a callback which returns a value in some monad (in this case `Eff (fs :: FS | eff)`) and returns a value in _the same monad_. This means that when the first callback returns a result, that monad can be used to bind the result to the input of the next asynchronous function. In fact, that's exactly what we did by hand in the `copyFile` example.

This is the basis of the _continuation monad transformer_, which is defined in the `Control.Monad.Cont.Trans` module in `purescript-transformers`.

`ContT` is defined as a newtype as follows:

```haskell
newtype ContT r m a = ContT ((a -> m r) -> m r)
```

A _continuation_ is just another name for a callback. A continuation captures the _remainder_ of a computation - in our case, what happens after a result has been provided after an asynchronous call.

The argument to the `ContT` data constructor looks remarkably similar to the types of `readFile` and `writeFile`. In fact, if we take the type `a` to be the type `Either ErrorCode String`, `r` to be `Unit` and `m` to be the monad `Eff (fs :: FS | eff)`, we recover the right-hand side of the type of `readFile`.

This motivates the following type synonym, defining an `Async` monad, which we will use to compose asynchronous actions like `readFile` and `writeFile`:

```haskell
type Async eff = ContT Unit (Eff eff)
```

For our purposes, we will always use `ContT` to transform the `Eff` monad, and the type `r` will always be `Unit`, but this is not required.

We can treat `readFile` and `writeFile` as computations in the `Async` monad, by simply applying the `ContT` data constructor:

```haskell
readFileCont
  :: forall eff
   . FilePath
  -> Async (fs :: FS | eff) (Either ErrorCode String)
readFileCont path = ContT $ readFile path

writeFileCont
  :: forall eff
   . FilePath
  -> String
  -> Async (fs :: FS | eff) (Either ErrorCode Unit)
writeFileCont path text = ContT $ writeFile path text
```

With that, we can write our copy-file routine by simply using do notation for the `ContT` monad transformer:

```haskell
copyFileCont
  :: forall eff
   . FilePath
  -> FilePath
  -> Async (fs :: FS | eff) (Either ErrorCode Unit)
copyFileCont src dest = do
  e <- readFileCont src
  case e of
    Left err -> return $ Left err
    Right content -> writeFileCont dest content
```

Note how the asynchronous nature of `readFileCont` is hidden by the monadic bind expressed using do notation - this looks just like synchronous code, but the `ContT` monad is taking care of wiring our asynchronous functions together.

We can run this computation using the `runContT` handler by providing a continuation. The continuation represents _what to do next_, i.e. what to do when the asynchronous copy-file routine completes. For our simple example, we can just choose the `logShow` function as the continuation, which will print the result of type `Either ErrorCode Unit` to the console:

```haskell
import Prelude

import Control.Monad.Eff.Console (logShow)
import Control.Monad.Cont.Trans (runContT)

main =
  runContT
    (copyFileCont "/tmp/1.txt" "/tmp/2.txt")
    logShow
```

X> ## Exercises
X>
X> 1. (Easy) Use `readFileCont` and `writeFileCont` to write a function which concatenates two text files.
X> 1. (Medium) Use the FFI to give an appropriate type to the `setTimeout` function. Write a wrapper function which uses the `Async` monad:
X>
X>     ```haskell
X>     type Milliseconds = Number
X>
X>     foreign import data TIMEOUT :: !
X>
X>     setTimeoutCont
X>       :: forall eff
X>        . Milliseconds
X>       -> Async (timeout :: TIMEOUT | eff) Unit
X>     ```

## Putting ExceptT To Work

This solution works, but it can be improved.

In the implementation of `copyFileCont`, we had to use pattern matching to analyze the result of the `readFileCont` computation (of type `Either ErrorCode String`) to determine what to do next. However, we know that the `Either` monad has a corresponding monad transformer, `ExceptT`, so it is reasonable to expect that we should be able to use `ExceptT` with `ContT` to combine the two effects of asynchronous computation and error handling.

In fact, it is possible, and we can see why if we look at the definition of `ExceptT`:

```haskell
newtype ExceptT e m a = ExceptT (m (Either e a))
```

`ExceptT` simply changes the result of the underlying monad from `a` to `Either e a`. This means that we can rewrite `copyFileCont` by transforming our current monad stack with the `ExceptT ErrorCode` transformer. It is as simple as applying the `ExceptT` data constructor to our existing solution:

```haskell
readFileContEx
  :: forall eff
   . FilePath
  -> ExceptT ErrorCode (Async (fs :: FS | eff)) String
readFileContEx path = ExceptT $ readFileCont path

writeFileContEx
  :: forall eff
   . FilePath
  -> String
  -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
writeFileContEx path text = ExceptT $ writeFileCont path text
```

Now, our copy-file routine is much simpler, since the asynchronous error handling is hidden inside the `ExceptT` monad transformer:

```haskell
copyFileContEx
  :: forall eff
   . FilePath
  -> FilePath
  -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
copyFileContEx src dest = do
  content <- readFileContEx src
  writeFileContEx dest content
```

X> ## Exercises
X>
X> 1. (Medium) Modify your solution which concatenated two files, using `ExceptT` to handle any errors.
X> 1. (Medium) Write a function `concatenateMany` to concatenate multiple text files, given an array of input file names. _Hint_: use `traverse`.

## A HTTP Client

As another example of using `ContT` to handle asynchronous functions, we'll now look at the `Network.HTTP.Client` module from this chapter's source code. This module uses the `Async` monad to support asynchronous HTTP requests using the `request` module, which is available via NPM.

The `request` module provides a function which takes a URL and a callback, makes a HTTP(S) request and invokes the callback when the response is available, or in the event of an error. Here is an example request:

```javascript
require('request')('http://purescript.org'), function(err, _, body) {
  if (err) {
    console.error(err);
  } else {
    console.log(body);
  }
});
```

We will recreate this simple example in PureScript using the `Async` monad.

In the `Network.HTTP.Client` module, the `request` method is wrapped with a function `getImpl`:

```haskell
foreign import data HTTP :: !

type URI = String

foreign import getImpl
  :: forall eff
   . Fn3 URI
         (String -> Eff (http :: HTTP | eff) Unit)
         (String -> Eff (http :: HTTP | eff) Unit)
         (Eff (http :: HTTP | eff) Unit)
```

```javascript
exports.getImpl = function(uri, done, fail) {
  return function() {
    require('request')(uri, function(err, _, body) {
      if (err) {
        fail(err)();
      } else {
        done(body)();
      }
    });
  };
};
```

Again, we can use the `Data.Function.Uncurried` module to turn this into a regular, curried PureScript function. As before, we turn the two callbacks into a single callback, this time accepting a value of type `Either String String`, and apply the `ContT` constructor to construct an action in our `Async` monad:

```haskell
get :: forall eff.
  URI ->
  Async (http :: HTTP | eff) (Either String String)
get req = ContT \k ->
  runFn3 getImpl req (k <<< Right) (k <<< Left)
```

X> ## Exercises
X>
X> 1. (Easy) Use `runContT` to test `get` in PSCi, printing the result to the console.
X> 1. (Medium) Use `ExceptT` to write a function `getEx` which wraps `get`, as we did previously for `readFileCont` and `writeFileCont`.
X> 1. (Difficult) Write a function which saves the response body of a request to a file on disk using `getEx` and `writeFileContEx`.

## Parallel Computations

We've seen how to use the `ContT` monad and do notation to compose asynchronous computations in sequence. It would also be useful to be able to compose asynchronous computations _in parallel_.

If we are using `ContT` to transform the `Eff` monad, then we can compute in parallel simply by initiating our two computations one after the other.

The `purescript-parallel` package defines a type class `MonadPar` for monads like `Async` which support parallel execution. `MonadPar` is defined in terms of a function `par` with the following type signature:

```haskell
par :: forall m a b r
     . MonadPar m
    => (a -> b -> r)
    -> m a
    -> m b
    -> m r
```

`par` takes two asynchronous computations, and a function to combine their results, and returns a single computation which computes and combines the results in parallel.

We can use the `par` function to read two files in parallel, or to issue two HTTP requests and wait for their results in parallel.

`purescript-parallel` defines an instance of `MonadPar` for the `ContT` monad transformer applied to the `Eff eff` monad. It uses mutable references (with the `Ref` effect) to implement `MonadPar`, by keeping track of which of the two continuations has been called. When both results have been returned, we can compute the final result and pass it to the main continuation.

Here is a simple example which reads two text files in parallel, and concatenates and prints their results.

```haskell
import Prelude
import Control.Apply (lift2)
import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Parallel.Class (par)

main = flip runContT logShow $
  par (lift2 append) (readFileCont "/tmp/1.txt")
                     (readFileCont "/tmp/2.txt")
```

Note that, since `readFileCont` returns a value of type `Either ErrorCode String`, we need to lift the `append` function over the `Either` type constructor using `lift2` to form our combining function.

X> ## Exercises
X>
X> 1. (Easy) Use `par` with `get` to make two HTTP requests and collect their response bodies in parallel. Your combining function should concatenate the two response bodies, and your continuation should use `print` to print the result to the console.
X> 1. (Medium) `purescript-parallel` defines another function
X>
X>     ```haskell
X>     race :: forall m a
X>           . MonadRace m
X>          => m a
X>          -> m a
X>          -> m a
X>     ```
X>
X>     which executes two computations in parallel, and returns the result from the computation which completes first.
X>
X>     Use the `race` function in conjunction with your `setTimeoutCont` function to define a function
X>
X>     ```haskell
X>     timeout :: forall a eff
X>              . Milliseconds
X>             -> Async (timeout :: TIMEOUT | eff) a
X>             -> Async (timeout :: TIMEOUT | eff) (Maybe a)
X>     ```
X>
X>     which returns `Nothing` if the specified computation does not provide a result within the given number of milliseconds.

## An Applicative Functor For Parallelism

The type of the `par` combinator looks a lot like the type of `lift2` for the monad `m`. In fact, it is possible to define a new applicative functor for which `par` is _exactly_ `lift2`.

You might be wondering why we don't define an `Applicative` instance for `m` directly in terms of `par`. The reason is this: if a type constructor has a `Monad` instance, then it is expected that the `Monad` and `Applicative` instances agree, in the sense that `apply` is equivalent to the following function:

```haskell
ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
ap mf ma = do
  f <- mf
  a <- ma
  return (f a)
```

This allows us to refactor code, by replacing independent computations in a `do` block with a call to `apply` (or `lift2`, or a related function). However, our hypothetical `Applicative` instance differs from the `Monad` instance in the amount of parallelism: `apply` evaluates its arguments in parallel, whereas `ap` waits for its first computation to complete before executing the second.

Instead, `purescript-parallel` defines a newtype wrapper for `m a`, called `Parallel m a`, as follows:

```haskell
newtype Parallel m a = Parallel (m a)
```

We can write a function which turns a `Parallel m` computation into a computation in the `m` monad, by simply removing the outer data constructor. This is provided by the `runParallel` function:

```haskell
runParallel :: forall m a. Parallel m a -> m a
```

The inverse transformation, from `Async` to `Parallel` is called `parallel`:

```haskell
parallel :: forall m a. m a -> Parallel m a
```

The `Functor` instance for `Parallel m` is built directly from the `Functor` instance for `m`. However, in the case of the `Apply` type class, the `par` function is used instead:

```haskell
instance applyParallel :: MonadPar m => Apply (Parallel m) where
  apply f a = parallel (par ($) (runParallel f) (runParallel a))
```

`par` is used to combine a function with its argument by using function application `($)` as the combining function.

We can now rewrite our example above to read two files in parallel by using the `Parallel` type constructor:

```haskell
import Prelude
import Control.Apply (lift2)
import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Parallel.Class (parallel, runParallel)

main = flip runContT logShow $ runParallel $
  lift2 append <$> parallel (readFileCont "/tmp/1.txt")
               <*> parallel (readFileCont "/tmp/2.txt")
```

Because applicative functors support lifting of functions of arbitrary arity, we can perform more computations in parallel by using the applicative combinators. We can also benefit from all of the standard library functions which work with applicative functors, such as `traverse` and `sequence`!

We can also combine parallel computations with sequential portions of code, by using applicative combinators in a do notation block, or vice versa, using `parallel` and `runParallel` to change type constructors where appropriate.

X> ## Exercises
X>
X> 1. (Medium) Rewrite the parallel file IO example to use `ExceptT` for error handling, instead of lifting `append` with `lift2`. Your solution should use the `ExceptT` transformer to transform the `Parallel` functor.
X>
X>     Use this approach to modify your `concatenateMany` function to read the multiple input files in parallel.
X> 1. (Difficult, Extended) Suppose we are given a collection of JSON documents on disk, such that each document contains an array of references to other files on disk:
X>
X>     ```javascript
X>     { references: ['/tmp/1.json', '/tmp/2.json'] }
X>     ```
X>     
X>     Write a utility which takes a single filename as input, and spiders the JSON files on disk referenced transitively by that file, collecting a list of all referenced files.
X>
X>     Your utility should use the `purescript-foreign` library to parse the JSON documents, and should fetch files referenced by a single file in parallel.

## Conclusion

In this chapter, we have seen a practical demonstration of monad transformers:

- We saw how the common JavaScript idiom of callback-passing can be captured by the `ContT` monad transformer.
- We saw how the problem of callback hell can be solved by using do notation to express sequential asynchronous computations, and an applicative functor to express parallelism.
- We used `ExceptT` to express _asynchronous errors_.
