# The Eff Monad

## Chapter Goals

In the last chapter, we introduced applicative functors, an abstraction which we used to deal with _side-effects_: optional values, error messages and validation. This chapter will introduce another abstraction for dealing with side-effects in a more expressive way: _monads_.

The goal of this chapter is to explain why monads are a useful abstraction, and their connection with _do notation_. We will build upon the address book example of the previous chapters, by using a particular monad to handle the side-effects of building a user interface in the browser. The monad we will use is an important monad in PureScript - the `Eff` monad - used to encapsulate so-called _native_ effects.

## Project Setup

The source code for this project builds on the source for the previous chapter, and as such, uses the Grunt build script to include its source files.

The code is broken into three modules:

- `Main` which provides the entry point to the application.
- `Data.AddressBook.UI` which provides functions to render the user interface in the browser.
- `Control.Monad.Eff.DOM` which provides a simple library of functions for working with the DOM.

The project adds the `purescript-eff` package as a Bower dependency. `purescript-eff` defines the `Eff` monad, which will be the subject of the second half of the chapter.

To run this project, build with Grunt, and open the `html/index.html` file in your web browser.

## Monads and Do Notation

Do notation was first introduced when we covered _array comprehensions_. Array comprehensions provide syntactic sugar for the `concatMap` function from the `Data.Array` module.

Consider the following example. Suppose we throw two dice and want to count the number of ways in which we can score a total of `n`. We could do this using the following non-deterministic algorithm:

- _Choose_ the value `x` of the first throw.
- _Choose_ the value `y` of the second throw. 
- If the sum of `x` and `y` is `n` then return the pair `[x, y]`, else fail.

Array comprehensions allow us to write this non-deterministic algorithm in a natural way:

```haskell
import Prelude
import Data.Array

countThrows :: Int -> Array (Array Int)
countThrows n = do
  x <- 1 .. 6
  y <- 1 .. 6
  if x + y == n 
    then return [x, y] 
    else empty
```

We can see that this function works in PSCi:

```text
> countThrows 10
[[4,6],[5,5],[6,4]]

> countThrows 12  
[[6,6]]
```

In the last chapter, we formed an intuition for the `Maybe` applicative functor, embedding PureScript functions into a larger programming language supporting _optional values_. In the same way, we can form an intuition for the _array monad_, embedding PureScript functions into a larger programming language supporting _non-deterministic choice_.

In general, a _monad_ for some type constructor `m` provides a way to use do notation with values of type `m a`. Note that in the array comprehension above, every line contains a computation of type `Array a` for some type `a`. In general, every line of a do notation block will contain a computation of type `m a` for some type `a` and our monad `m`. The monad `m` must be the same on every line (i.e. we fix the side-effect), but the types `a` can differ (i.e. individual computations can have different result types).

Here is another example of do notation, this type applied to the type constructor `Maybe`. Suppose we have some type `XML` representing XML nodes, and an operator 

```haskell
(</>) :: XML -> String -> Maybe XML
```

which looks for a child element of a node, and returns `Nothing` if no such element exists.

In this case, we can look for a deeply-nested element by using do notation. Suppose we wanted to read a user's city from a user profile which had been encoded as an XML document:

```haskell
userCity :: XML -> Maybe XML
userCity root = do
  prof <- root </> "profile"
  addr <- prof </> "address"
  city <- addr </> "city"
  return city
``` 

The `userCity` function looks for a child element `profile`, an element `address` inside the `profile` element, and finally an element `city` inside the `address` element. If any of these elements are missing, the return value will be `Nothing`. Otherwise, the return value is constructed using `Just` from the `city` node.

Remember, the `return` function in the last line is not a keyword. It is actually a synonym for the `pure` function, which is defined for every `Applicative` functor. Despite what you might expect from JavaScript, it does _not_ correspond to early termination of the function in any sense. It would be equally valid to change the last line to `Just city`.

## The Monad Type Class

The `Monad` type class is defined as follows:

```haskell
class (Apply m) <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

class (Applicative m, Bind m) <= Monad m
```

The key function here is `bind`, defined in the `Bind` type class. Just like for the `<$>` and `<*>` operators in the `Functor` and `Apply` type classes, the Prelude defines an infix alias `>>=` for the `bind` function.

The `Monad` type class extends `Bind` with the operations of the `Applicative` type class that we have already seen.

It will be useful to see some examples of the `Bind` type class. A sensible definition for `Bind` on arrays can be given as follows:

```haskell
instance bindArray :: Bind Array where
  bind xs f = concatMap f xs
```

This explains the connection between array comprehensions and the `concatMap` function that has been alluded to before.

Here is an implementation of `Bind` for the `Maybe` type constructor:

```haskell
instance bindMaybe :: Bind Maybe where
  bind Nothing  _ = Nothing
  bind (Just a) f = f a
```

This definition solidifies the intuition that missing values are propagated through a do notation block.

Let's see how the `Bind` type class is related to do notation. Consider a simple do notation block which starts by binding a value from the result of some computation:

```haskell
do value <- someComputation
   whatToDoNext
```

Every time the PureScript compiler sees this pattern, it replaces the code with this:

```haskell
bind someComputation \value -> whatToDoNext
```

or, written infix:

```haskell
someComputation >>= \value -> whatToDoNext
```

The computation `whatToDoNext` is allowed to depend on `value`.

If there are multiple binds involved, this rule is applied multiple times, starting from the top. For example, the `userCity` example that we saw earlier gets desugared as follows:

```haskell
userCity :: XML -> Maybe XML
userCity root = 
  root </> "profile" >>= \prof ->
    prof </> "address" >>= \addr ->
      addr </> "city" >>= \city ->
        return city
```

It is worth noting that code expressed using do notation is often much clearer than the equivalent code using the `>>=` operator. However, writing binds explicitly using `>>=` can often lead to opportunities to write code in _point-free_ form - but the usual warnings about readability apply.

## Monad Laws

The `Monad` type class comes equipped with three laws, called the _monad laws_. These tell us what we can expect from sensible implementations of the `Monad` type class.

It is simplest to explain these laws using do notation.

### Identity Laws

The _right-identity_ law is the simplest of the three laws. It tells us that we can eliminate a call to `return` if it is the last expression in a do notation block:

```haskell
do 
  x <- expr
  return x
```

The right-identity law says that this is equivalent to just `expr`.

The _left-identity_ law states that we can eliminate a call to `return` if it is the first expression in a do notation block:

```haskell
do 
  x <- return y
  next
``` 

This code is equivalent to `next`, after the name `x` has been replaced with the expression `y`.

The last law is the _associativity law_. It tells us how to deal with nested do notation blocks. It states that the following piece of code:

```haskell
c1 = do 
  y <- do 
    x <- m1
    m2
  m3
```

is equivalent to this code:

```haskell  
c2 = do 
  x <- m1
  y <- m2
  m3
```

Each of these computations involves three monadic expression `m1`, `m2` and `m3`. In each case, the result of `m1` is eventually bound to the name `x`, and the result of `m2` is bound to the name `y`. 

In `c1`, the two expressions `m1` and `m2` are grouped into their own do notation block.

In `c2`, all three expressions `m1`, `m2` and `m3` appear in the same do notation block.

The associativity law tells us that it is safe to simplify nested do notation blocks in this way.

_Note_ that by the definition of how do notation gets desugared into calls to `bind`, both of `c1` and `c2` are also equivalent to this code:

```haskell  
c3 = do 
  x <- m1
  do
    y <- m2
    m3
```

## Folding With Monads

As an example of working with monads abstractly, this section will present a function which works with any type constructor in the `Monad` type class. This should serve to solidify the intuition that monadic code corresponds to programming "in a larger language" with side-effects, and also illustrate the generality which programming with monads brings.

The function we will write is called `foldM`. It generalizes the `foldl` function that we met earlier to a monadic context. Here is its type signature:

```haskell
foldM :: forall m a b. (Monad m) => (a -> b -> m a) -> a -> List b -> m a 
```

Notice that this is the same as the type of `foldl`, except for the appearance of the monad `m`:

```haskell
foldl :: forall a b. (a -> b -> a) -> a -> List b -> a
```

Intuitively, `foldM` performs a fold over a list in some context supporting some set of side-effects. 

For example, if we picked `m` to be `Maybe`, then our fold would be allowed to fail by returning `Nothing` at any stage - every step returns an optional result, and the result of the fold is therefore also optional.

If we picked `m` to be the `Array` type constructor, then every step of the fold would be allowed to return zero or more results, and the fold would proceed to the next step independently for each result. At the end, the set of results would consist of all folds over all possible paths. This corresponds to a traversal of a graph!

To write `foldM`, we can simply break the input list into cases.

If the list is empty, then to produce the result of type `a`, we only have one option: we have to return the second argument:

```haskell
foldM _ a Nil = return a
```

Note that we have to use `return` to lift `a` into the monad `m`.

What if the list is non-empty? In that case, we have a value of type `a`, a value of type `b`, and a function of type `a -> b -> m a`. If we apply the function, we obtain a monadic result of type `m a`. We can bind the result of this computation with a backwards arrow `<-`.

It only remains to recurse on the tail of the list. The implementation is simple:

```haskell
foldM f a (Cons b bs) = do
  a' <- f a b
  foldM f a' bs
```

Note that this implementation is almost identical to that of `foldl` on lists, with the exception of do notation.

We can define and test this function in PSCi. Here is an example - suppose we defined a "safe division" function on integers, which tested for division by zero and used the `Maybe` type constructor to indicate failure:

```haskell
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)
```
  
Then we can use `foldM` to express iterated safe division:  
  
```text
> import Data.List
> foldM safeDivide 100 (toList [5, 2, 2])
Just (5)

> foldM safeDivide 100 (toList [2, 0, 4])
Nothing
```

The `foldM safeDivide` function returns `Nothing` if a division by zero was attempted at any point. Otherwise it returns the result of repeatedly dividing the accumulator, wrapped in the `Just` constructor.

## Monads and Applicatives

Every instance of the `Monad` type class is also an instance of the `Applicative` type class, by virtue of the superclass relationship between the two classes.

However, there is also an implementation of the `Applicative` type class which comes "for free" for any instance of `Monad`, given by the `ap` function:

```haskell
ap :: forall m. (Monad m) => m (a -> b) -> m a -> m b
ap mf ma = do
  f <- mf
  a <- ma
  return (f a)
```

If `m` is a law-abiding member of the `Monad` type class, then there is a valid `Applicative` instance for which `pure` is given by `return`, and `apply` is given by `ap`.

The interested reader can check that `ap` agrees with `apply` for the monads we have already encountered: `Array`, `Maybe`, `Either e` and `V e`.

If every monad is also an applicative functor, then we should be able to apply our intuition for applicative functors to every monad. In particular, we can reasonably expect a monad to correspond, in some sense, to programming "in a larger language" augmented with some set of additional side-effects. We should be able to lift functions of arbitrary arities, using `map` and `apply`, into this new language.

But monads allow us to do more than we could do with just applicative functors, and the key difference is highlighted by the syntax of do notation. Consider the `userCity` example again, in which we looked for a user's city in an XML document which encoded their user profile:

```haskell
userCity :: XML -> Maybe XML
userCity root = do
  prof <- root </> "profile"
  addr <- prof </> "address"
  city <- addr </> "city"
  return city
```

Do notation allows the second computation to depend on the result `prof` of the first, and the third computation to depend on the result `addr` of the second, and so on. This dependence on previous values is not possible using only the interface of the `Applicative` type class.

Try writing `userCity` using only `pure` and `apply`: you will see that it is impossible. Applicatives only allow us to lift function arguments which are independent of each other, but monads allow us to write computations which involve more interesting data dependencies.

In the last chapter, we saw that the `Applicative` type class can be used to express parallelism. This was precisely because the function arguments being lifted were independent of one another. Since the `Monad` type class allows computations to depend on the results of previous computations, the same does not apply - a monad has to combine its side-effects in sequence.

X> ## Exercises
X> 
X> 1. (Easy) Look up the types of the `head` and `tail` functions from the `Data.Array` module in the `purescript-arrays` package. Use do notation with the `Maybe` monad to combine these functions into a function `third` which returns the third element of an array with three or more elements. Your function should return an appropriate `Maybe` type.
X> 1. (Medium) Write a function `sums` which uses `foldM` to determine all possible totals that could be made using a set of coins. The coins will be specified as an array which contains the value of each coin. Your function should have the following result:
X> 
X>     ```text
X>     > sums []
X>     [0]
X> 
X>     > sums [1, 2, 10]
X>     [0,1,2,3,10,11,12,13]
X>     ```
X> 
X>     _Hint_: This function can be written as a one-liner using `foldM`. You might want to use the `nub` and `sort` functions to remove duplicates and sort the result respectively.
X> 1. (Medium) Confirm that the `ap` function and the `apply` operator agree for the `Maybe` monad.
X> 1. (Medium) Verify that the monad laws hold for the `Monad` instance for the `Maybe` type, as defined in the `purescript-maybe` package. 
X> 1. (Medium) Write a function `filterM` which generalizes the `filter` function on lists. Your function should have the following type signature:
X> 
X>     ```haskell
X>     filterM :: forall m a. (Monad m) => (a -> m Boolean) -> List a -> m (List a)
X>     ```
X> 
X>     Test your function in PSCi using the `Maybe` and `Array` monads.
X> 1. (Difficult) Every monad has a default `Functor` instance given by:
X> 
X>     ```haskell
X>     map f a = do
X>       x <- a
X>       return (f a)
X>     ```
X> 
X>     Use the monad laws to prove that for any monad, the following holds: 
X> 
X>     ```haskell
X>     lift2 f (return a) (return b) = return (f a b)
X>     ```
X>     
X>     where the `Applicative` instance uses the `ap` function defined above. Recall that `lift2` was defined as follows:
X>    
X>     ```haskell
X>     lift2 :: forall f a b c. (Applicative f). (a -> b -> c) -> f a -> f b -> f c
X>     lift2 f a b = f <$> a <*> b
X>     ```

## Native Effects

We will now look at one particular monad which is of central importance in PureScript - the `Eff` monad.

The `Eff` monad is defined in the Prelude, in the `Control.Monad.Eff` module. It is used to manage so-called _native_ side-effects.

What are native side-effects? They are the side-effects which distinguish JavaScript expressions from idiomatic PureScript expressions, which typically are free from side-effects. Some examples of native effects are:

- Console IO
- Random number generation
- Exceptions
- Reading/writing mutable state

And in the browser:

- DOM manipulation
- XMLHttpRequest / AJAX calls
- Interacting with a websocket
- Writing/reading to/from local storage

We have already seen plenty of examples of "non-native" side-effects:

- Optional values, as represented by the `Maybe` data type
- Errors, as represented by the `Either` data type
- Multi-functions, as represented by arrays or lists

Note that the distinction is subtle. It is true, for example, that an error message is a possible side-effect of a JavaScript expression, in the form of an exception. In that sense, exceptions do represent native side-effects, and it is possible to represent them using `Eff`. However, error messages implemented using `Either` are not a side-effect of the JavaScript runtime, and so it is not appropriate to implement error messages in that style using `Eff`. So it is not the effect itself which is native, but rather how it is implemented at runtime.

## Side-Effects and Purity

In a pure language like PureScript, one question which presents itself is: without side-effects, how can one write useful real-world code?

The answer is that PureScript does not aim to eliminate side-effects. It aims to represent side-effects in such a way that pure computations can be distinguished from computations with side-effects in the type system. In this sense, the language is still pure.

Values with side-effects have different types from pure values. As such, it is not possible to pass a side-effecting argument to a function, for example, and have side-effects performed unexpectedly. 

The only way in which side-effects managed by the `Eff` monad will be presented is to run a computation of type `Eff eff a` from JavaScript.

The Pulp build tool (and other tools) provide a shortcut, by generating additional JavaScript to invoke the `main` computation when the application starts. `main` is required to be a computation in the `Eff` monad.

In this way, we know exactly what side-effects to expect: exactly those used by `main`. In addition, we can use the `Eff` monad to restrict what types of side-effects `main` is allowed to have, so that we can say with certainty for example, that our application will interact with the console, but nothing else.

## The Eff Monad

The goal of the `Eff` monad is to provide a well-typed API for computations with side-effects, while at the same time generating efficient Javascript. It is also called the monad of _extensible effects_, which will be explained shortly.

Here is an example. It uses the `purescript-random` package, which defines functions for generating random numbers:

```haskell
module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console

main = do
  n <- random
  print n
```  
  
If this file is saved as `src/Main.purs`, then it can be compiled and run using Pulp:

```text
$ pulp run
```

Running this command, you will see a randomly chosen number between `0` and `1` printed to the console.

This program uses do notation to combine two types of native effects provided by the Javascript runtime: random number generation and console IO.

## Extensible Effects

We can inspect the type of main by opening the module in PSCi:

```text
> :type Main.main

forall eff. Eff (console :: CONSOLE, random :: RANDOM | eff) Unit
```

This type looks quite complicated, but is easily explained by analogy with PureScript’s records.

Consider a simple function which uses a record type:

```haskell
fullName person = person.firstName ++ " " ++ person.lastName
```

This function creates a full name string from a record containing `firstName` and `lastName` properties. If you find the type of this function in PSCi as before, you will see this:

```haskell
forall r. { firstName :: String, lastName :: String | r } -> String
```
 
This type reads as follows: “`fullName` takes a record with `firstName` and `lastName` fields _and any other properties_ and returns a `String`”.

That is, `fullName` does not care if you pass a record with more fields, as long as the `firstName` and `lastName` properties are present:

```text
> firstName { firstName: "Phil", lastName: "Freeman", location: "Los Angeles" }

Phil Freeman
```

Similarly, the type of `main` above can be interpreted as follows: “`main` is a _computation with side-effects_, which can be run in any environment which supports random number generation and console IO, _and any other types of side effect_, and which returns a value of type `Unit`”.

This is the origin of the name “extensible effects”: we can always extend the set of side-effects, as long as we can support the set of effects that we need.

## Interleaving Effects

This extensibility allows code in the `Eff` monad to _interleave_ different types of side-effect.

The `random` function which we used has the following type:

```haskell
forall eff1. Eff (random :: RANDOM | eff1) Number
```

The set of effects `(random :: Random | eff1)` here is _not_ the same as those appearing in `main`.

However, we can _instantiate_ the type of `random` in such a way that the effects do match. If we choose `eff1` to be `(console :: CONSOLE | eff)`, then the two sets of effects become equal, up to reordering.

Similarly, `print` has a type which can be specialized to match the effects of `main`:

```haskell
forall eff2. (Show a) => a -> Eff (console :: CONSOLE | eff2) Unit
```

This time we have to choose `eff2` to be `(random :: RANDOM | eff)`.

The point is that the types of `random` and `print` indicate the side-effects which they contain, but in such a way that other side-effects can be _mixed-in_, to build larger computations with larger sets of side-effects.

Note that we don't have to give a type for `main`. `psc` will find a most general type for `main` given the polymorphic types of `random` and `print`.

## The Kind of Eff

The type of `main` is unlike other types we've seen before. To explain it, we need to consider the _kind_ of `Eff`. Recall that types are classified by their kinds just like values are classified by their types. So far, we've only seen kinds built from `*` (the kind of types) and `->` (which builds kinds for type constructors).

To find the kind of `Eff`, use the `:kind` command in PSCi:

```text
> :kind Control.Monad.Eff.Eff

 # ! -> * -> *
```

There are two symbols here that we have not seen before.

`!` is the kind of _effects_, which represents _type-level labels_ for different types of side-effects. To understand this, note that the two labels we saw in `main` above both have kind `!`:

```text
> :kind Control.Monad.Eff.Console.CONSOLE
 
  !
 
> :kind Control.Monad.Eff.Random.RANDOM

  !
```

The `#` kind constructor is used to construct kinds for _rows_, i.e. unordered, labelled sets.

So `Eff` is parameterized by a row of effects, and its return type. That is, the first argument to `Eff` is an unordered, labelled set of effect types, and the second argument is the return type.

We can now read the type of `main` above:

```text
forall eff. Eff (console :: CONSOLE, random :: RANDOM | eff) Unit
```

The first argument to `Eff` is `(console :: CONSOLE, random :: RANDOM | eff)`. This is a row which contains the `CONSOLE` effect and the `RANDOM` effect. The pipe symbol `|` separates the labelled effects from the _row variable_ `eff` which represents _any other side-effects_ we might want to mix in.

The second argument to `Eff` is `Unit`, which is the return type of the computation.

## Objects And Rows

Considering the kind of `Eff` allows us to make a deeper connection between extensible effects and records.

Take the function we defined above:

```haskell
fullName :: forall r. { firstName :: String, lastName :: String | r } -> String
fullName person = person.firstName ++ " " ++ person.lastName
```

The kind of the type on the left of the function arrow must be `*`, because only types of kind `*` have values.

The curly braces are actually syntactic sugar, and the full type as understood by the PureScript compiler is as follows:

```haskell
fullName :: forall r. Object (firstName :: String, lastName :: String | r) -> String
```

Note that the curly braces have been removed, and there is an extra `Object` constructor. `Object` is a built-in type constructor defined in the `Prim` module. If we find its kind, we see the following:

```text
> :kind Object

  # * -> *
```

That is, `Object` is a type constructor which takes a _row of types_ and constructs a type. This is what allows us to write row-polymorphic functions on records.

The type system uses the same machinery to handle extensible effects as is used for row-polymorphic records (or _extensible records_). The only difference is the _kind_ of the types appearing in the labels. Records are parameterized by a row of types, and `Eff` is parameterized by a row of effects.

The same type system feature could even be used to build other types which were parameterized on rows of type constructors, or even rows of other rows!

## Fine-Grained Effects

Type annotations are usually not required when using `Eff`, since rows of effects can be inferred, but they can be used to indicate to the compiler which effects are expected in a computation.

If we annotate the previous example with a _closed_ row of effects:

``` haskell
main :: Eff (console :: CONSOLE, random :: RANDOM) Unit
main = do
  n <- random
  print n
```

(note the lack of the row variable `eff` here), then we cannot accidentally include a subcomputation which makes use of a different type of effect. In this way, we can control the side-effects that our code is allowed to have.

## Handlers and Actions

Functions such as `print` and `random` are called _actions_. Actions have the `Eff` type on the right hand side of their functions, and their purpose is to _introduce_ new effects.

This is in contrast to _handlers_, in which the `Eff` type appears as the type of a function argument. While actions _add_ to the set of required effects, a handler usually _subtracts_ effects from the set.

As an example, consider the `purescript-exceptions` package. It defines two functions, `throwException` and `catchException`:

```haskell
throwException :: forall a eff. Error -> Eff (err :: EXCEPTION | eff) a

catchException :: forall a eff. (Error -> Eff eff a) -> 
                                Eff (err :: EXCEPTION | eff) a -> 
                                Eff eff a
```

`throwException` is an action. `Eff` appears on the right hand side, and introduces the new `EXCEPTION` effect.

`catchException` is a handler. `Eff` appears as the type of the second function argument, and the overall effect is to _remove_ the `EXCEPTION` effect.

This is useful, because the type system can be used to delimit portions of code which require a particular effect. That code can then be wrapped in a handler, allowing it to be embedded inside a block of code which does not allow that effect.

For example, we can write a piece of code which throws exceptions using the `Exception` effect, then wrap that code using `catchException` to embed the computation in a piece of code which does not allow exceptions.

Suppose we wanted to read our application's configuration from a JSON document. The process of parsing the document might result in an exception. The process of reading and parsing the configuration could be written as a function with this type signature:

``` haskell
readConfig :: forall eff. Eff (err :: EXCEPTION | eff) Config
```

Then, in the `main` function, we could use `catchException` to handle the `EXCEPTION` effect:

```haskell
main = do
  config <- catchException printException readConfig
  runApplication config
  
  where
  printException e = trace (message e)
```

The `purescript-eff` package also defines the `runPure` handler, which takes a computation with _no_ side-effects, and safely evaluates it as a pure value:

```haskell
type Pure a = forall eff. Eff eff a

runPure :: forall a. Pure a -> a
```

## Mutable State

There is another effect defined in the core libraries: the `ST` effect.

The `ST` effect is used to manipulate mutable state. As pure functional programmers, we know that shared mutable state can be problematic. However, the `ST` effect uses the type system to restrict sharing in such a way that only safe _local_ mutation is allowed.

The `ST` effect is defined in the `Control.Monad.ST` module. To see how it works, we need to look at the types of its actions:

```haskell
newSTRef :: forall a h eff. a -> Eff (st :: ST h | eff) (STRef h a)

readSTRef :: forall a h eff. STRef h a -> Eff (st :: ST h | eff) a

writeSTRef :: forall a h eff. STRef h a -> a -> Eff (st :: ST h | eff) a

modifySTRef :: forall a h eff. STRef h a -> (a -> a) -> Eff (st :: ST h | eff) a
```

`newSTRef` is used to create a new mutable reference cell of type `STRef h a`, which can be read using the `readSTRef` action, and modified using the `writeSTRef` and `modifySTRef` actions. The type `a` is the type of the value stored in the cell, and the type `h` is used to indicate a _memory region_ in the type system.

Here is an example. Suppose we want to simulate the movement of a particle falling under gravity by iterating a simple update function over a large number of small time steps.

We can do this by creating a mutable reference cell to hold the position and velocity of the particle, and then using a for loop (using the `forE` action in `Control.Monad.Eff`) to update the value stored in that cell:

```haskell
import Control.Monad.Eff
import Control.Monad.ST

simulate :: forall eff h. Number -> Number -> Number -> Eff (st :: ST h | eff) Number
simulate x0 v0 time = do
  ref <- newSTRef { x: x0, v: v0 }
  forE 0.0 (time * 1000.0) $ \i -> do
    modifySTRef ref (\o ->
      { v: o.v - 9.81 * 0.001
      , x: o.x + o.v * 0.001
      })
    return unit
  final <- readSTRef ref
  return final.x
```

At the end of the computation, we read the final value of the reference cell, and return the position of the particle.

Note that even though this function uses mutable state, it is still a pure function, so long as the reference cell `ref` is not allowed to be used by other parts of the program. We will see that this is exactly what the `ST` effect disallows.

To run a computation with the `ST` effect, we have to use the `runST` function: 

```haskell
runST :: forall a eff. (forall h. Eff (st :: ST h | eff) a) -> Eff eff a
```

The thing to notice here is that the region type `h` is quantified _inside the parentheses_ on the left of the function arrow. That means that whatever action we pass to `runST` has to work with _any region_ `h` whatsoever.

However, once a reference cell has been created by `newSTRef`, its region type is already fixed, so it would be a type error to try to use the reference cell outside the code delimited by `runST`.  This is what allows `runST` to safely remove the `ST` effect!

In fact, since `ST` is the only effect in our example, we can use `runST` in conjunction with `runPure` to turn `simulate` into a pure function:

```haskell
simulate' :: Number -> Number -> Number -> Number
simulate' x0 v0 time = runPure (runST (simulate x0 v0 time))
```

You can even try running this function in PSCi:

```text
> Main.simulate' 100.0 0.0 0.0
100.00

> Main.simulate' 100.0 0.0 1.0
95.10

> Main.simulate' 100.0 0.0 2.0
80.39

> Main.simulate' 100.0 0.0 3.0
55.87

> Main.simulate' 100.0 0.0 4.0
21.54
```

In fact, if we inline the definition of `simulate` at the call to `runST`, as follows:

```haskell
simulate :: Number -> Number -> Number -> Number
simulate x0 v0 time = runPure (runST (do
  ref <- newSTRef { x: x0, v: v0 }
  forE 0.0 (time * 1000.0) $ \i -> do
    modifySTRef ref (\o ->  
      { v: o.v - 9.81 * 0.001
      , x: o.x + o.v * 0.001  
      })
    return unit  
  final <- readSTRef ref
  return final.x))
```

then the `psc` compiler will notice that the reference cell is not allowed to escape its scope, and can safely turn it into a `var`. Here is the generated JavaScript for the body of the call to `runST`:

```javascript
var ref = { x: x0, v: v0 };

Control_Monad_Eff.forE(0.0)(time * 1000.0)(function (i) {
  return function __do() {
    ref = (function (o) {
      return {
        v: o.v - 9.81 * 1.0e-3, 
        x: o.x + o.v * 1.0e-3
      };
    })(ref);
    return Prelude.unit;
  };
})();

return ref.x;
``` 

The `ST` effect is a good way to generate short JavaScript when working with locally-scoped mutable state, especially when used together with actions like `forE`, `foreachE`, `whileE` and `untilE` which generate efficient loops in the `Eff` monad.

X> ## Exercises
X> 
X> 1. (Medium) Rewrite the `safeDivide` function to throw an exception using `throwException` if the denominator is zero.
X> 1. (Difficult) The following is a simple way to estimate pi: randomly choose a large number `N` of points in the unit square, and count the number `n` which lie in the inscribed circle. An estimate for pi is `4n/N`. Use the `RANDOM` and `ST` effects with the `forE` function to write a function which estimates pi in this way.

## DOM Effects

In the final sections of this chapter, we will apply what we have learned about effects in the `Eff` monad to the problem of working with the DOM. 

There are a number of freely-available PureScript packages for working directly with the DOM, or with open-source DOM libraries:

- [`purescript-simple-dom`](http://github.com/aktowns/purescript-simple-dom) is a set of bindings to the JavaScript DOM API.
- [`purescript-jquery`](http://github.com/purescript-contrib/purescript-jquery) is a set of bindings to the [jQuery](http://jquery.org) library.
- [`purescript-react`](http://github.com/purescript-contrib/purescript-react) is a set of bindings to the [React](http://facebook.github.io/react/) library.
- [`purescript-angular`](http://github.com/purescript-contrib/purescript-angular) is a set of bindings to the [AngularJS](http://angularjs.org/) library.
- [`purescript-virtual-dom`](http://github.com/purescript-contrib/purescript-virtual-dom) is a minimal wrapper around the [virtual-dom](http://github.com/Matt-Esch/virtual-dom) library.

However, most of these libraries are still very new, and their APIs are still undergoing changes, so to ensure that the content of this chapter is stable, the source code for this chapter includes a very minimal set of functions for working with DOM elements, in the `Control.Monad.Eff.DOM` module. 

It contains actions for querying and creating DOM elements:

```haskell
body :: forall eff. Eff (dom :: DOM | eff) Node
createElement :: forall eff. String -> Eff (dom :: DOM | eff) Node
querySelector :: forall eff. String -> Eff (dom :: DOM | eff) (Maybe Node)
```

There are actions for modifying the content and style of existing elements:

```haskell
setText :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node
setInnerHTML :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node
appendChild :: forall eff. Node -> Node -> Eff (dom :: DOM | eff) Node
addClass :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node
```

Finally, there is an action for handling DOM events:

```haskell
addEventListener :: forall eff. String -> 
                                Eff (dom :: DOM | eff) Unit -> 
                                Node -> 
                                Eff (dom :: DOM | eff) Node
```

These are the actions that we will need to build a user interface to our address book application.

## An Address Book User Interface

The user interface that we are going to build will be split across HTML and PureScript files. The HTML defines the layout of the components on the page, and the PureScript code defines the logic which controls the dynamic behavior of the form.

We are going to build a form which will allow a user to add a new entry into our address book. The form will contain text boxes for the various fields (first name, last name, city, state, etc.), and an area in which validation errors will be displayed. As the user types text into the text boxes, the validation errors will be updated.

To keep things simple, the form will have a fixed shape: the different phone number types (home, cell, work, other) will be expanded into separate text boxes.

The HTML file is completely static, except for the following code which appears at the end of the file:

```html
<script type="text/javascript" src="../dist/Main.js"></script>
```

This line includes the JavaScript code which is generated by Pulp. We place it at the end of the file to ensure that the relevant elements are on the page before we try to access them. To rebuild the `Main.js` file, Pulp can be used with the `--to` argument:

```text
$ pulp build --to dist/Main.js 
```

The `Main` module is very simple. It only defines the `main` function, which delegates to the `setupEventHandlers` function in the `Data.AddressBook.UI` module:

```haskell
main :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
main = do
  log "Attaching event handlers"
  setupEventHandlers 
```

Note, however, that this provides an example of interleaving effects: the `log` function uses the `CONSOLE` effect, and the `setupEventHandlers` function uses both the `CONSOLE` effect and the `DOM` effect (defined in `Control.Monad.Eff.DOM`), as we will see.

The `setupEventHandlers` function is also very simple (notice how we can simplify reasoning about our code, by breaking it into small functions each with a single purpose):

```haskell
setupEventHandlers :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
setupEventHandlers = do
  -- Listen for changes on form fields
  body >>= addEventListener "change" validateAndUpdateUI 
```

`setupEventHandlers` first uses the `body` action to get a reference to the body of the document, and passes the result to the `addEventListener` action using `>>=`. `addEventListener` will listen for `change` events, and call the `validateAndUpdateUI` action whenever an event is raised.

Note that, by the definition of do notation, we could have written this as follows:

```haskell
setupEventHandlers = do
  -- Listen for changes on form fields
  b <- body
  addEventListener "change" validateAndUpdateUI b
``` 

It is a matter of personal preference whether this is more or less readable. The first version is an example of _point-free_ form, since there are no function arguments named, unlike the second version which uses the name `b` for the document body.

The responsibility of the `validateAndUpdateUI` action is to run the form validators, and display a list of errors to the user if necessary. Again, it does this by delegating its responsibilities to other functions. Firstly, it uses the `querySelector` action to select the `validationErrors` element of the page. It then uses the `setInnerHTML` action to clear the contents of that element:

```haskell
validateAndUpdateUI :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
validateAndUpdateUI = do
  Just validationErrors <- querySelector "#validationErrors"	    
  setInnerHTML "" validationErrors 
```

Next, `validateAndUpdateUI` calls the `validateControls` action to run the form validators:

```haskell
  errorsOrResult <- validateControls
```

As we will soon see, `errorsOrResult` has type `Either Errors Person`, indicating either a list of errors (`Errors` is a type synonym for `Array String`, as in the previous chapter), or a `Person` record.

Finally, if the input fails validation, `validateAndUpdateUI` delegates to the `displayValidationErrors` action to show the errors on the page:
  
```haskell
  case errorsOrResult of
    Left errs -> displayValidationErrors errs
    Right result -> print result
 
  return unit
```

If the validators succeed, the code simply prints out the validated result onto the console. In a real application, of course, the next step would be to save the data to the database, or something similar.

The `validateControls` function is more interesting. Recall that its role is to run the form validators and return a result indicating either success or failure. The first thing it does is trace a debug message to the console:

```haskell
validateControls :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) 
                                    (Either Errors Person)  
validateControls = do
  trace "Running validators"
```

The `Data.AddressBook.UI` module defines a function `valueOf`, which reads a value from a form field. We will not discuss its implementation here, but only show its type signature:

```haskell
valueOf :: forall eff. String -> Eff (dom :: DOM | eff) String
```

`valueOf` takes the ID of a form element, and returns the value which the user has typed into that text box.

Next, `validateControls` builds a `Data.AddressBook.Person` data structure by reading various strings from the form fields on the page:

```haskell
  p <- person 
    <$> valueOf "#inputFirstName"
    <*> valueOf "#inputLastName"
    <*> (address <$> valueOf "#inputStreet"
                 <*> valueOf "#inputCity"
                 <*> valueOf "#inputState")
    <*> sequence [ phoneNumber HomePhone <$> valueOf "#inputHomePhone"
                 , phoneNumber CellPhone <$> valueOf "#inputCellPhone"
                 ]
```

Note that this computation uses `Eff` as an applicative functor to lift the `person`, `address` and `phoneNumber` functions. It also uses the `sequence` function from `Data.Traversable` to sequence an array of `Eff` actions, which we need to do in order to populate the phone numbers array of the `Person` data structure.

Finally, `validateControls` runs the validation function which we wrote in the previous chapter, and returns its result:

```haskell
  return $ validatePerson' p
```

The remaining piece of code is the `displayValidationErrors` function. `displayValidationErrors` takes an array of errors, and should print those strings onto the page.

The first thing the function does is to create a new `div` element to contain the errors. Since we are using the [Bootstrap library](http://getbootstrap.com/) to handle the form layout, we use the `addClass` action to set the appropriate CSS classes on the new element:

```haskell
displayValidationErrors :: forall eff. Errors -> Eff (dom :: DOM | eff) Unit
displayValidationErrors errs = do
  alert <- createElement "div"
    >>= addClass "alert" 
    >>= addClass "alert-danger"
```

Note again that this code is in point-free form. The interested reader is encouraged to try rewriting it without `>>=`.

Next, the code creates a `ul` element and adds it to the `div`:

```haskell
  ul <- createElement "ul"
  ul `appendChild` alert
```

For each error in the array, an `li` element is created and added to the list. The `setText` action is used to set the text content of the `li` element to the error message.

```haskell
  foreachE errs $ \err -> do
    li <- createElement "li" >>= setText err
    li `appendChild` ul
    return unit
```

This code uses the `foreachE` action which uses a for loop over each item in an array. It is similar to the `traverse` function which we have seen before, but is specialized for use with the `Eff` monad.

Finally, the `validationErrors` element is found using the `querySelector` action, and the `div` is appended to it.
    
```haskell  
  Just validationErrors <- querySelector "#validationErrors"
  alert `appendChild` validationErrors
  
  return unit
```

And that's it! Try the user interface out by running `pulp build --to dist/Main.js` and then opening the `html/index.html` file in your web browser.

You should be able to enter some values into the form fields and see the validation errors printed onto the page. When you have fixed all of the validation errors, you should see the validated result printed onto the browser's console.

Obviously, this user interface can be improved in a number of ways. The exercises will explore some ways in which we can make the application more usable.

X> ## Exercises
X> 
X> 1. (Easy) Modify the application to include a work phone number text box.
X> 1. (Medium) Instead of using a `ul` element to show the validation errors in a list, modify the code to create one `div` with the `alert` style for each error.
X> 1. (Medium) Rewrite the code in the `Data.AddressBook.UI` module without explicit calls to `>>=`.
X> 1. (Difficult, Extended) One problem with this user interface is that the validation errors are not displayed next to the form fields they originated from. Modify the code to fix this problem.
X>   
X>   _Hint_: the error type returned by the validator should be extended to indicate which field caused the error. You might want to use the following error type:
X>   
X>   ```haskell
X>   data ValidationError = ValidationError String Field
X>   
X>   data Field = FirstNameField
X>              | LastNameField 
X>              | StreetField
X>              | CityField
X>              | StateField
X>              | PhoneField PhoneType
X>   ```
X> 
X>   You will need to write a function which turns a `Field` into a call to the `querySelector` action to select the appropriate form element.

## Conclusion

This chapter has covered a lot of ideas about handling side-effects in PureScript:

- We met the `Monad` type class, and its connection to do notation.
- We introduced the monad laws, and saw how they allow us to transform code written using do notation.
- We saw how monads can be used abstractly, to write code which works with different side-effects.
- We saw how monads are examples of applicative functors, how both allow us to compute with side-effects, and the differences between the two approaches.
- The concept of native effects was defined, and we met the `Eff` monad, which is used to handle native side-effects.
- We saw how the `Eff` monad supports extensible effects, and how multiple types of native effect can be interleaved into the same computation.
- We saw how effects and records are handled in the kind system, and the connection between extensible records and extensible effects.
- We used the `Eff` monad to handle a variety of effects: random number generation, exceptions, console IO, mutable state, and DOM manipulation.

The `Eff` monad is a fundamental tool in real-world PureScript code. It will be used in the rest of the book to handle side-effects in a number of other use-cases.