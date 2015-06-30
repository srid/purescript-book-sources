# Pattern Matching

## Chapter Goals

This chapter will introduce two new concepts: algebraic data types, and pattern matching. We will also briefly cover an interesting feature of the PureScript type system: row polymorphism.

Pattern matching is a common technique in functional programming and allows the developer to write compact functions which express potentially complex ideas, by breaking their implementation down into multiple cases. 

Algebraic data types are a feature of the PureScript type system which enable a similar level of expressiveness in the language of types - they are closely related to pattern matching.

The goal of the chapter will be to write a library to describe and manipulate simple vector graphics using algebraic types and pattern matching.

## Project Setup

The source code for this chapter is defined in the file `src/Data/Picture.purs`. 

The project uses some Bower packages which we have already seen, and adds the following new dependencies:

- `purescript-globals`, which provides access to some common JavaScript values and functions.
- `purescript-math`, which provides access to the JavaScript `Math` module.

The `Data.Picture` module defines a data type `Shape` for simple shapes, and a type `Picture` for collections of shapes, along with functions for working with those types.  

The module imports the `Data.Foldable` module, which provides functions for folding data structures:

```haskell
module Data.Picture where

import Data.Foldable
```

## Simple Pattern Matching

Let's begin by looking at an example. Here is a function which computes the greatest common divisor of two integers using pattern matching:

```haskell
gcd :: Number -> Number -> Number
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m then gcd (n - m) m else gcd n (m - n)
```

This algorithm is called the Euclidean Algorithm. If you search for its definition online, you will likely find a set of mathematical equations which look a lot like the code above. This is one benefit of pattern matching: it allows you to define code by cases, writing simple, declarative code which looks like a specification of a mathematical function.

A function written using pattern matching works by pairing sets of conditions with their results. Each line is called an _alternative_ or a _case_. The expressions on the left of the equals sign are called _patterns_, and each case consists of one or more patterns, separated by spaces. Cases describe which conditions the arguments must satisfy before the expression on the right of the equals sign should be evaluated and returned. Each case is tried in order, and the first case whose patterns match their inputs determines the return value.

For example, the `gcd` function is evaluated using the following steps:

- The first case is tried: if the second argument is zero, the function returns `n` (the first argument). 
- If not, the second case is tried: if the first argument is zero, the function returns `m` (the second argument). 
- Otherwise, the function evaluates and returns the expression in the last line.

Note that patterns can bind values to names - each line in the example binds one or both of the names `n` and `m` to the input values. As we learn about different kinds of patterns, we will see that different types of patterns correspond to different ways to choose names from the input arguments.

## Simple Patterns

The example code above demonstrates two types of patterns:

- Numeric literals patterns, which match something of type `Number`, only if the value matches exactly.
- Variable patterns, which bind their argument to a name

There are other types of simple patterns:

- String and Boolean literals
- Wildcard patterns, indicated with an underscore (`_`), which match any argument, and which do not bind any names.

Here are two more examples which demonstrate using these simple patterns:

```haskell
fromString :: String -> Boolean
fromString "true" = true
fromString _      = false

toString :: Boolean -> String
toString true  = "true"
toString false = "false"
```

Try these functions in `psci`.

## Guards

In the Euclidean algorithm example, we used an `if .. then .. else` expression to switch between the two alternatives when `m > n` and `m <= n`. Another option in this case would be to use a _guard_.

A guard is a boolean-valued expression which must be satisfied in addition to the constraints imposed by the patterns. Here is the Euclidean algorithm rewritten to use a guard:

```haskell
gcd :: Number -> Number -> Number
gcd n 0 = n
gcd 0 n = n
gcd n m | n > m = gcd (n - m) m 
gcd n m         = gcd n (m - n)
```

In this case, the third line uses a guard to impose the extra condition that the first argument is strictly larger than the second.

As this example demonstrates, guard appear on the left of the equals symbol, separated from the list of patterns by a pipe character (`|`).

X> ## Exercises
X> 
X> 1. (Easy) Write the factorial function using pattern matching. _Hint_. Consider the two cases zero and non-zero inputs.
X> 1. (Medium) Look up _Pascal's Rule_ for computing binomial coefficients. Use it to write a function which computes binomial coefficients using pattern matching.

## Matching Arrays

Let's look at ways in which we can match arrays using patterns. There are two types of array pattern: array literal patterns, and cons patterns.

### Array Literal Patterns

_Array literal patterns_ provide a way to match arrays of a fixed length. For example, suppose we want to write a function `isEmpty` which identifies empty arrays. We could do this by using an empty array pattern (`[]`) in the first alternative:

```haskell
isEmpty :: forall a. [a] -> Boolean
isEmpty [] = true
isEmpty _ = false
```

Here is another function which matches arrays of length five, binding each of its five elements in a different way:

```haskell
takeFive :: [Number] -> Number
takeFive [0, 1, a, b, _] = a * b
takeFive _ = 0
```

The first pattern only matches arrays with five elements, whose first and second elements are 0 and 1 respectively. In that case, the function returns the product of the third and fourth elements. In every other case, the function returns zero. For example, in `psci`:

```text
> let takeFive [0, 1, a, b, _] = a * b
      takeFive _ = 0
  
> takeFive [0, 1, 2, 3, 4]
6

> takeFive [1, 2, 3, 4, 5]
0

> takeFive []
0
```

### Cons Patterns

_Cons patterns_ are used to match non-empty arrays. They provide a way to separate the first element (or head) of an array, and the rest (or tail) of the array.

Cons patterns consist of a pattern for the head element, and a pattern for the tail, separated by a colon (`:`). For example, here is a function which sums the squares in an array of numbers:

```haskell
sumOfSquares :: [Number] -> Number
sumOfSquares [] = 0
sumOfSquares (n : ns) = n * n + sumOfSquares ns
```

This function works by separating the input into two cases: empty and non-empty arrays. If the array is empty, then the sum of squares is zero. If not, then we separate the head and tail of the array using a cons pattern, square the head element, and add it to the sum of the squares of the tail.

Here is another example. This function finds the sum of all products of adjacent pairs in a list of numbers:

```haskell
sumOfProducts :: [Number] -> Number
sumOfProducts [] = 0
sumOfProducts [_] = 0
sumOfProducts (n : m : ns) = n * m + sumOfProducts (m : ns)
```

This function splits the input into three cases: zero elements, one element, and two or more elements. In the last case, we multiply the first two elements, and recurse on the tail.

X> ## Exercises
X> 
X> 1. (Easy) Write a function `allTrue` which determines if all elements of an array of Boolean values are equal to `true`.
X> 2. (Medium) Write a function `isSorted` which tests if an array of numbers is sorted.

## Record Patterns and Row Polymorphism

_Record patterns_ are used to match - you guessed it - records.

Record patterns look like record literals, but instead of colons to separate labels from expressions, record patterns use equals symbols to separate labels from patterns. 

For example: this pattern matches any record which contains fields called `first` and `last`, and binds their values to the names `x` and `y` respectively:

```haskell
showPerson :: { first :: String, last :: String } -> String
showPerson { first = x, last = y } = y ++ ", " ++ x
```

Record patterns provide a good example of an interesting feature of the PureScript type system: _row polymorphism_. Suppose we had defined `showPerson` without a type signature above. What would its inferred type have been? Interestingly, it is not the same as the type we gave:

```text
> let showPerson { first = x, last = y } = y ++ ", " ++ x

> :t showPerson
forall r. { first :: String, last :: String | r } -> String
```

What is the type variable `r` here? Well, if we try `showPerson` in `psci`, we see something interesting:

```text
> showPerson { first: "Phil", last: "Freeman" }
"Freeman, Phil"

> showPerson { first: "Phil", last: "Freeman", location: "Los Angeles" }
"Freeman, Phil"
```

We are able to append additional fields to the record, and the `showPerson` function will still work. As long as the record contains the `first` and `last` fields of type `String`, the function application is well-typed. However, it is _not_ valid to call `showPerson` with too _few_ fields:

```text
> showPerson { first: "Phil" }

Object does not have property last
```

We can read the new type signature of `showPerson` as "takes any record with `first` and `last` fields which are `Strings` _and any other fields_, and returns a `String`". 

This function is polymorphic in the _row_ `r` of record fields, hence the name _row polymorphism_.

Note that we could have also written

```haskell
> let showPerson p = p.last ++ ", " ++ p.first
```

and `psci` would have inferred the same type.

We will see row polymorphism again later, when we discuss _extensible effects_.

## Nested Patterns

Array patterns and record patterns both combine smaller patterns to build larger patterns. For the most part, the examples above have only used simple patterns inside array patterns and record patterns, but it is important to note that patterns can be arbitrarily _nested_, which allows functions to be defined using conditions on potentially complex data types.

For example, this code combines record and array patterns to match an array of records:

```haskell
type Person = { height :: Number }

totalHeight :: [Person] -> Number
totalHeight [] = 0
totalHeight ({ height = h } : ps) = h + totalHeight ps
```

## Named Patterns

Patterns can be _named_ to bring additional names into scope when using nested patterns. Any pattern can be named by using the `@` symbol. 

For example, the following code matches any array with one or more elements, but in addition to binding the head of the array to the name `x`, the value of the array itself is bound to the name `arr`:

```haskell
dup :: forall a. [a] -> [a]
dup arr@(x : _) = x : arr
dup [] = []
```

The result is that `dup` duplicates the head element of a non-empty array:

```text
> dup [1, 2, 3]
[1, 1, 2, 3]
```

X> ## Exercises
X> 
X> 1. (Easy) Write a function `getCity` which uses record patterns to find a person's city. A Person should be represented as a record which contains an `address` field of type `Address`, and `Address` should contain the `city` field.
X> 1. (Medium) What is the most general type of the `getCity` function, taking into account row polymorphism? What about the `totalHeight` function defined above?
X> 1. (Medium) Write a function `flatten` which uses only patterns and the concatenation (`++`) operator to flatten an array of arrays into a singly-nested array. _Hint_: the function should have type `forall a. [[a]] -> [a]`.

## Case Expressions

Patterns do not only appear in top-level function declarations. It is possible to use patterns to match on an intermediate value in a computation, using a `case` expression. Case expressions provide a similar type of utility to anonymous functions: it is not always desirable to give a name to a function, and a `case` expression allows us to avoid naming a function just because we want to use a pattern.

Here is an example. This function computes "longest zero suffix" of an array (the longest suffix which sums to zero):

```haskell
lzs :: [Number] -> [Number]
lzs [] = []
lzs xs@(_ : t) = case sum xs of
  0 -> xs
  _ -> lzs t
```

For example:

```text
> lzs [1, 2, 3, 4]
[]

> lzs [1, -1, -2, 3]
[-1, -2, 3]
```

This function works by case analysis. If the array is empty, our only option is to return an empty array. If the array is non-empty, we first use a `case` expression to split into two cases. If the sum of the array is zero, we return the whole array. If not, we recurse on the tail of the array.

## Pattern Match Failures

If patterns in a case expression are tried in order, then what happens in the case when none of the patterns in a case alternatives match their inputs? In this case, the case expression will fail at runtime with a _pattern match failure_.

We can see this behaviour with a simple example:

```haskell
patternFailure :: Number -> Number
patternFailure 0 = 0
```

This function contains only a single case, which only matches a single input, zero. If we compile this file, and test in `psci` with any other argument, we will see an error at runtime:

```text
> patternFailure 10

Failed pattern match
```

Functions which return a value for any combination of inputs are called _total_ functions, and functions which do not are called _partial_.

It is generally considered better to define total functions where possible. If it is known that a function does not return a result for some valid set of inputs, it is usually better to return a value with type `Maybe a` for some `a`, using `Nothing` to indicate failure. This way, the presence or absence of a value can be indicated in a type-safe way.

Here is the `patternFailure` function, rewritten to use `Maybe Number` as the return type:

```haskell
patternFailure :: Number -> Maybe Number
patternFailure 0 = Just 0
patternFailure _ = Nothing
```

## Algebraic Data Types

This section will introduce a feature of the PureScript type system called _Algebraic Data Types_ (or _ADTs_), which are fundamentally related to pattern matching.

However, we'll first consider a motivating example, which will provide the basis of a solution to this chapter's problem of implementing a simple vector graphics library.

Suppose we wanted to define a type to represent some simple shape types: lines, rectangles, circles, text, etc. In an object oriented language, we would probably define an interface or abstract class `Shape`, and one concrete subclass for each type of shape that we wanted to be able to work with.

However, this approach has one major drawback: to work with `Shape`s abstractly, it is necessary to identify all of the operations one might wish to perform, and to define them on the `Shape` interface. It becomes difficult to add new operations without breaking modularity. 

Algebraic data types provide a type-safe way to solve this sort of problem, if the set of shapes is known in advance. It is possible to define new operations on `Shape` in a modular way, and still maintain type-safety.

Here is how `Shape` might be represented as an algebraic data type:

```haskell
data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
```

The `Point` type might also be defined as an algebraic data type, as follows:

```haskell
data Point = Point
  { x :: Number
  , y :: Number
  }
```

The `Point` data type illustrates some interesting points:

- The data carried by an ADT's constructors doesn't have to be restricted to primitive types: constructors can include records, arrays, or even other ADTs. 
- Even though ADTs are useful for describing data with multiple constructors, they can also be useful when there is only a single constructor.
- The constructors of an algebraic data type might have the same name as the ADT itself. This is quite common, and it is important not to confuse the `Point` _type constructor_ with the `Point` _data constructor_ - they live in different namespaces.

This declaration defines `Shape` as a sum of different constructors, and for each constructor identifies the data that is included. A `Shape` is either a `Circle` which contains a center `Point` and a radius (a number), or a `Rectangle`, or a `Line`, or `Text`. There are no other ways to construct a value of type `Shape`.

An algebraic data type is introduced using the `data` keyword, followed by the name of the new type and any type arguments. The type's constructors are defined after the equals symbol, and are separated by pipe characters (`|`).

Let's see another example from PureScript's standard libraries. We saw the `Maybe` type, which is used to to define optional values, earlier in the book. Here is it's definition from the `purescript-maybe` package:

```haskell
data Maybe a = Nothing | Just a
```

This example demonstrates the use of a type parameter `a`. Reading the pipe character as the word "or", its definition almost reads like English: "a value of type `Maybe a` is either `Nothing`, or `Just` a value of type `a`".

Data constructors can also be used to define recursive data structures. Here is one more example, defining a data type of singly-linked lists of elements of type `a`:

```haskell
data List a = Nil | Cons a (List a)
```

This example is taken from the `purescript-lists` package. Here, the `Nil` constructor represents an empty list, and `Cons` is used to create non-empty lists from a head element and a tail. Notice how the tail is defined using the data type `List a`, making this a recursive data type.

## Using ADTs

It is simple enough to use the constructors of an algebraic data type to construct a value: simply apply them like functions, providing arguments corresponding to the data included with the appropriate constructor.

For example, the `Line` constructor defined above required two `Point`s, so to construct a `Shape` using the `Line` constructor, we have to provide two arguments of type `Point`:

```haskell
exampleLine :: Shape
exampleLine = Line origin origin
  where
  origin :: Point
  origin = Point { x: 0, y: 0 }
```

To construct the `origin`, we apply the `Point` constructor to its single argument, which is a record.

So, constructing values of algebraic data types is simple, but how do we use them? This is where the important connection with pattern matching appears: the only way to consume a value of an algebraic data type is to use a pattern to match its constructor.

Let's see an example. Suppose we want to convert a `Shape` into a `String`. We have to use pattern matching to discover which constructor was used to construct the `Shape`. We can do this as follows:

```haskell
showPoint :: Point -> String
showPoint (Point { x = x, y = y }) =
  "(" ++ show x ++ ", " ++ show y ++ ")"

showShape :: Shape -> String
showShape (Circle c r)      = ...
showShape (Rectangle c w h) = ...
showShape (Line start end)  = ...
showShape (Text loc text) = ...
```

Each constructor can be used as a pattern, and the arguments to the constructor can themselves be bound using patterns of their own. Consider the first case of `showShape`: if the `Shape` matches the `Circle` constructor, then we bring the arguments of `Circle` (center and radius) into scope using two variable patterns, `c` and `r`. The other cases are similar.

`showPoint` is another example of pattern matching. In this case, there is only a single case, but we use a nested pattern to match the fields of the record contained inside the `Point` constructor.

X> ## Exercises
X> 
X> 1. (Easy) Construct a value of type `Shape` which represents a circle centered at the origin with radius `10`.
X> 1. (Medium) Write a function from `Shape`s to `Shape`s, which scales its argument by a factor of `2`, center the origin.
X> 1. (Medium) Write a function which extracts the text from a `Shape`. It should return `Maybe String`, and use the `Nothing` constructor if the input is not constructed using `Text`.

## Newtypes

There is an important special case of algebraic data types, called _newtypes_. Newtypes are introduced using the `newtype` keyword instead of the `data` keyword. 

Newtypes must define _exactly one_ constructor, and that constructor must take _exactly one_ argument. That is, a newtype gives a new name to an existing type. In fact, the values of a newtype have the same runtime representation as the underlying type. They are, however, distinct from the point of view of the type system. This gives an extra layer of type safety.

As an example, we might want to define newtypes as type-level aliases for `Number`, to ascribe units like pixels and inches:

```haskell
newtype Pixels = Pixels Number
newtype Inches = Inches Number
```

This way, it is impossible to pass a value of type `Pixels` to a function which expects `Inches`, but there is no runtime performance overhead.

Newtypes will become important when we cover _type classes_ in the next chapter, since they allow us to attach different behavior to a type without changing its representation at runtime.

## A Library for Vector Graphics

Let's use the data types we have defined above to create a simple library for using vector graphics.

Define a type synonym for a `Picture` - just an array of `Shape`s:

```haskell
type Picture = [Shape]
```

For debugging purposes, we'll want to be able to render a `Picture` as a `String`. The following function, defined using pattern matching, lets us do that:

```haskell
showPicture :: Picture -> String
showPicture picture = "[" ++ go picture ++ "]"
  where
  go :: Picture -> String
  go [] = ""
  go [x] = showShape x
  go (x : xs) = showShape x ++ ", " ++ go xs
``` 

Notice how the recursion is handled using a helper function defined in a `where` block. The function `go` is not accessible to users of the module, only inside the function `showPicture`.

`go` handles three cases: empty arrays, singleton arrays, and anything else. This approach avoids printing an extra comma character at the end of the string.

Let's try it out. Compile your module with `grunt` and open `psci`:

```text
$ grunt
$ psci

> import Data.Picture

> showPicture [Line (Point { x: 0, y: 0 }) (Point { x: 1, y: 1 })]

"[Line [start: (0, 0), end: (1, 1)]]"
```

## Computing Bounding Rectangles

The example code for this module contains a function `bounds` which computes the smallest bounding rectangle for a `Picture`.

The `Bounds` data type defines a bounding rectangle. It is also defined as an algebraic data type with a single constructor:

```haskell
data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }
```

`bounds` uses the `foldl` function from `Data.Foldable` to traverse the array of `Shapes` in a `Picture`, and accumulate the smallest bounding rectangle:

```haskell
bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = shapeBounds shape \/ b
```

In the base case, we need to find the smallest bounding rectangle of an empty `Picture`, and the empty bounding rectangle defined by `emptyBounds` suffices.

The accumulating function `combine` is defined in a `where` block. `combine` takes a bounding rectangle computed from `foldl`'s recursive call, and the next `Shape` in the array, and uses a user-defined operator `\/` to compute the union of the two bounding rectangles. The `shapeBounds` function computes the bounds of a single shape using pattern matching.

X> ## Exercises
X> 
X> 1. (Medium) Extend the vector graphics library with a new operation `area` which computes the area of a `Shape`. For the purposes of this exercise, the area of a piece of text is assumed to be zero.
X> 1. (Difficult) Extend the `Shape` type with a new data constructor `Clipped`, which clips another `Picture` to a rectangle. Extend the `shapeBounds` function to compute the bounds of a clipped picture. Note that this makes `Shape` into a recursive data type.

## Conclusion

In this chapter, we covered pattern matching, a basic but powerful technique from functional programming. We saw how to use simple patterns as well as array and record patterns to match parts of deep data structures.

This chapter also introduced algebraic data types, which are closely related to pattern matching. We saw how algebraic data types allow concise descriptions of data structures, and provide a modular way to extend data types with new operations.

Finally, we covered _row polymorphism_, a powerful type of abstraction which allows many idiomatic JavaScript functions to be given a type. We will see this idea again later in the book.

In the rest of the book, we will use ADTs and pattern matching extensively, so it will pay dividends to become familiar with them now. Try creating your own algebraic data types and writing functions to consume them using pattern matching.

