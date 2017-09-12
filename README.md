[![Build Status](https://travis-ci.org/zatonovo/lambda.r.png)](https://travis-ci.org/zatonovo/lambda.r)

Description
===========
Provides a syntax for writing functional programs in R. Lambda.r has a clean
syntax for defining multi-part functions with optional guard statements.
Simple pattern matching is also supported. Types can be
easily defined and instantiated using the same functional notation. Type 
checking is integrated and optional, giving the programmer complete flexibility
over their application or package.

Basic Usage
===========
Defining a function
-------------------
Functions are defined using `%as%` notation. Any block of code can be in the 
function definition.

```R
fib(n) %as% { fib(n-1) + fib(n-2) }
```

Pattern matching
----------------
Multi-part function definitions are easily constructed. For simple criteria,
pattern matching of literals can be used directly in lambda.r.

```R
fib(0) %as% 1
fib(1) %as% 1
```

Strings can also be pattern matched within definitions.

Guard statements
----------------
Executing different function variants within a multi-part function sometimes
requires more detail than simple pattern matching. For these scenarios a guard
statement is used to define the condition for execution. Guards are simply an 
additional clause in the function definition.

```R
fib(n) %when% { n >= 0 } %as% { fib(n-1) + fib(n-2) }
```

A function variant only executes if the guard statements all evaluate to true.
As many guard statements as desired can be added in the block. Just separate
them with either a new line or a semi-colon.

```R
fib(n) %when% {
  is.numeric(n)
  n >= 0
} %as% { fib(n-1) + fib(n-2) }
```

Note that in the above example the type check can be handled using a type 
declaration, which is discussed below.

For functions defined in multiple parts, each separate function variant is
evaluated in the same order as they are defined. Hence a less restrictive
variant that evaluates to true defined early in the chain of function
definitions will take precedence over variants defined later. (If you are
following along in sequence, the pattern matches for `fib(0)` and `fib(1)` will
never be called since the first definition `fib(n)` will always evaulate to true).

Types
=====
Lambda.R introduces types as an alternative to classes. Types are data 
structures with type information attached to it. Like classes, constructors
exist for types and one type can inherit from another type. The difference is
that types do not have embedded methods. In functional programming, functions
are first class so there is no need to embed them within the data structure.
Using types provides type safety, which means that a function variant will only
execute if the types are correct. 

Defining a type
---------------
Types are defined by defining their constructor.  We define constructors using 
an uppercase function name.  The return value of the constructor is 
automatically typed.  Hence the value x will be of type Integer.

```R
Integer(x) %as% x
```

Instantiating the type is as simple as calling the function. Check the type
using the standard S3 introspection function `class`. The `%isa%` operator
can also be used to test whether an object is a particular type.

```R
x <- Integer(5)
x %isa% Integer
```

Type declarations
-----------------
Type constraints can be added to a function. These constraints specify the type
of each input argument in addition to the return type. Using this approach
ensures that the arguments can only have compatible types when the function is
called. The final type in the constraint is the return type, which is checked
after a function is called. If the result does not have the correct return type,
then the call will fail.

```R
fib(n) %::% Integer : Integer
fib(0) %as% Integer(1)
fib(1) %as% Integer(1)
fib(n) %as% { fib(n-1) + fib(n-2) }

fib(x)
```

The call `fib(1)` will fail because `1` is not of type `Integer`.

```R
> fib(1)
Error in UseFunction("fib", ...) : No valid function for 'fib(1)'
```

Properly typing the argument by calling `fib(Integer(1))` will give the
correct output. Note that pattern matching works even with the custom type.

```R
> fib(Integer(1))
[1] 1
attr(,"class")
[1] "Integer" "numeric"
```

Type constraints must be declared prior to the function implementation. Once
declared, the type declaration will retain scope
until another type declaration with the same number of parameters is declared
(see tests/types.R for an example).

Legacy types
------------
There are plenty of built-in types that are supported just like custom types 
defined in lambda.r. Use the same syntax for these types. In the example above
we can just as easily declare

```R
fib(n) %::% numeric : numeric
```

or even 

```R
fib(n) %::% Integer : numeric
```

NOTE: For objects of type function, due to the precedence rules of the parser,
you cannot specify 'function' in a type constraint. Instead use 'Function'.

```R
# Do this
make.gen(n) %::% numeric : Function

# Don't do this
make.gen(n) %::% numeric : function

```

Type Variables
--------------
Type constraints are useful, but too specific of a constraint can destroy the
polymorphism of a function. To preserve this while still retaining some
type safety you can use a type variable. With type variables the actual type
is not checked. Instead it is the relationship between types that are checked.

```R
fib(n) %::% a : a
fib(0) %as% 1
fib(1) %as% 1
fib(n) %as% { fib(n-1) + fib(n-2) }

```

In this type constraint, both the input and output types must match.

Note that the only characters valid for a type variable are the lowercase
letters (i.e. a-z). If you need more than this for a single function definition,
you've got other problems.

The Ellipsis Type
-----------------
The ellipsis can be inserted in a type constraint. This has interesting
properties as the ellipsis represents a set of arguments. To specify
that input values should be captured by the ellipsis, use ```...``` within
the type constraint. For example, suppose you want a function that
multiplies the sum of a set of numbers. The ellipsis type tells
lambda.r to bind the types associated with the ellipsis type.

```R
sumprod(x, ..., na.rm=TRUE) %::% numeric : ... : logical : numeric
sumprod(x, ..., na.rm=TRUE) %as% { x * sum(..., na.rm=na.rm) }

> sumprod(4, 1,2,3,4)
[1] 40
```

Alternatively, suppose you want all the values bound to the ellipsis
to be of a certain type. Then you can append ```...``` to a concrete
type.

```R
sumprod(x, ..., na.rm=TRUE) %::% numeric : numeric... : logical : numeric
sumprod(x, ..., na.rm=TRUE) %as% { x * sum(..., na.rm=na.rm) }

> sumprod(4, 1,2,3,4)
[1] 40
> sumprod(4, 1,2,3,4,'a')
Error in UseFunction(sumprod, "sumprod", ...) :
  No valid function for 'sumprod(4,1,2,3,4,a)'
```

If you want to preserve polymorphism but still constrain values bound
to the ellipsis to a single type, you can use a type variable. Note that
the same rules for type variables apply. Hence a type variable represents
a type that is not specified elsewhere.

```R
sumprod(x, ..., na.rm=TRUE) %::% a : a... : logical : a
sumprod(x, ..., na.rm=TRUE) %as% { x * sum(..., na.rm=na.rm) }

> sumprod(4, 1,2,3,4)
[1] 40
> sumprod(4, 1,2,3,4,'a')
Error in UseFunction(sumprod, "sumprod", ...) :
  No valid function for 'sumprod(4,1,2,3,4,a)'
```

The Don't-Care Type
-------------------
Sometimes it is useful to ignore a specific type in a constraint. Since
we are not inferring all types in a program, this is an acceptable
action. Using the ```.``` within a type constraint tells lambda.r to not
check the type for the given argument.

For example in ```R f(x, y) %::% . : numeric : numeric```, the type of 
```x``` will not be checked.


One Shot
========
Here is the complete example using built-in types:

```R
fib(n) %::% numeric : numeric
fib(0) %as% 1
fib(1) %as% 1
fib(n) %as% { fib(n-1) + fib(n-2) }

fib(5)
seal(fib)
```

To ignore types altogether, just omit the type declaration in the above listing
and the code will evaluate the same.

Here is the same example with custom types:

```R
Integer(x) %as% x
     
fib(n) %::% Integer : Integer
fib(0) %as% Integer(1)
fib(1) %as% Integer(1)
fib(n) %as% { fib(n-1) + fib(n-2) }

x <- Integer(5)
fib(x)
```

The `seal` command in the first example prevents new statements from being
added to an existing function definition. Instead new definitions reset the
function. Typically you don't need this function as lambda.r will auto-replace
function definitions that have the same signature.

Sugar Coating
=============
All the great features of R function calls are still supported in lambda.r. In 
addition, lambda.r provides some parse transforms to add some extra features
to make application development even faster.

Object Attributes
-----------------
Attributes are a form of meta data that decorate an object. This information
can be used to simplify type structures retaining polymorphism and compatibility
with existing functions while providing the detail needed for your application.
Lambda.R provides convenient syntax for interacting with attributes via the `@`
symbol.

```R
Temperature(x, system, units) %as%
{
  x@system <- system
  x@units <- units
  x
}
```

These attributes can then be accessed in guards and function bodies using the
same syntax.

```R
freezing(x) %::% Temperature : logical
freezing(x) %when% {
  x@system == 'metric'
  x@units == 'celsius'
} %as% {
  if (x < 0) { TRUE }
  else { FALSE }
}
```
 
Note that outside of lambda.r you must use the standard attr() function to 
access specific attributes. Also note that attributes have not been tested with
S4 objects.

Optional Arguments
------------------
A nice convenience in R is the ability to specify optional arguments with
default values. Lambda.R preserves this feature in multipart function 
definitions. Functions are matched based on the order in which they are 
defined, and this holds true with functions with optional arguments.

```R
Temperature(x, system="metric", units='celsius') %as%
{
  x@system <- system
  x@units <- units
  x
}

> ctemp <- Temperature(20)
> ctemp
[1] 20
attr(,"system")
[1] "metric"
attr(,"units")
[1] "celsius"
attr(,"class")
[1] "Temperature" "numeric"
```

The Ellipsis Argument
---------------------
Support for the ellipsis argument is built into lambda.r. Required arguments 
must still be matched, while any additional arguments will be enclosed in the 
ellipsis. Here's an example using the plant data included in R's lm help page.

```R
regress(formula, ..., na.action='na.fail') %as% {
  lm(formula, ..., na.action=na.action)
}

ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
data <- data.frame(group=gl(2,10,20,labels=c("Ctl","Trt")), weight=c(ctl, trt))
lm.D9 <- regress(weight ~ group, data=data)
```

Care does need to be used with the ellipsis as it behaves like a greedy match,
so subsequent definitions may not work as you intend when using the ellipsis
argument in a function variant.

Named Arguments
---------------
The examples above all hint at supporting named arguments. Named arguments can
be mixed and matched with positional arguments just as in legacy function
definitions.

```R
lm.D9 <- regress(data=data, weight ~ group)
```

Auto Replace and Sealing Definitions
-------------------
As of version 1.1.0, lambda.r can detect a duplicate function signature and
update an existing definition. This means development is more efficient 
since you can re-source files and the existing definitions will update as you
expect. This process is compatible with multi-part function definitions and
type constraints. Do note that when using type constraints, only functions
associated with the active type constraint can be auto replaced. The reason is
that there can be two identical function signatures and lambda.r really has
no way of knowing which one you mean. Hence, you have to tell lambda.r via
the type constraint.


For example take this simple reciprocal function. There are two type constraint
clauses, and three total function variants.  The signatures for variants 2 and 3 
are identical, so the only thing that distinguishes them are the different type 
constraints associated with each one.  Notice that there is an explicit bug in 
the definition of variant 2.

```R
reciprocal(n) %::% numeric : numeric
reciprocal(0) %as% stop("Reciprocal of 0 is undefined")
reciprocal(n) %as% { 2/n }

reciprocal(n) %::% character : numeric
reciprocal(n) %as% { reciprocal(as.numeric(n)) }
```

To change the definition of variant 2, you must re-declare
the first type constraint.  Otherwise, lambda.r would not know whether to update
variant 2 or 3.

```R
reciprocal(n) %::% numeric : numeric
reciprocal(n) %as% { 1/n }
```

Lambda.R has no way of knowing whether a function definition is complete or not.
Explicitly telling lambda.r will ensure that any new function definitions will
reset the function as opposed to append another definition.

```R
seal(freezing)
```

If providing a broad interface, be careful not to seal the function. Sealing is
analogous to making a variable final in Java, such that no further modifications
can be made. The key difference is that attempting to add further defintions to
the sealed function will overwrite the existing definition. This behavior is
intended to make application and package development more iterative.

Introspection
=============
A function in lambda.r has a lot of meta data attached to it. Accessing the raw
data can be overwhelming, so lambda.r provides facilities to extract the 
useful bits. Viewing basic information about a function is accomplished by just
typing out the function in the shell. This results in a dump of the type
declarations and function signatures for the function.

```R
> fib
<function>
[[1]]
fib(n) %::% Integer:Integer 
fib(0) %as% ...
[[2]]
fib(n) %::% Integer:Integer 
fib(1) %as% ...
[[3]]
fib(n) %::% Integer:Integer 
fib(n) %as% ...
```

Actual function bodies are not displayed to minimize clutter. 

Examining Functions
-------------------
To view a full function definition, use the 'describe' function to get the
definition of a specific function variant. The numbers separating each variant
is the index to use.

```R
> describe(fib,3)
function(n) { Integer ( fib ( n - 1 ) + fib ( n - 2 ) ) }
<environment: 0x10488ed10>
```

Examining Types
---------------
A type constructor is similar to a normal function, and the same technique works
to view a type body.

```R
> describe(Integer,1)
function(x) { x }
<environment: 0x10494aca8>
```


Monads
------

Maybe(a) %:=% a
Just(a) %:=% Maybe(a)
Nothing() %:=% Maybe(NA)

mreturn(x) %as% Just(x)


m %>>=% g %when% { is.null(m) } %as% NULL
m %>>=% g %::% Just : Function : Maybe
m %>>=% g %as% g(m)



m %>>=% g %::% Nothing : Function : Maybe
m %>>=% g %as% m

m %>>=% g %::% Just : Function : Maybe
m %>>=% g %as% g(m)


Composition
f %.% g %:=% function(...) f(g(...))

> unsafelogsqrt <- log %.% sqrt
> unsafelogsqrt(100)
 [1] 2.302585


Monadic composition
f %>=>% g %:=% { function(x) f(x) %>>=% g }
f %<=<% g %:=% { function(x) g(x) %>>=% f }

safelog(x) %::% numeric : Maybe
safelog(x) %when% { x <= 0 } %as% Nothing()
safelog(x) %:=% Just(log(x))

safesqrt(x) %::% numeric : Maybe
safesqrt(x) %when% { x <= 0 } %as% Nothing()
safesqrt(x) %:=% Just(sqrt(x))

safelogsqrt <- safelog %<=<% safesqrt




Debugging
---------
The standard debug function will not work with lambda.r functions. Instead, use
the included functions debug.lr and undebug.lr. These functions will allow you
to debug through a complete multipart function call.

Known Limitations
=================
If you try to break lambda.r, you will most likely succeed. There are things 
that won't work, but most use cases should work fine. Do let me know if you 
find something that fails, but don't break it just to break it. Below are
some things that won't work.

1. Complex mix and match of named and positional arguments

```R
lm.D9 <- regress(data=data, formula=weight ~ group, NULL)
```

Don't do this, please. It's bad style.

What's New
==========

Version 1.1.5
-------------
+ Support the ellipsis (...) in type constraints
+ Support a typed ellipsis (numeric...) in type constraints
+ Support ellipsis type variable (a...) in type constraints
+ Support debug.lr in functions defined in packages (currently locked)
+ Show which functions are being debugged
+ Handle default arguments that execute a function
+ Support more pattern matching (like empty lists)
+ Support guards of the form f(a,b,c) %when% { sum(a,b,c) == 1 } %as% { 1 }
+ Support guards of the form f(x) %when% { length(grep('foo',x)) > 0 } %as% { 1 }
+ Support proper import namespace for packages
+ Install from github using devtools: install_github('lambda.r','zatonovo')
+ Travis CI integration: https://travis-ci.org/zatonovo/lambda.r

Version 1.1.1
-------------
+ Support Function in every type position (only supported for return type)
+ Auto-replacing a function with 0 arguments fails
+ Fix type inheritance
+ Functions that return non-scalar values work as default values on arguments
+ Support pattern matching on NULL and NA
+ Support pattern matching on special symbol EMPTY

Version 1.1.0
-------------
+ Handle function types in type declarations
+ Support type variables
+ Auto-replace function definitions with a matching signature (no need for seal)
+ Handle 0 argument functions

Future
======
+ Support *apply and lambda expressions in guard statements
+ Support defining operators
+ Lock functions by default (check when next function is different name). Use
  public() as way to indicate function can be globally modified
+ Think about supporting namespaces
+ Support take, drop, cycle
+ Support partial function application
+ Check for side effects
+ Support tail recursion
+ Support type inference

