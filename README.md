Description
===========
Provides a syntax for writing functional programs in R. Lambda.R has a clean
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
Types are defined by defining their constructor. The return value of the
constructor is automatically typed. Hence the value x will be of type Integer.

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
function.

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

Sealing Definitions
-------------------
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

Debugging
---------
The standard debug function will not work with lambda.r functions. Instead, use
the included functions debug.lr and undebug.lr. These functions will allow you
to debug through a complete multipart function call.

Known Limitations
=================
If you try to break lambda r, you will most likely succeed. There are things 
that won't work, but most use cases should work fine. Do let me know if you 
find something that fails, but don't break it just to break it. Below are
some things that won't work.

1. Complex mix and match of named and positional arguments

```R
lm.D9 <- regress(data=data, formula=weight ~ group, NULL)
```

Don't do this, please. It's bad style.

R3.0.0 Support
==============
+ DESCRIPTION: Remove Depends: parser
+ R/framework.R: Replace attr(parser(text=...), 'data') -> getParseData(parse(text=...))
+ R/framework.R: Replace line$token.desc -> line.token
+ R/framework.R: iterator() - Remove empty expression blocks
  tree <- tree[! (tree$token=='expr' & tree$text==''),]

Future
======
+ Support debugging functions in packages (e.g. debug.lr(futile.any::anynames))
+ Support more pattern matching (like empty lists)
+ Check for side effects
+ Support tail recursion
+ Support type inference

