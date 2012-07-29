Description
===========
Provides a syntax for writing functional programs in R. Lambda-R has a clean
syntax for defining multi-part functions with optional guards. Simple pattern
matching is also supported. Types can be
easily defined and instantiated using the same functional notation. Type 
checking is integrated and optional, giving the programmer complete flexibility
over their system.

Basic Usage
===========
Defining a function
-------------------
Functions are defined using the %as% notation. Any block can be in the 
function definition

    fib(n) %as% { fib(n-1) + fib(n-2) }

Adding guards
-------------
Guards are simply an additional clause in the function.

    fib(n) %when% { n >= 0 } %as% { fib(n-1) + fib(n-2) }

As many guard statements can be added in the block as desired. Just separate
them with either a new line or a semi colon.

    fib(n) %when% {
      is.numeric(n)
      n >= 0
    } %as% { fib(n-1) + fib(n-2) }

Pattern Matching
----------------
Simple pattern matching of literals is supported in lambda-r.

    fib(0) %as% 1
    fib(1) %as% 1


Types
=====
Lambda R introduces types as an alternative to classes. Types are data 
structures with type information attached to it. This allows you to write
programs that are strongly typed.

Defining a type
---------------
Define types by defining their constructor.

    Integer(x) %as% x

Instantiating the type is as simple as calling the function.

    x <- Integer(5)

Type declarations
-----------------
Type constraints can be added to a function. This ensures that the arguments
have compatible types when the function is called. The final type is the
return type. The return type is checked after a function is called. If the 
result does not have the correct return type, then the call will fail.

    fib(n) %::% Integer : Integer

Note that for a type to have effect on a definition, it must be declared prior
to the function implementation. A single type declaration will retain scope
until another type declaration with the same number of parameters is declared
(see tests/types.R for an example).

One Shot
========
Here's the complete example without types

    fib(0) %as% 1
    fib(1) %as% 1
    fib(n) %as% { fib(n-1) + fib(n-2) }

    fib(5)
    seal(fib)

and with types

    Integer(x) %as% x
     
    fib(n) %::% Integer : Integer
    fib(0) %as% Integer(1)
    fib(1) %as% Integer(1)
    fib(n) %as% { Integer(fib(n-1) + fib(n-2)) }

    x <- Integer(5)
    fib(x)

Advanced Features
=================

Object Attributes
-----------------
Lambda R provides convenient syntax for interacting with attributes. This
approach allows you to take advantage of existing functions for R data 
structures since meta data is orthogonal to real data.

    Temperature(x, system, units) %as%
    {
      x@system <- system
      x@units <- units
      x
    }

These attributes can then be accessed in guards and function bodies using the
same syntax.

    freezing(x) %::% Temperature : logical
    freezing(x) %when% {
      x@system == 'metric'
      x@units == 'celsius'
    } %as% {
      if (x < 0) { TRUE }
      else { FALSE }
    }
 

Note that outside of Lambda R you must use the standard attr() function to 
access specific attributes.

Optional Arguments
------------------
A nice convenience in R is the ability to specify optional arguments with
default values. Lambda R preserves this feature in multipart function 
definitions. Functions are matched based on the order in which they are 
defined, and this holds true with functions with optional arguments.

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

Sealing Definitions
-------------------
Lambda R has no way of knowing whether a function definition is complete or not.
Explicitly telling lambda r will ensure that any new function definitions will
reset the function as opposed to append another definition.

    seal(freezing)

Introspection
=============
A function in lambda r is much more complicated than a simple R function. Basic
information about a function is accomplished by just typing out the function.
This results in a dump of the type declarations and function signatures for the
function.

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

Actual function bodies are not displayed to minimize clutter. 

Examining Functions
-------------------
To view a full function definition, use the 'describe' function to get the
definition of a specific function variant. The numbers separating each variant
is the index to use.

    > describe(fib,3)
    function(n) { Integer ( fib ( n - 1 ) + fib ( n - 2 ) ) }
    <environment: 0x10488ed10>

Examining Types
---------------
A type constructor is similar to a normal function, and the same technique works
to view a type body.

    > describe(Integer,1)
    function(x) { x }
    <environment: 0x10494aca8>

Debugging
---------
The standard debug function will not work with lambda r functions. Instead, use
the included functions debug.lr and undebug.lr. These functions will allow you
to debug through a complete multipart function call.


Future
======
+ Support ellipsis argument
+ Add warning for same number of arguments with different arg names
+ Prevent type constructors from having type declarations
+ Check for side effects
+ Support tail recursion
+ Support type inference
