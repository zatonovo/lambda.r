Description
===========
Provides a syntax for writing functional programs in R. Lambda-R has a clean
syntax for defining multi-part functions with optional guards. Simple pattern
matching is also supported. Types can be
easily defined and instantiated using the same functional notation. Type 
checking is integrated and optional, giving the programmer complete flexibility
over their system.

Examples
========
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
      is.integer(n)
      n >= 0
    } %as% { fib(n-1) + fib(n-2) }

Pattern Matching
----------------
Simple pattern matching of literals is supported in lambda-r.

    fib(0) %as% 1
    fib(1) %as% 1

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
return type.

    fib(n) %::% Integer : Integer

One Shot
========
Here's the complete example without types

    fib(0) %as% 1
    fib(1) %as% 1
    fib(n) %as% { fib(n-1) + fib(n-2) }

    fib(5)

and with types

    Integer(x) %as% x
     
    fib(0) %as% 1
    fib(1) %as% 1
    fib(n) %::% Integer : Integer
    fib(n) %as% { fib(n-1) + fib(n-2) }

    x <- Integer(5)
    fib(x)

Future
======
+ Tests
+ Literals need to go before variables in definitions
+ Better warning messages
+ Handle arbitrary locations for type declarations
+ Allow arbitrary placement of type declarations
+ Add warning if no function definition exists for a type declaration
+ Check for side effects
+ Introspection
+ Support ellipsis argument
+ Add assertions (particularly with an assertion for the last type argument)
+ Prevent type constructors from having type declarations
+ Support type inference
+ Support tail recursion
+ Add warning for same number of arguments with different arg names
