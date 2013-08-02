\name{lambda.r-package}
\alias{lambda.r-package}
\alias{lambda.r}
\docType{package}
\title{
Modeling Data with Functional Programming
}
\description{
Lambda.r is a language extension that supports a functional programming 
style in R. As an alternative to the object-oriented systems,
lambda.r offers a functional syntax for defining types and functions.
Functions can be defined with multiple distinct function clauses
similar to how multipart mathematical functions are defined.
There is also support for pattern matching and guard expressions to 
finely control function dispatching, all the while still
supporting standard features of R. Lambda.r also introduces its own
type system with intuitive type constructors are and
type constraints that can optionally be added to function definitions.
Attributes are also given the attention they deserve with a clean
and convenient syntax that reduces type clutter.
}
\details{
\tabular{ll}{
Package: \tab lambda.r\cr
Type: \tab Package\cr
Version: \tab 1.1.3\cr
Date: \tab 2013-08-02\cr
License: \tab LGPL-3\cr
LazyLoad: \tab yes\cr
}
Data analysis relies so much on mathematical operations, transformations,
and computations that a functional approach is better suited for these
types of applications. The reason is that object models rarely make sense in
data analysis since so many transformations are applied to data sets. Trying to
define classes and attach methods to them results in a futile enterprise rife
with arbitrary choices and hierarchies. Functional programming avoids this
unnecessary quandry by making objects and functions first class and preserving
them as two distinct entities. 

R provides many functional programming concepts mostly inherited from 
Scheme. Concepts like first class functions and lazy evaluation are
key components to a functional language, yet R lacks some of the more
advanced features of modern functional programming languages.
Lambda.r introduces a syntax for writing applications using a
declarative notation that facilitates reasoning about your program
in addition to making programs modular and easier to maintain.


\subsection{Function Definition}{
Functions are defined using the \code{\%as\%} symbol in place of \code{<-}.
Simple functions can be defined as simply as
\preformatted{f(x) \%as\% x }
and can be called like any other function.
\preformatted{f(1) }

Functions that have a more complicated body require braces.
\preformatted{f(x) \%as\% { 2 * x }

g(x, y) \%as\%
{
  z <- x + y
  sqrt(z)
}
}

\subsection{Multipart functions and guards}{
Many functions are defined in multiple parts. For example absolute value
is typically defined in two parts: one covering negative numbers and one
covering everything else. Using guard expressions and the \code{\%when\%}
keyword, these parts can be easily captured.
\preformatted{abs(x) \%when\% { x < 0 } \%as\% -x
abs(x) \%as\% x
}

Any number of guard expressions can be in a guard block, such that all
guard expressions must evaluate to true.
\preformatted{abs(x) \%when\% {
  is.numeric(x)
  length(x) == 1
  x < 0
} \%as\% -x

abs(x) \%when\% {
  is.numeric(x)
  length(x) == 1
} \%as\% x
}

If a guard is not satisfied, then the next clause is tried. If no
function clauses are satisfied, then an error is thrown.
}

\subsection{Pattern matching}{
Simple scalar values can be specified in a function definition in 
place of a variable name. These scalar values become patterns that
must be matched exactly in order for the function clause to execute.
This syntactic technique is known as pattern matching.

Recursive functions can be defined simply using pattern matching.
For example the famed Fibonacci sequence can be defined recursively.
\preformatted{fib(0) \%as\% 1
fib(1) \%as\% 1
fib(n) \%as\% { fib(n-1) + fib(n-2) }
}
This is also useful for conditionally executing a function.
The reason you would do this is that it becomes easy to symbolically
transform the code, making it easier to reason about.
\preformatted{pad(x, length, TRUE) \%as\% c(rep(NA,length), x)
pad(x, length, FALSE) \%as\% x
}

It is also possible to match on \code{NULL} and \code{NA}.
\preformatted{sizeof(NULL) \%as\% 0
sizeof(x) \%as\% length(x)
}
}

}

\subsection{Types}{
A type is a custom data structure with meaning. Formally a type is
defined by its type constructor, which codifies how to create objects
of the given type. The lambda.r type system is fully compatible with
the built-in S3 system. Types in lambda.r must start with a
capital letter.

\subsection{Type constructors}{
A type constructor is responsible for creating objects of a given type.
This is simply a function that has the name of the type. So to
create a type \code{Point} create its type constructor.
\preformatted{Point(x,y) \%as\% list(x=x,y=y) }
Note that any built-in data structure can be used as a base type. 
Lambda.r simply extends the base type with additional type information.

Types are then created by calling their type constructor.
\preformatted{p <- Point(3,4)}

To check whether an object is of a given type, use the \code{\%isa\%}
operator. \preformatted{p \%isa\% Point}
}

\subsection{Type constraints}{
Once a type is defined, it can be used to limit execution of a
function. R is a dynamically typed language, but with type constraints
it is possible to add static typing to certain functions. S4 does
the same thing, albeit in a more complicated manner.

Suppose we want to define a distance function for \code{Point}.
Since it is only meaningful for \code{Point}s we do not want to 
execute it for other types. This is achieved by using a type constraint,
which declares the function argument types as well as the
type of the return value. Type constraints are defined by declaring the
function signature followed by type arguments. \preformatted{distance(a,b) \%::\% Point : Point : numeric
distance(a,b) \%as\% { sqrt((b$x - a$x)^2 + (b$y - a$y)^2) }}
With this type constraint \code{distance} will only be called if both arguments
are of type \code{Point}. After the function is applied, a further
requirement is that the return value must be of type \code{numeric}.
Otherwise lambda.r will throw an error.
Note that it is perfectly legal to mix and match lambda.r types with
S3 types in type constraints.

}

\subsection{Type variables}{
Declaring types explicitly gives a lot of control, but it also
limits the natural polymorphic properties of R functions. 
Sometimes all that is needed is to define the relationship
between arguments. These relationships can be captured by
a type variable, which is simply any single lower case letter 
in a type constraint.

In the distance example, suppose we do not want to restrict the 
function to just \code{Point}s, but whatever type is used must
be consistent for both arguments. In this case a type variable is
sufficient. \preformatted{distance(a,b) \%::\% z : z : numeric
distance(a,b) \%as\% { sqrt((b$x - a$x)^2 + (b$y - a$y)^2) }}
The letter \code{z} was used to avoid confusion with the names of
the arguments, although it would have been just as valid to use
\code{a}.

Type constraints and type variables can be applied to any lambda.r
function, including type constructors.
}

}

\subsection{Attributes}{
The attribute system in R is a vital, yet often overlooked feature.
This orthogonal data structure is essentially a list attached to 
any object. The benefit of using attributes is that it reduces
the need for types since it is often simpler to reuse existing
data structures rather than create new types.

Suppose there are two kinds of \code{Point}s: those defined as
Cartesian coordinates and those as Polar coordinates. Rather than
create a type hierarchy, you can attach an attribute to the object.
This keeps the data clean and separate from meta-data that only
exists to describe the data.
\preformatted{Point(r,theta, 'polar') \%as\% {
  o <- list(r=r,theta=theta)
  o@system <- 'polar'
  o
}

Point(x,y, 'cartesian') \%as\% {
  o <- list(x=x,y=y)
  o@system <- 'cartesian'
  o
}
}

Then the \code{distance} function can be defined according to the
coordinate system.
\preformatted{distance(a,b) \%::\% z : z : numeric
distance(a,b) \%when\% {
  a@system == 'cartesian'
  b@system == 'cartesian'
} \%as\% {
  sqrt((b$x - a$x)^2 + (b$y - a$y)^2)
}

distance(a,b) \%when\% {
  a@system == 'polar'
  b@system == 'polar'
} \%as\% {
  sqrt(a$r^2 + b$r^2 - 2 * a$r * b$r * cos(a$theta - b$theta))
}
}
Note that the type constraint applies to both function clauses.

}

\subsection{Debugging}{
As much as we would like, our code is not perfect. To help
troubleshoot any problems that exist, lambda.r provides hooks into
the standard debugging system. Use \code{debug.lr} as a drop-in 
replacement for \code{debug} and \code{undebug.lr} for \code{undebug}.
In addition to being aware of multipart functions, lambda.r's 
debugging system keeps track of what is being debugged, so you can 
quickly determine which functions are being debugged. To see 
which functions are currently marked for debugging, call
\code{which.debug}. Note that if you use \code{debug.lr} for
all debugging then lambda.r will keep track of all debugging in 
your R session. Here is a short example demonstrating this.
\preformatted{> f(x) \%as\% x
> debug.lr(f)
> debug.lr(mean)
>
> which.debug()
[1] "f"    "mean"
}
}

}
\author{
Brian Lee Yung Rowe

Maintainer: Brian Lee Yung Rowe <r@zatonovo.com>
}
\references{
Blog posts on lambda.r: http://cartesianfaith.com/category/r/lambda-r/

Project site: https://github.com/muxspace/lambda.r
}
\keyword{ package }
\keyword{ programming }
\seealso{
\code{\link{\%as\%}}, \code{\link{describe}}, \code{\link{debug.lr}},
\code{\link{\%isa\%}}
}
\examples{
is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

## Use built in types for type checking
fib(n) \%::\% numeric : numeric
fib(0) \%as\% 1
fib(1) \%as\% 1
fib(n) \%when\% {
  is.wholenumber(n)
} \%as\% {
  fib(n-1) + fib(n-2)
}

fib(5)


## Using custom types
Integer(x) \%when\% { is.wholenumber(x) } \%as\% x

fib.a(n) \%::\% Integer : Integer
fib.a(0) \%as\% Integer(1)
fib.a(1) \%as\% Integer(1)
fib.a(n) \%as\% { Integer(fib.a(n-1) + fib.a(n-2)) }

fib.a(Integer(5))


## Newton-Raphson optimization
converged <- function(x1, x0, tolerance=1e-6) abs(x1 - x0) < tolerance
minimize <- function(x0, algo, max.steps=100)
{
  step <- 0
  old.x <- x0
  while (step < max.steps)
  {
    new.x <- iterate(old.x, algo)
    if (converged(new.x, old.x)) break
    old.x <- new.x
  }
  new.x
}

iterate(x, algo) \%::\% numeric : NewtonRaphson : numeric
iterate(x, algo) \%as\% { x - algo$f1(x) / algo$f2(x) }

iterate(x, algo) \%::\% numeric : GradientDescent : numeric
iterate(x, algo) \%as\% { x - algo$step * algo$f1(x) }

NewtonRaphson(f1, f2) \%as\% list(f1=f1, f2=f2)
GradientDescent(f1, step=0.01) \%as\% list(f1=f1, step=step)


fx <- function(x) x^2 - 4
f1 <- function(x) 2*x
f2 <- function(x) 2

algo <- NewtonRaphson(f1,f2)
minimize(3, algo)

algo <- GradientDescent(f1, step=0.1)
minimize(3, algo)
}
