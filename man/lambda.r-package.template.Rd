\name{lambda.r-package}
\alias{lambda.r-package}
\alias{lambda.r}
\docType{package}
\title{
Functional programming in R
}
\description{
Lambda.R is a language extension that supports a functional programming style in
R. As an alternative to the object-oriented systems, lambda.r offers a 
functional syntax for defining types and functions. Functions can be defined in
multiple parts (aka multi-part), use pattern matching, and guards while still
supporting all the standard features of R. Type constructors are intuitive and
type constraints can optionally be added to function definitions. Attributes
can be accessed using a convenient syntax to reduce type clutter.
}
\details{
\tabular{ll}{
Package: \tab lambda.r\cr
Type: \tab Package\cr
Version: \tab {version}\cr
Date: \tab {date}\cr
License: \tab LGPL-3\cr
LazyLoad: \tab yes\cr
}
R leverages many functional programming concepts, but application development in
R is primarily an object-oriented affair. In fact there are few choices beyond
S3, S4, and the newer Reference Classes, which are all object-oriented 
approaches.
Data analysis relies so much on mathematical operations, transformations,
and computations that a functional approach is better suited for these
types of applications. The reason is that object models rarely make sense in
data analysis since so many transformations are applied to data sets. Trying to
define classes and attach methods to them results in a futile enterprise rife
with arbitrary choices and hierarchies. Functional programming avoids this
unnecessary quandry by making objects and functions first class and preserving
them as two distinct
entities. Lambda.R introduces a syntax for writing applications using a
declarative notation that helps make programs more modular and easier to
maintain.

Functions

Types
}
\author{
Brian Lee Yung Rowe

Maintainer: Brian Lee Yung Rowe <r@zatonovo.com>
}
\references{
Some background on guards and pattern matching:
  http://en.wikipedia.org/wiki/Guard_\%28computing\%29
  http://www.erlang.org/doc/reference_manual/functions.html#id2271076
  http://www.haskell.org/tutorial/patterns.html
}
\note{
This package replaces futile.paradigm.
}
\keyword{ package }
\seealso{
\code{\link{describe}}, \code{\link{debug.lr}}, \code{\link{UseFunction}}, \code{\link{\%as\%}}
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
seal(fib)

fib(5)


## Using custom types
Integer(x) \%when\% { is.wholenumber(x) } \%as\% x

fib.a(n) \%::\% Integer : Integer
fib.a(0) \%as\% Integer(1)
fib.a(1) \%as\% Integer(1)
fib.a(n) \%as\% { Integer(fib.a(n-1) + fib.a(n-2)) }
seal(fib.a)

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
