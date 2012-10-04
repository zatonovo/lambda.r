\name{lambda.r-package}
\alias{lambda.r-package}
\alias{lambda.r}
\docType{package}
\title{
Functional programming in R
}
\description{
Provides syntax for writing functional programs in R. Instead of using S3 or S4,
multi-part functions can be defined with pattern matching and explicit type
declarations. This package also provides a mechanism for defining and creating
types that is clean and intuitive.
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
R provides some functional programming facilities, but writing applications in R
there are few choices beyond S3, S4, and the newer Reference Classes. These are
all object-oriented approaches. Data analysis relies so much on computations
and data that a functional approach is really better suited for these types of
applications. Lambda-r introduces a syntax for writing applications using a
declarative notation that helps make programs more modular and easier to
maintain.
}
\author{
Brian Lee Yung Rowe

Maintainer: Brian Lee Yung Rowe <r@zatonovo.com>
}
\references{
Some background on using guards and pattern matching:
  %http://www.erlang.org/doc/reference_manual/functions.html#id2271076
  http://en.wikipedia.org/wiki/Guard_\%28computing\%29
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
