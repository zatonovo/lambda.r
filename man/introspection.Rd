\name{introspection}
\alias{describe}
\alias{debug.lr}
\alias{undebug.lr}
\alias{print.lambdar.fun}
\alias{print.lambdar.type}
\title{Introspection for lambda-r}
\description{These tools are used for debugging and provide a means of examining
the evaluation order of the function definitions as well as provide a lambda-r
compatible debugger.}
\usage{
describe(\dots)
debug.lr(x)
undebug.lr(x)
\method{print}{lambdar.fun}(x, \dots)
\method{print}{lambdar.type}(x, \dots)
}
\arguments{
  \item{x}{The function}
  \item{\dots}{Additional arguments}
}
\details{
  For a basic description of the function it is easiest to just type the 
  function name in the shell. This will call the print methods and print a
  clean output of the function definition. The definition is organized based
  on each function clause. If a type constraint exists, this precedes the 
  clause signature including guards. To reduce clutter, the actual body of the
  function clause is not printed. To view a clause body, each clause is 
  prefixed with an index number, which can be used in the \code{describe} function
  to get a full listing of the function.

  \code{describe(fn, idx)} 

  The 'debug.lr' and 'undebug.lr' functions are replacements for the built-in
  debug and undebug functions. They provide a mechanism to debug a complete
  function, which is compatible with the dispatching in lambda-r. The semantics
  is identical to the built-ins.
}
\value{
  The defined functions are invisibly returned.
}
\author{ Brian Lee Yung Rowe }

\examples{
# Type constraints are optional and include the return type
reciprocal(x) \%::\% numeric : numeric
reciprocal(x) \%when\% {
  # Guard statements can be added in succession
  x != 0
} \%as\% {
  # The function body follows
  1 / x
}

reciprocal(x) \%::\% character : numeric
reciprocal(x) \%as\% {
  reciprocal(as.numeric(x))
}

seal(reciprocal)
describe(reciprocal, 2)
}
\keyword{ methods }
\keyword{ programming }
