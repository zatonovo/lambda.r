\name{\%as\%}
\alias{\%as\%}
\alias{\%:=\%}
\alias{\%::\%}
\alias{EMPTY}
\alias{seal}
\title{Define functions and type constructors in lambda.r}
\description{The \%as\% function is used in place of the 
assignment operator for defining functions and type constructors 
with lambda.r. The \%as\% operator is the gateway to a
full suite of advanced functional programming features.}
\usage{
signature \%::\% types
signature \%as\% body
seal(fn)
}
\arguments{
  \item{signature}{The function signature for the function to be defined}
  \item{types}{The type constraints for the function}
  \item{body}{The body of the function}
  \item{fn}{The function to seal}
}
\details{
  The \%as\% and \%::\% operators are the primary touch points with lambda.r.

  Functions are defined using \%as\% notation. Any block of code can be in the 
  function definition. For simple criteria, pattern matching of literals can 
  be used directly in lambda.r. Executing different function clauses within a
  multipart function sometimes requires more detail than simple pattern 
  matching. For these scenarios a guard statement is used to define the 
  condition for execution. Guards are simply an additional clause in the 
  function definition defined by the \%when\% operator.

  \code{ fib(n) \%when\% { n >= 0 } \%as\% { fib(n-1) + fib(n-2) } }

  A function variant only executes if the guard statements all evaluate to true.
  As many guard statements as desired can be added in the block. Just separate
  them with either a new line or a semi-colon.

  Type constructors are no different from regular functions with one exception:
  the function name must start with a capital letter. In lambda.r, types are
  defined in PascalCase and functions are lower case. Violating this rule will
  result in undefined behavior. The return value of the type constructor is the
  object that represents the type. It will have the type attached to the object.

  \code{ Number(x, set='real') \%as\% {
    x@set <- set
    x
  }}

  Attributes can be accessed using lambda.r's at-notation, which borrows from
  S4's member notation. These attributes are standard R attributes and should
  not be confused with object properties. Hence with lambda.r it is possible to
  use both the $ to access named elements of lists and data.frames while using
  the @ symbol to access the object's attributes.

  Type constraints specify the type of each input argument in addition to the
  return type. Using this approach ensures that the arguments can only have
  compatible types when the function is called. The final type in the
  constraint is the return type, which is checked after a function is called.
  If the result does not have the correct return type, then the call will fail.
  Each type is separated by a colon and their order is defined by the order of
  the function clause signature. 
  
  Each function clause can have its own type constraint. Once a constraint is
  defined, it will continue to be valid until another type constraint is
  defined.

  'seal' finalizes a function definition. Any new statements found will reset
  the definition, effectively deleting it. This is useful to prevent other
  people from accidentally modifying your function definition.
}
\value{
  The defined functions are invisibly returned.
}
\author{ Brian Lee Yung Rowe }

\examples{
# Type constraints are optional and include the return type as the 
# final type
reciprocal(x) \%::\% numeric : numeric
reciprocal(0) \%as\% stop("Division by 0 not allowed")

# The type constraint is still valid for this function clause
reciprocal(x) \%when\% {
  # Guard statements can be added in succession
  x != 0
  # Attributes can be accessed using '@' notation
  is.null(x@dummy.attribute)
} \%as\% {
  # This is the body of the function clause
  1 / x
}

# This new type constraint applies from this point on
reciprocal(x) \%::\% character : numeric
reciprocal(x) \%as\% {
  reciprocal(as.numeric(x))
}

# Seal the function so no new definitions are allowed
seal(reciprocal)

print(reciprocal)
reciprocal(4)
reciprocal("4")

}
\keyword{ methods }
\keyword{ programming }
