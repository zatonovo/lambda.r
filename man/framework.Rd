\name{\%as\%}
\alias{\%as\%}
\alias{\%::\%}
\alias{seal}
\title{The lambda r way of defining functions and type constructors}
\description{This function is used in place of the assignment operator for
defining functions and type constructors with lambda r.}
\usage{
signature \%as\% body
signature \%::\% types
seal(fn)
}
\arguments{
  \item{signature}{The function signature for the function to be defined}
  \item{body}{The body of the function}
  \item{types}{The type constraints for the function}
  \item{fn}{The function to seal}
}
\details{
  These two operators are the primary touch points with lambda r. 

  'seal' finalizes a function definition. Any new statements found will reset
  the definition, effectively deleting it. This is useful to prevent other
  people from accidentally modifying your function definition.
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

print(reciprocal)
reciprocal(4)
reciprocal("4")

}
\keyword{ methods }
\keyword{ programming }
