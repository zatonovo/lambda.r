\name{duck-typing}
\alias{\%isa\%}
\alias{\%hasa\%}
\alias{\%hasall\%}
\title{Functions for duck typing}
\description{Duck typing is a way to emulate type checking by virtue of an
object's characteristics as opposed to strong typing.}
\usage{
argument \%isa\% type
argument \%hasa\% property
argument \%hasall\% property

}
\arguments{
  \item{argument}{An object to inspect}
  \item{type}{A type name}
  \item{property}{A property of an object}
}
\details{
  These operators provide a convenient method for testing for specific
  properties of an object. 

  \code{\%isa\%} checks if an object is of the given type.

  \code{\%hasa\%} checks if an object has a given property. This can
  be any named element of a list or data.frame.
}
\value{
  Boolean value indicating whether the specific test is true or not.
}
\author{ Brian Lee Yung Rowe }

\seealso{
\code{\link{\%as\%}}
}
\examples{
5 \%isa\% numeric

Point(r,theta, 'polar') \%as\% {
  o <- list(r=r,theta=theta)
  o@system <- 'polar'
  o
}

p <- Point(5, pi/2, 'polar')
p %hasa% theta

}
\keyword{ methods }
\keyword{ programming }
