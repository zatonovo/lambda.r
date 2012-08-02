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
  \item{argument}{Argument}
  \item{type}{Type}
  \item{property}{Property}
}
\details{
  Duck typing
}
\value{
  Boolean values
}
\author{ Brian Lee Yung Rowe }

\seealso{
\code{\link{\%as\%}}
}
\examples{
5 \%isa\% numeric
}
\keyword{ methods }
\keyword{ programming }
