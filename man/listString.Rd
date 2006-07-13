\name{listString}
\alias{listString}
\title{Convert a vector to a textual list}
\description{
Takes a vector and produces output like "element1 and element2" or "element1, element2 and element3"
}
\usage{
listString(x, period = FALSE, verbose = TRUE)
}
\arguments{
  \item{x}{a vector (converted to character if not)}
  \item{period}{a logical: should a period end the text?}
  \item{verbose}{a logical: how much details are logged}
}

\value{
a character string
}
\author{Max Kuhn}

\examples{
listString(letters[1])
listString(letters[1:2])
listString(letters[1:4])
}
\keyword{utilities}
