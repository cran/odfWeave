\name{announce}
\alias{announce}
\title{cat and flush console}
\description{
This function is a wrapper for \code{cat} with an immediate flush of the console}

\usage{
announce(verbose = TRUE, ...)
}
\arguments{
  \item{verbose}{a logical for printing}
  \item{\dots}{arguments passed to \code{cat}}
}

\value{
\code{NULL}, invisibly
}
\author{Nathan Coulter}

\examples{
announce()
announce(FALSE, "this will not be printed")
announce(TRUE, "but this will be")

}
\keyword{utilities}
