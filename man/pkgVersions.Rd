\name{pkgVersions}
\alias{pkgVersions}
\title{List packages currently used}
\description{
Creates a string, matrix or data frame that shows the packages and their versions currently used (i.e in the search path).
}
\usage{
pkgVersions(type = "string", ncol = 4) 
}
\arguments{
  \item{type}{a string  designating the type of output object: "string", "matrix" or "data frame"}
  \item{ncol}{integer specification for the number of columns in the output when \code{type = "matrix"} }
}

\value{
a string, character matrix or a data frame with columns "Package" and "Version"
}
\author{Max Kuhn, original version by Jim Rogers}
\examples{
pkgVersions()
pkgVersions("matrix")
pkgVersions("data frame")
}
\keyword{utilities}
