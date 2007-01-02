\name{matrixPaste}
\alias{matrixPaste}
\title{Element-Wise Paste of Conforming Matrices}
\description{
For a series of character matrices with the same dimensions, paste each element together in the order specified.
}
\usage{
matrixPaste(..., sep = rep(" ", length(list(...)) - 1))
}
\arguments{
  \item{\dots}{a set of \code{P} character matrices with the same dimensions}
  \item{sep}{a vector of \code{P-1} separators for each element}
}
\details{
The matrices are converted to vectors and pasted, then re-dimensioned. Different separators can be used between matrices, but the same separator must be used for all of the elements.
}
\value{
a character matrix
}
\author{Max Kuhn}

\examples{
mat1 <- matrix(letters[1:6], nrow = 2)
mat2 <- matrix(LETTERS[1:6], nrow = 2)
mat3 <- matrix(paste(1:6), nrow = 2)

matrixPaste(mat1, mat2)
matrixPaste(mat1, mat2, sep = "+")
matrixPaste(mat1, mat2, mat3, sep = c("+", "plus"))

}
\keyword{utilities}
