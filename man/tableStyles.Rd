\name{tableStyles}
\alias{tableStyles}
\title{Generate Table Styles}
\description{
Based on the current style specifications, create style names for all table elements.
}
\usage{
tableStyles(x, header = NULL)
}
\arguments{
  \item{x}{a vector, matrix or data frame. See details below}
  \item{header}{an optional vector of heading names}
}
\details{
Based on the dimensions of \code{x}, this function generates table style names for all of the elements. For example, f the data are an nxn matrix, it will create an nxn matrix of style names for the text and the table cells. 

The value of \code{x} must match the results produced by the \code{\link{odfTable}} specifications. For example, if \code{useRowNames} is \code{TRUE}, \code{x} should have an extra column for the row names and an extra entry in the headings. Also, if your data are in a vector, you must convert it to a matrix consistent with the dimensions produced by the \code{horizontal} argument in \code{\link{odfTable}}.

Once the appropriate set of style names are generated, the user can programatically alter it. For example, based on some logic, cells can have different text colors etc.

}
\value{
a list of style names with elements:  \code{table},  \code{text},  \code{cell},  \code{header} and  \code{headerCell}. The  \code{text} and  \code{cell} entries are for the non-header table elements.

}
\author{Max Kuhn}

\seealso{\code{\link{odfTable}}}
\examples{
# this would require useRowNames = FALSE
irisStyles <- tableStyles(iris, header = names(iris))
irisStyles$text[2,3] <- "ttRed"
odfTable(iris, useRowNames = FALSE, styles = irisStyles)

}
\keyword{utilities}
