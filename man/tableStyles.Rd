\name{tableStyles}
\alias{tableStyles}
\title{Generate Table Styles}
\description{
Based on the current style specifications, create style names for all table elements.
}
\usage{
tableStyles(x, useRowNames = TRUE, header = NULL)
}
\arguments{
  \item{x}{a vector, matrix or data frame. See details below}
  \item{useRowNames}{a logical: should the row names be printed in the final table}  
  \item{header}{an optional vector of heading names}
}
\details{
Based on the dimensions of \code{x}, this function generates table style names for all of the elements. For example, f the data are an nxn matrix, it will create an nxn matrix of style names for the text and the table cells. 

The arguments of \code{tableStyles} must be consistent with those specified for \code{\link{odfTable}}, specifically the \code{useRowNames} and \code{header} arguments.  

Once the appropriate set of style names are generated, the user can programatically alter it. For example, based on some logic, cells can have different text colors etc.

}
\value{
a list of style names with elements:  \code{table},  \code{text},  \code{cell},  \code{header} and  \code{headerCell}. The  \code{text} and  \code{cell} entries are for the non-header table elements.

}
\author{Max Kuhn}

\seealso{\code{\link{odfTable}}}
\examples{
irisStyles <- tableStyles(iris, useRowNames = TRUE, header = names(iris))
irisStyles$text[2,3] <- "ttRed"
odfTable(iris, useRowNames = TRUE, styles = irisStyles)

}
\keyword{utilities}
