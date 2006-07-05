\name{odfTable}
\alias{odfTable}
\alias{odfTable.data.frame}
\alias{odfTable.matrix}
\alias{odfTable.character}
\alias{odfTable.factor}
\alias{odfTable.numeric}

\title{Create an Open Document Format table}
\description{
Create an Open Document Format table from a data frame, matrix or vector
}
\usage{
odfTable(x, ...)

odfTable.numeric(x, horizontal = length(x) < 5, colnames = names(x), 
   digits = max(3, getOption("digits") - 3), 
   name = paste("Table", floor(runif(1) * 1000), sep = ""), 
   styles = NULL, ...)
   
odfTable.character(x, horizontal = length(x) < 5, colnames = names(x), 
   name = paste("Table", floor(runif(1) * 1000), sep = ""), 
   styles = NULL, ...)
   
odfTable.character(x, horizontal = length(x) < 5, colnames = names(x), 
   name = paste("Table", floor(runif(1) * 1000), sep = ""), 
   styles = NULL, ...)
   
odfTable.data.frame(x, colnames = NULL, useRowNames = TRUE, 
   digits = max(3, getOption("digits") - 3), 
   name = paste("Table", floor(runif(1) * 1000), sep = ""), 
   styles = NULL, ...)
   
odfTable.matrix(x, colnames = NULL, useRowNames = TRUE, 
   digits = max(3, getOption("digits") - 3),
    name = paste("Table", floor(runif(1) * 1000), sep = ""), 
    styles = NULL, ...)
}
\arguments{
  \item{x}{a vector, matrix or data frame}
  \item{horizontal}{a logical: should the vector be shown as a 1xn table or nx1? This is ignored for other data structures.}  
  \item{colnames}{a vector of column names that can be used. Note that if the row names are used in the table, this should contain an extra element for that column.}
  \item{useRowNames}{a logical: should the row names be printed in the final table}
  \item{digits}{number of significant digits passed to format}
  \item{name}{A name for the table. ODF requires a name for each object, so a random name will be used if unspecified.}
  \item{styles}{An optional list of style names for each table element (cells, headers etc). See \code{\link{tableStyles}}}
  \item{\dots}{optional arguments that can be passed to \code{format}}
}
\details{
There are two main components to specifying output formats: style definitions and style assignments. The definition has the specific components (such as a table cell) and their format values (e.g. boxed with solid black lines). The function \code{\link{getStyleDefs}} can fetch the pre-existing styles in the package. These can be modified and new definitions can be added. The function \code{\link{setStyleDefs}} ``registers'' the style changes with the package. When \code{\link{odfWeave}} is called, these definitions are written to the style sections of the XML files. There is a second mechanism to assign  styles to specific output elements. The functions\code{\link{getStyles}} and \code{\link{setStyles}} can be used to tell odfWeave which style definition to use for a particular output

The data structures are converted to character matrices using format.
}
\value{
a list of character string that contain XML markup
}
\author{Max Kuhn}

\seealso{\code{\link{tableStyles}}}
\examples{
odfTable(iris[1:5,])
}
\keyword{utilities}

