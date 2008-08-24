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

\method{odfTable}{numeric}(x, horizontal = length(x) < 5, colnames = names(x), 
   digits = max(3, getOption("digits") - 3), 
   name = paste("Table", floor(runif(1) * 1000), sep = ""), 
   styles = NULL, ...)
  
\method{odfTable}{character}(x, horizontal = length(x) < 5, colnames = names(x), 
   name = paste("Table", floor(runif(1) * 1000), sep = ""), 
   styles = NULL, ...)
   
  
\method{odfTable}{data.frame}(x, colnames = NULL, useRowNames = TRUE, 
   digits = max(3, getOption("digits") - 3), 
   name = paste("Table", floor(runif(1) * 1000), sep = ""), 
   styles = NULL, ...)
   
\method{odfTable}{matrix}(x, colnames = NULL, useRowNames = TRUE, 
   digits = max(3, getOption("digits") - 3),
    name = paste("Table", floor(runif(1) * 1000), sep = ""), 
    styles = NULL, ...)
}
\arguments{
  \item{x}{a vector, matrix or data frame}
  \item{horizontal}{a logical: should the vector be shown as a 1xn table or nx1? This is ignored for other data structures.}  
  \item{colnames}{a vector of column names that can be used. Note that if the row names are used in the table, this should contain an extra element for that column.}
  \item{useRowNames}{a logical: should the row names be printed in the final table}
  \item{digits}{number of significant digits passed to \code{format}}
  \item{name}{A name for the table. ODF requires a name for each object, so a random name will be used if unspecified.}
  \item{styles}{An optional list of style names for each table element (cells, headers etc). See \code{\link{tableStyles}}}
  \item{\dots}{optional arguments that can be passed to \code{format}}
}

\details{
The data structures are converted to character matrices using \code{format}. The \code{justify} and \code{trim} arguments to \code{format} are usually overridden by the table style options, so those arguments are automatically set to \code{justify = "none"} and \code{trim = TRUE}. However, if values of these arguments are passed using the three dots, format will use them (but they probably won't do anything).

When using \code{odfTable} in a code chunk, the chunk's \code{results}
argument should be set to \code{xml}.

The document formatting.odt in the package's examples directory
illustrates the process of changing the appearance of the table.
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

