\name{odfCat}
\alias{odfCat}
\title{Concatenate and Print in Native ODF}
\description{
Outputs the objects, concatenating the representations, and sandwiches the output between XML tags.
}
\usage{
odfCat(..., sep = " ", trim = FALSE, 
   digits = max(3, getOption("digits") - 3), nsmall = 0,
   width = NULL, na.encode = TRUE, scientific = NA)
}
\arguments{
  \item{...}{R objects (see Details for the types of objects allowed).}
  \item{sep}{character string to insert between the objects to print.}
  \item{trim}{used to convert numeric data to character using \code{\link{format}} }
  \item{digits}{used to convert numeric data to character using \code{\link{format}} }
  \item{nsmall}{used to convert numeric data to character using \code{\link{format}} }
  \item{width}{used to convert numeric data to character using \code{\link{format}} }
  \item{na.encode}{used to convert numeric data to character using \code{\link{format}} }x
  \item{scientific}{used to convert numeric data to character using \code{\link{format}} }
}

\details{

\code{odfCat} is an analog to \code{\link{cat}} and is useful for
producing output in user-defined functions. It converts its arguments to
character strings, concatenates them, separating them by the given
'sep=' string. It then sandwiches the text between \code{<text:p>} tags,
and then outputs them. Note that this will produce paragraphs and cannot
sequentially produce sentences within a paragraph.
     
\code{\link{cat}} uses an internal function, so there will be some differences between \code{\link{cat}} and \code{odfCat}. For example, factors are naively converted to character and numeric data are converted to character.

Since the text is embedded in ODF tags escaped characters
(e.g. \code{\\n}) don't have any effect. Exceptions are \code{\\"} and
  \code{\\'}.

The paragraph uses the current paragraph style. The document formatting.odt in the package's examples directory
illustrates the process of changing the appearance of the paragraph.
}
\value{
a character strng of class \code{odfCat}}

\author{Max Kuhn}

\seealso{\code{\link{cat}}, \code{\link{format}} }
\examples{
odfCat("\"hello world\"")
odfCat("these are the first letters", letters[1:5])
odfCat("some random normal data:", rnorm(5))

}
\keyword{utilities }
