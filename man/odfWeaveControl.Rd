\name{odfWeaveControl}
\alias{odfWeaveControl}

\title{Control odfWeave options}
\description{
Allows the user to specify how odfWeave operates and style information for the document.
}
\usage{
odfWeaveControl(
    zipCmd = c("zip -r $$file$$ .", "unzip -o $$file$$"), 
    cleanup = TRUE, verbose = TRUE)}
\arguments{
  \item{zipCmd}{a string for the zipping/unzipping the \code{odt} file
    via a system call. The token \code{$$file$$} will be gsub'ed with the file name.}
  \item{cleanup}{a logical: remove the working directory?}
  \item{verbose}{a logical: should details be printed?}
}

\value{
a list with element for each of the arguments above. 
}
\author{Max Kuhn}

\seealso{\code{\link{odfWeave}}}
\examples{
odfWeaveControl(cleanup = TRUE)
}
\keyword{utilities}
