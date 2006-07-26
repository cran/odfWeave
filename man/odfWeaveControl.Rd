\name{odfWeaveControl}
\alias{odfWeaveControl}

\title{Control odfWeave options}
\description{
Allows the user to specify how odfWeave operates and style information for the document.
}
\usage{
odfWeaveControl(
    zipCmd = c("zip -r $$file$$ .", "unzip -o $$file$$"), 
    cleanup = FALSE, verbose = TRUE, plotType = "png", 
    plotDevice = ifelse(.Platform$OS.type == "windows", "png",
"bitmap"),
    plotHeight = ifelse(plotDevice \%in\% c("jpeg", "png", "bmp"), 480, 6),
    plotWidth = ifelse(plotDevice \%in\% c("jpeg", "png", "bmp"), 480, 6),
    dispHeight = 4.5, dispWidth = 4.5) 
}
\arguments{
  \item{zipCmd}{a string for the zipping/unzipping the \code{odt} file. The token \code{$$file$$} will be gsub'ed with the file name.}
  \item{cleanup}{a logical: remove the working directory?}
  \item{verbose}{a logical: should details be printed?}
  \item{plotType}{the image type}
  \item{plotDevice}{the R device to use }
  \item{plotHeight}{height of the image as displayed in the document (units depend on the device)}
  \item{plotWidth}{width of the image as displayed in the document (units depend on the device)}  
  \item{dispHeight}{height of the image as displayed in the document (units depend on the device)}
  \item{dispWidth}{width of the image as displayed in the document (units depend on the device)}
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
