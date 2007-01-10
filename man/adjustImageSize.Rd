\name{adjustImageSize}
\alias{adjustImageSize}
\title{Automate image size adjustments}
\description{
This function is a wrapper for \code{getImageDefs} and \code{setImageDefs} to allow the user to change image sizes in one function call.
}
\usage{
adjustImageSize(x, y, scale = 1, res = 96)
}
\arguments{
  \item{x}{the display width of an image (in inches)}
  \item{y}{the display height of an image (in inches)}
  \item{scale}{a scaling factor for the image file size}
  \item{res}{resolution for bitmap images}
}
\details{
This function will get the current image specifications, change them and reset them using \code{setImageDefs}. If the device is either \code{bmp}, \code{jpeg} or \code{png}, the image will be converted to pixel size using the \code{res} argument.

Optionally, to make bitmap images look a little better, the \code{scale} argument can be used to scale-up the image file. This also has the effect of reducing the size of text and points in the displayed image.
}
\value{
No data are returned.
}
\author{Sarah Goslee, Max Kuhn}
\examples{
getImageDefs()
adjustImageSize(2, 5, scale = 1.75)
getImageDefs()
}
\keyword{utilities}
