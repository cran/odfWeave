\name{setStyles}
\alias{getStyles}
\alias{setStyles}
\alias{getStyleDefs}
\alias{setStyleDefs}
\alias{getImageDefs}
\alias{setImageDefs}
\title{Style Definitions and Assignments}
\description{
Utility functions for declaring and setting styles
}
\usage{
getStyles()
setStyles(style)

getStyleDefs()
setStyleDefs(def)

getImageDefs()
setImageDefs(def, verbose = TRUE)
}
\arguments{
  \item{style}{a list of style assignments}
  \item{def}{a list of style definitions}
  \item{verbose}{a logical: should warnings be printed?}
}
\details{

There are two main components to specifying output formats: style definitions and style assignments. The definition has the specific components (such as a table cell) and their format values (e.g. boxed with solid black lines). The function \code{getStyleDefs} can fetch the pre-existing styles in the package. These can be modified and new definitions can be added. The function \code{setStyledefs} ``registers'' the style changes with the package. When \code{odfWeave} is called, these definitions are written to the style sections of the XML files. See the example below.

There is a second mechanism to assign styles to specific output elements. The functions \code{getStyles} and \code{setStyles} can be used to tell \code{\link{odfWeave}}  which style definition to use for a particular output.

For example, the \code{input} and \code{output} elements control how R code and command--line output look. To change either of these, an existing definition can be assigned to these entries and reset using \code{setStyles(currentStyles)}. Unlike the style definitions, the style assignments can be modified throughout the R code.

For graphics, \code{getImageDefs} and \code{setImageDefs} can be used to
specifiy the type of plot device and it's arguments. \code{getImageDefs}
will return a list with elements
\describe{
\item{type}{a character string for the image type (this is also used to
  set the file extension). Possible values are "png", "jpeg", "bmp", or
  "eps" (OpenOffice does not accept pdf or svg graphics)}
\item{device}{a character string for the device that should be used to
  generate the graphics. Some systems may not have png or jpeg devices
  setup, so \code{capabilities} is used to make that determination by
  default}
\item{plotHeight}{the height for the image file. For "png", "bmp" and
  "jpeg" devices, this is in pixels, but for others it is in inches}
\item{plotWidth}{similar to \code{plotHeight}}
\item{dispWidth}{the height of the image, in inches, as shown in OpenOffice}
\item{dispWidth}{similar to \code{dispHeight}}
Since these functions can be called from within code chunks, graphical
parameters can be changes during the Sweave process.
}
}
\value{
The get functions return named lists.
}
\author{Max Kuhn}

\examples{
currentStyleDefs <- getStyleDefs()
currentStyleDefs$ArialNormal$fontSize <- "10pt"
setStyleDefs(currentStyleDefs)
}
\keyword{utilities}


