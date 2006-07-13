\name{setStyles}
\alias{getStyles}
\alias{setStyles}
\alias{getStyleDefs}
\alias{setStyleDefs}
\title{Style Definitions and Assignments}
\description{
Utility functions for declaring and setting styles
}
\usage{
getStyles()
setStyles(style)
getStyleDefs()
setStyleDefs(def)
}
\arguments{
  \item{style}{a listof style assignments}
  \item{def}{a list of style definitions}
}
\details{

There are two main components to specifying output formats: style definitions and style assignments. The definition has the specific components (such as a table cell) and their format values (e.g. boxed with solid black lines). The function \code{getStyleDefs} can fetch the pre-existing styles in the package. These can be modified and new definitions can be added. The function \code{setStyledefs} ``registers'' the style changes with the package. When \code{odfWeave} is called, these definitions are written to the style sections of the XML files. See the example below.

There is a second mechanism to assign  styles to specific output elements. The functions \code{getStyles} and \code{setStyles} can be used to tell \code{\link{odfWeave}}  which style definition to use for a particular output.

For example, the \code{input} and \code{output} elements control how R code and command--line output look. To change either of these, an existing definition can be assigned to these entries and reset using \code{setStyles(currentStyles)}. Unlike the style definitions, the style assignments can be modified throughout the R code.

}
\value{
The get functions return lists.
}
\author{Max Kuhn}

\examples{
currentStyleDefs <- getStyleDefs()
currentStyleDefs$ArialNormal$fontSize <- "10pt"
setStyleDefs(currentStyleDefs)
}
\keyword{utilities}

