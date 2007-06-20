\name{odfSetPageStyle}
\alias{odfSetPageStyle}
\title{Set the Page Style}
\description{
  Insert a page break with a specified page style.
}
\usage{
odfSetPageStyle(style="Standard")
}
\arguments{
  \item{style}{a character strig of length one that references a page
    style that is either contained in the style definitions or already
    used within the document.}
}
\details{
  This function should be called in a code chunk in an odfWeave document
  at the point where a page break with a page style is desired.
  A common use would be to change the page to landscape mode, perhaps
  using the "RlandscapePage" style in order to display a table in
  landscape mode, and then to set it back to the standard style
  after generating the table.
}
\examples{
\dontrun{
odfSetPageStyle("RlandscapePage")
}
}
\keyword{utilities}
