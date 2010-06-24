\name{odfFigureCaption}
\alias{odfFigureCaption}
\title{Provide a Caption for a Figure}
\description{
  Provide a numbered caption for a figure.  Captions are automatically numbered,
  and by default using arabic numerals, but letters or roman numerals can also
  be specified via the numformat argument.
}
\usage{
odfFigureCaption(caption, 
                 numformat = '1', 
                 numlettersync = FALSE,
                 formula = 'Illustration+1', 
                 label = 'Illustration')
}
\arguments{
  \item{caption}{the text portion of the caption}
  \item{numformat}{the format to use the figure number}
  \item{numlettersync}{specifies the style of numbering to use if numformat is 'A' or 'a'}
  \item{formula}{the formula to use for computing this figure number from the previous}
  \item{label}{the label to use for the caption.  Defaults to 'Illustration'.}
}
\details{
  This function should be called no more than once in a code chunk where
  'figure' was set to true.

  Legal values for numformat are 'A', 'a', 'I', 'i', and '1'.

  If numformat is 'A' or 'a', numlettersync specifies what style of numbering
  to use after the first 26 figures.  If numlettersync is true, the next 26
  figures will be numbered 'AA', 'BB', ..., 'ZZ', 'AAA', 'BBB', etc.
  If numlettersync is false, the subsequent figures will be numbered 'AA', 'AB',
  ..., 'AZ', 'BA', 'BB', ..., 'BZ', etc.

  The default formula, which numbers figures consecutively, is usually desired,
  but you could specify a formula of 'Illustration+10' to have your figures
  numbered 1, 11, 21, etc.
}
\examples{
\dontrun{
odfFigureCaption("This is a very interesting figure")
}
}
\keyword{utilities}
