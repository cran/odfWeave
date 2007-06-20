\name{odfTableCaption}
\alias{odfTableCaption}
\title{Provide a Caption for a Table}
\description{
  Provide a numbered caption for a table.  Captions are automatically numbered,
  and by default using arabic numerals, but letters or roman numerals can also
  be specified via the numformat argument.
}
\usage{
odfTableCaption(caption, numformat='1', numlettersync=FALSE, formula='Table+1')
}
\arguments{
  \item{caption}{the text portion of the caption}
  \item{numformat}{the format to use the table number}
  \item{numlettersync}{specifies the style of numbering to use if numformat is 'A' or 'a'}
  \item{formula}{the formula to use for computing this table number from the previous}
}
\details{
  This function should be called immediately after a call to odfTable in a code chunk
  in an odfWeave document.

  Legal values for numformat are 'A', 'a', 'I', 'i', and '1'.

  If numformat is 'A' or 'a', numlettersync specifies what style of numbering
  to use after the first 26 tables.  If numlettersync is true, the next 26
  tables will be numbered 'AA', 'BB', ..., 'ZZ', 'AAA', 'BBB', etc.
  If numlettersync is false, the subsequent tables will be numbered 'AA', 'AB',
  ..., 'AZ', 'BA', 'BB', ..., 'BZ', etc.

  The default formula, which numbers tables consecutively, is usually desired,
  but you could specify a formula of 'Table+10' to have your tables
  numbered 1, 11, 21, etc.
}
\examples{
\dontrun{
odfTableCaption("This is a very boring table")
}
}
\keyword{utilities}
