\name{XML Processing Functions}
\alias{odfTranslate}
\alias{stripXmlTag}
\alias{processInLine}
\alias{xml2R}

\title{XML Processing Functions}
\description{
These function takes a character vector of XML for the Open Document Format and process it it various ways. 

\code{odfTranslate} converts some XML modified characters (such as
\code{&gt}) to R code (\code{>}). This function also tries to mistake proof the code by anticipating characters that might be in UTF-8 encoding to R compliant characters (e.g. OpenOffice may convert some characters. For example, " will become a UTF-8 character, which R will choke on).

\code{stripXmlTag}  removes any text between "<" and ">" and optionally remove leading or trailing whitespace. 

\code{processInLine} uses \code{odfTranslate} and \code{stripXmlTag} to process in-line \code{Sweave} statements. Similarly, \code{xml2R} does the same thing for code chunks.
}
\usage{
odfTranslate(x, toR = TRUE)
stripXmlTag(x, leadWhite = FALSE, endWhite = FALSE)
processInLine(x, matches)
xml2R(x)
}
\arguments{
  \item{x}{a character vector}
  \item{toR}{a logical. If \code{TRUE}, the text is translated from XML to R. The opposite is done if \code{FALSE}.}
  \item{leadWhite}{a logical for removing leading white space}
  \item{endWhite}{a logical for removing tailing white space}
  \item{matches}{an integer vector with the character positions of any Sweave tags. \code{matches} must contain attributes \code{match.length} and \code{match.type} which are the length of the tag and the type (e.g. "sepxr"), respectively}  
}

\value{
a character vector
}
\author{Max Kuhn and Nathan Coulter }

\examples{

x <- "   <text:p text:style-name=\"P3\"><text:s text:c=\"3\"/>lm(y~x)</text:p>"
y <- "\\Sexpr{paste(letters[1:5], <text:s text:c=\"2\"/>collapse = &quot;,&quot;)}"
z <- " <text:span text:style-name=\"T1\">\\Sexpr{x &gt; 2}</text:span>"

stripXmlTag(x)
odfTranslate(y)
xml2R(y)
#processInLine(z)
}
\keyword{utilities}
