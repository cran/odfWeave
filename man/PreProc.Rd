\name{odfTranslate}
\alias{odfTranslate}


\title{Translation of text to XML}
\description{

\code{odfTranslate} converts some XML modified characters (such as
\code{&gt}) to R code (\code{>}). This function also tries to mistake proof the code by anticipating characters that might be in UTF-8 encoding to R compliant characters (e.g. OpenOffice may convert some characters. For example, " will become a UTF-8 character, which R will choke on).

}
\usage{
odfTranslate(x, toR = TRUE)
}
\arguments{
  \item{x}{a character vector}
  \item{toR}{a logical. If \code{TRUE}, the text is translated from XML to R. The opposite is done if \code{FALSE}.}
}

\value{
a character vector
}
\author{Max Kuhn and Nathan Coulter}

\examples{

y <- "This is an &quot;interesting&quot; function."

cat(odfTranslate(y))
}
\keyword{utilities}
