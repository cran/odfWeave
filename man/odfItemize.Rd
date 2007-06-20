\name{odfItemize}
\alias{odfItemize}
\title{Write XML for one layer lists}
\description{
Creates ODF markup for enumerated or bulleted list one layer deep
}
\usage{
odfItemize(data, ...)
}
\arguments{
  \item{data}{a vector}
  \item{...}{options to pass to \code{format}}  
}

\value{
a string of XML markup
}
\author{Max Kuhn}

\examples{
odfItemize(levels(iris$Species))
}
\keyword{utilities}
