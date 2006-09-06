\name{odfWeave}
\alias{odfWeave}
\title{Sweave processing of Open Document Format (ODF) files}
\description{
  Sweave processing of Open Document Format files
}
\usage{
odfWeave(file, dest, workDir = odfTmpDir(), control = odfWeaveControl())
}
\arguments{
  \item{file}{the ODF file created using OpenOffice V2.0 or above.}
  \item{dest}{path to put the processed file (should include file name and extension)}  
  \item{workDir}{a path to a directory where the source file will be unpacked and processed}
  \item{control}{a list of control settings.  See
    \code{\link{odfWeaveControl}}
    for the names of the settable control values and their effects.}
}
\details{
\code{odfWeave} can be used to embed R code within a word processing
document. The \code{odfWeave} package was created so that the
functionality
of \code{\link[utils]{Sweave}} can used to generate documents
that the end--user can easily edit. 

The markup language used is the Open Document Format (ODF), which is an
open, non--proprietary format that  encompasses text documents,
presentations  and spreadsheets. There are several editors/office suites
that can produce ODF files. OpenOffice, as of version 2.0, uses ODF as
the default format. \code{odfWeave}
has been tested with OpenOffice to produce text documents. As of the
current version,  \code{odfWeave} processing of presentations and
spreadsheets should be considered to be experimental (but should be
supported in subsequent versions). 

Since ODF files are compressed archives of files and directories, R
will need to zip and unzip the source file. While R has an unzip
utility, it does not have one for re-zipping files, so an external
application is needed. \code{unzip} and \code{zip} are free utilities located at 

\url{http://www.info-zip.org/}

Also, \code{jar} can be used. See \code{link{odfWeaveControl}} for more
information on configuring \code{odfWeave} to use applications other than
\code{zip} and \code{unzip}. 

The functionality of \code{\link[utils]{Sweave}} is mostly preserved in
\code{odfWeave}, such as weaving, hooks, figure environments, etc. Some
functionality, such as writing output to separate files for each code
chunk using the \code{split} argument, doesn't make sense when using
ODF. See \code{\link{RweaveOdf}} for more details about the available
options.

\code{odfWeave} uses the noweb convention for R code. In-line R commands
should be in \code{\Sexpr} calls and block code chunks should use the
\code{<<>>=} syntax (i.e. no Latex syntax will currently work).


The image format and sizes are specified using \code{\link{setImageDefs}}.
The dimensions of the image file and the dimensions of the rendered
image can be set independently. See \code{\link{setImageDefs}} for more details.
}
\value{
an ODF file with the R output
}
\author{Max Kuhn}

\seealso{ \code{\link{odfWeaveControl}},  \code{\link{RweaveOdf}}, \code{\link[utils]{Sweave}}}
\examples{

\dontrun{vignette("odfWeave")}

\dontrun{
demoFile <- system.file("examples", "examples.odt", package = "odfWeave")
demoFile <- system.file("examples", "testCases.odt", package = "odfWeave")
}

demoFile <- system.file("examples", "simple.odt", package = "odfWeave")
outputFile <- gsub("simple.odt", "output.odt", demoFile)

library(odfWeave)
odfWeave(demoFile, outputFile)
}
\keyword{utilities}
