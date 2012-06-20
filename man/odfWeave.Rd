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
  \item{workDir}{a path to a directory where the source file will be
    unpacked and processed. If it does not exist, it will be created.
    If it exists, it should be empty, since all its contents will be
    included in the generated file.}
  \item{control}{a list of control settings.  See
    \code{\link{odfWeaveControl}}
    for the names of the settable control values and their effects.}
}
\details{
\code{odfWeave} can be used to embed R code within a word processing
document. The \code{odfWeave} package was created so that the
functionality
of \code{\link[utils:Sweave]{Sweave}} can be used within a rich editor like
OpenOffice.  The generated document can also easily be edited.

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

Also, \code{jar} can be used. See \code{\link{odfWeaveControl}} for more
information on configuring \code{odfWeave} to use applications other than
\code{zip} and \code{unzip}. 

A few notes about file paths and working directories:
\itemize{
\item When specifying the location of the odt file, you cannot use relative paths such as \code{file = "../file.odt"}. If the input or output files are not in the current working directory, then the absolute path should be used

\item \code{odfWeave} changes the working directory to the location where the odt file is decompressed. If an error occurs within \code{odfWeave}, the working directory will be changed back to the original path

\item Since \code{odfWeave} changes the working directory when the code chunks are executed, references to files and directories should use absolute paths. For example, if you are using \code{read.csv} to bring data into R, the file specification should include the whole path since the working directory will have been changed to a temporary location
}
 

The functionality of \code{\link[utils:Sweave]{Sweave}} is mostly preserved in
\code{odfWeave}, such as weaving, hooks, figure environments, etc. Some
functionality, such as writing output to separate files for each code
chunk using the \code{split} argument, doesn't make sense when using
ODF. See \code{\link{RweaveOdf}} for more details about the available
options.

\code{odfWeave} uses the noweb convention for R code. In-line R commands
should be in \code{Sexpr} calls.  The \code{Sexpr} text should all be
completely in one visual format.
e.g., changing the color of part of the \code{Sexpr} could result in an error. Block code chunks should use the
\code{<<>>=} syntax (i.e. no Latex syntax will currently work).


The image format and sizes are specified using \code{\link{setImageDefs}}.
The dimensions of the image file and the dimensions of the rendered
image can be set independently. See \code{\link{setImageDefs}} for more
details.

The document formatting.odt in the package's examples directory
illustrates the process of changing the appearance of the various
document elements.
}

\value{
an ODF file with the R output
}
\author{Max Kuhn}

\seealso{ \code{\link{odfWeaveControl}},  \code{\link{RweaveOdf}}, \code{\link[utils:Sweave]{Sweave}}}
\examples{

\dontrun{vignette("odfWeave")}

\dontrun{
demoFile <- system.file("examples", "examples.odt", package = "odfWeave")
demoFile <- system.file("examples", "testCases.odt", package ="odfWeave")
demoFile <- system.file("examples", "formatting.odt", package = "odfWeave")
}

demoFile <- system.file("examples", "simple.odt", package = "odfWeave")
outputFile <- gsub("simple.odt", "output.odt", demoFile)

library(odfWeave)
odfWeave(demoFile, outputFile)
}
\keyword{utilities}
