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
  \item{workDir}{a path to a directory where the source file will be unpacked and processed.  If it does not exist, it will be created.  If it exists, it should be empty, since all its contents will be included in the generated file.}
  \item{control}{a list of control settings.  See
    \code{\link{odfWeaveControl}}
    for the names of the settable control values and their effects.}
}
\details{
\code{odfWeave} can be used to embed R code within a word processing
document. The \code{odfWeave} package was created so that the
functionality
of \code{\link[utils]{Sweave}} can be used within a rich editor like
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
should be in \code{\Sexpr} calls.  the \code{Sexpr} text should all be
completely in one visual format.
e.g., changing the color of part of the \code{Sexpr} could result in an error. Block code chunks should use the
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
demoFile <- system.file("examples", "testCases.odt", package ="odfWeave")
demoFile <- system.file("examples", "formatting.odt", package = "odfWeave")
}

demoFile <- system.file("examples", "simple.odt", package = "odfWeave")
outputFile <- gsub("simple.odt", "output.odt", demoFile)

library(odfWeave)
odfWeave(demoFile, outputFile)

\dontshow{
   odfWeave(
      system.file("examples", "examples.odt", package = "odfWeave"),
      gsub(
         "examples.odt", "examplesOut.odt",
         system.file("examples", "examples.odt", package = "odfWeave"),
         fixed = TRUE))

   odfWeave(
      system.file("examples", "testCases.odt", package = "odfWeave"),
      gsub(
         "testCases.odt", "testCasesOut.odt",
         system.file("examples", "examples.odt", package = "odfWeave"),
         fixed = TRUE))

   require(stats)
   attach(environmental)
   data(state)
   
   options(width = 90)
   
   basicStyles <- getStyleDefs()
   
   basicStyles$lowerBorder$backgroundColor <-  "#FFFFFF"
   
   basicStyles$lowerBorderGray <-  basicStyles$lowerBorder
   basicStyles$lowerBorderGray$backgroundColor <-  "#E8E8E8"
   
   basicStyles$ArialCenteredItalics <- basicStyles$ArialCenteredBold
   basicStyles$ArialCenteredItalics$fontSize <- "10 pt"
   basicStyles$ArialCenteredItalics$fontType <- "italic"
   
   basicStyles$ArialHighlight$fontColor <- "#000080"
   basicStyles$ArialHighlight$fontType <- ""
   
   basicStyles$highlight <- basicStyles$noBorder
   basicStyles$highlight$backgroundColor <- "#D4EBF3"
   
   basicStyles$RTable2 <- basicStyles$RTable1
   basicStyles$RTable2$marginLeft <- "1.5in"
   basicStyles$RTable2$marginRight <- "1.5in"
   basicStyles$RTable2$marginTop <- "1in"
   basicStyles$RTable2$marginBottom <- "2in"
   
   basicStyles$ttRed$fontSize <- "9pt"
   basicStyles$ttBlue$fontSize <- "9pt"
   
   setStyleDefs(basicStyles)
   
   plotInfo <- getImageDefs()
   plotInfo$dispHeight <- 4  
   plotInfo$dispWidth <- 4 
   setImageDefs(plotInfo)

   odfWeave(
      system.file("examples", "formatting.odt", package = "odfWeave"),
      gsub(
         "formatting.odt", "formattingOut.odt",
         system.file("examples", "formatting.odt", package = "odfWeave"),
         fixed = TRUE))

}



}
\keyword{utilities}
