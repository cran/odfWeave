\name{RweaveOdf}
\alias{RweaveOdf}
\alias{RweaveOdfSetup}
\title{R/ODF Driver for Sweave}
\description{
  A driver for \code{\link[utils]{Sweave}} that translates R code chunks in XML files
  produced in Open Document Format (ODF) files.
}
\usage{
RweaveOdf()

RweaveOdfSetup(file, syntax,
             output=NULL, quiet=FALSE, debug=FALSE, echo=TRUE,
             eval=TRUE, ...)
}
\arguments{
  \item{file}{Name of Sweave source file.}
  \item{syntax}{An object of class  \code{\link[utils]{SweaveSyntax}}.}
  \item{output}{Name of output file, default is to remove extension
    \file{.nw}, \file{.Rnw} or \file{.Snw} and to add extension
    \file{.xml}. Any 
    directory names in \code{file} are also removed such that the output
    is created in the current working directory.}
  \item{quiet}{If \code{TRUE} all progress messages are suppressed.}
  \item{debug}{If \code{TRUE}, input and output of all code
    chunks is copied to the console.}
  \item{echo}{set default for option \code{echo}, see details below.}
  \item{eval}{set default for option \code{eval}, see details below.}
  \item{\ldots}{optional arguments. This is used to pass the control object to the driver}  
}
\section{Supported Options}{
   \code{\link{RweaveOdf}} supports the following options for code chunks (the values
  in parentheses show the default values):
  \describe{
    \item{echo:}{logical (\code{TRUE}). Include S code in the
      output file?}
    \item{eval:}{logical (\code{TRUE}). If \code{FALSE}, the code chunk is not
      evaluated, and hence no text or graphical output produced.}
    \item{results:}{character string (\code{verbatim}).
      If \code{verbatim}, the output of S commands is
      included in the verbatim-like Soutput environment. If
      \code{xml}, the output is taken to be already proper XML markup
      and included as is. If \code{hide}, then all output is
      completely suppressed (but the code executed during the weave).}
    \item{print:}{logical (\code{FALSE})
      If \code{TRUE}, each expression in the
      code chunk is wrapped into a \code{print()} statement before evaluation,
      such that the values of all expressions become visible.}
    \item{term:}{logical (\code{TRUE}). If \code{TRUE}, visibility of values
      emulates an interactive R session: values of assignments are not
      printed, values of single objects are printed. If \code{FALSE},
      output comes only from explicit \code{\link{print}} or
      \code{\link{cat}} statements.}
    \item{fig:}{logical (\code{FALSE}), indicating whether the code
      chunk produces 
      graphical output. Note that only one figure per code chunk can be
      processed this way.}
  }
  Note that image options, such as the image type and size, are set using \code{\link{setImageDefs}}.
  
}
\author{Max Kuhn, based on \code{\link{RweaveLatex}} by Friedrich Leisch}

\seealso{\code{\link[utils]{Sweave}}, \code{\link{odfWeave}}, \code{\link{setImageDefs}}}
\keyword{utilities}
