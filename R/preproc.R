# The purpose of this file is to transform the content.xml file
# from an ODF document into a file that can be processed by
# the Sweave function.  That involves pulling lines of text
# out of paragraphs, and replacing them with line of R code.
# For example:
#
#   <text:p style:name="Standard">&lt;&lt;&gt;&gt;=</text:p>
#   <text:p style:name="Standard">x &lt;- 42</text:p>
#   <text:p style:name="Standard">@</text:p>
# 
# gets turned into:
#
#   <<>>=
#   x <- 42
#   @
#
# Things get tricky when we have things like footnotes,
# frames, tables, text formatting, line breaks, etc.
#
# The plan is to extract multiple streams of text from
# content.xml.  The text normally comes from text:p elements,
# but we also include the text from text:span elements that
# are children of text:p elements.  There are multiple streams
# because we get one stream for each office:text, table:cell,
# and text:note-body element in content.xml.  Within a stream,
# we look for code chunks.  When we find one, we remove the
# text:p and text:span elements that defined it, and replace
# the first one with the raw text.  That raw text will later
# be replaced by one of more text:p elements during the
# Sweave phase.
#

# utility functions that are only used within this file

# tests if the argument is a start-of-code-chunk marker
startOfCodeChunk <- function(x)
{
   if (!is.null(x))
   {
      s <- strsplit(trim(x), '')[[1]]
      n <- length(s)
      n >= 5 && s[c(1,2)] == '<' && s[c(n-2, n-1)] == '>' && s[n] == '='
   } else {
      FALSE
   }
}

# tests if the argument is an end-of-code-chunk marker
endOfCodeChunk <- function(x)
{
   !is.null(x) && trim(x) == '@'
}

# tests if the argument is a start-of-options marker
sweaveOpts <- function(x)
{
   !is.null(x) && regexpr('\\\\SweaveOpts\\{.*\\}', x) != -1
}

# tests if the argument contains a start-of-code-expr marker
startOfCodeExpr <- function(x)
{
   if (is.null(x))
      x <- ''
   regexpr('\\\\Sexpr\\{', x)
}

# tests if the argument contains an entire code-expr
codeExpr <- function(x)
{
   if (is.null(x))
      x <- ''
   regexpr('\\\\Sexpr\\{.*\\}', x)
}

# tests if the argument is an end-of-code-expr marker
endOfCodeExpr <- function(x)
{
   if (is.null(x))
      x <- ''
   regexpr('\\}', x)
}

processExpr <- function(t, outfile)
{
   if (!is.null(t))
   {
      t <- correct(t)
      debug('processExpr: expr:', t)
      cat(t, file=outfile)
   }
}

processCodeExprs <- function(x, outfile)
{
   # this function must be called from the 'Start' state,
   # but it doesn't check
   state <- 'Start'
   while ((e <- startOfCodeExpr(x)) != -1)
   {
      processText(substr(x, 1, e - 1), outfile)  # handle text before the Sexpr
      if ((e2 <- codeExpr(x)) != -1)             # see if we have the entire Sexpr
      {
         m <- e2 + attr(e2, 'match.length')
         sexpr <- substr(x, e2, m - 1)  # get the entire Sexpr
         processExpr(sexpr, outfile)    # write the Sexpr
         x <- substr(x, m, nchar(x))    # get text after the Sexpr
      } else {
         # the remaining text is all part of the Sexpr
         processExpr(substr(x, e, nchar(x)), outfile)
         x <- ''           # this will break the loop
         state <- 'Sexpr'  # we're in the middle of a Sexpr, so change state
      }
   }

   # at this point, x could be the entire input (because there wasn't
   # any Sexpr), the text trailing a Sexpr, or an empty string
   # (because we only got part of the Sexpr).  we call processText,
   # because that must be done for the first two cases, and doesn't
   # hurt in the last case.
   processText(x, outfile)
   state
}

# tests if the argument is the name of a legal text parent
legalParent <- function(name)
{
   # XXX I don't think a 'text:list-item' should be a legal parent, because
   # I suspect it can only have a single 'text:p' as a child.
   # Actually, I'm not sure if a 'table:table-cell' allows multiple children.
   any(name == c('office:text', 'table:table-cell', 'text:note-body', 'draw:text-box', 'text:list-item'))
}

# event handler functions

# this handler is called at the start of parsing the XML document.
# this is used in the "post" phase, where XML is actually generated.
# it is also used by postproc and procstyles.
startDocument <- function(.state)
{
   cat('<?xml version="1.0" encoding="UTF-8"?>\n', file=.state$outfile)
   .state
}

# this handler is called at the end of parsing the XML document.
# this is used in the "post" phase, where XML is actually generated.
# it is also used by postproc and procstyles.
endDocument <- function(.state)
{
   cat('\n', file=.state$outfile)  # prevents warning from R readline
   .state
}

# this isn't technically a handler, but it does the work of one.  The
# reason that the code is here is so it can be called from both
# preStartElement and preText.
preHandleText <- function(x, .state)
{
   if (!is.null(x))
   {
      debug('preHandleText: text:', x)
      i <- .state$curlevel
      if (i > 0 && any(.state$path[i] == c('text:span', 'text:sequence', 'text:s')))
         i <- i - 1

      # decide if we are interested in this text or not
      if (i > 1 && .state$path[i] == 'text:p' && legalParent(.state$path[i - 1]))
      {
         # figure out where this text came from
         parentId <- .state$ipath[i - 1]
         debug('preHandleText: parentId:', parentId)
         debug('preHandleText: textparent:', .state$textparent)

         if (.state$state == 'Start')
         {
            # we're not in a code chunk
            if (parentId == .state$textparent)
            {
               # append to the existing line of text
               debug('preHandleText: appending text to existing line')
               .state$text <- c(.state$text, correct(x))
               .state$endid <- .state$eventid
            } else {
               # start a new line of text
               if (.state$textparent == 0)
                  debug('preHandleText: starting a new line of text')
               else
                  debug('preHandleText: adopting a new parent since I am in Start mode')

               .state$text <- correct(x)
               .state$textparent <- parentId
               .state$startid <- .state$eventid
               .state$endid <- .state$eventid
            }
         } else if (.state$state == 'CodeChunk') {
            # we're in a code chunk, so ignore any "nested text"
            if (parentId == .state$textparent) {
               # append to the existing line of text
               debug('preHandleText: appending text to existing line (code mode)')
               .state$text <- c(.state$text, correct(x))
               .state$endid <- .state$eventid
            } else if (.state$textparent == 0) {
               # start a new line of text
               debug('preHandleText: starting a new line of text (code mode)')
               .state$text <- correct(x)
               .state$textparent <- parentId
               .state$startid <- .state$eventid
               .state$endid <- .state$eventid
            } else {
               # this happens when you have a footnote in the middle of a
               # code chunk, for instance
               debug('preHandleText: ignoring text from a different parent (code mode)')
            }
         } else {
            stop('illegal state: ', .state$state)
         }
         debug('preHandleText: collected text:', .state$text)
      } else {
         # this is text that isn't in a paragraph, or the paragraph has
         # an invalid parent
         debug('preHandleText: ignoring text')
      }
   } else {
      debug('ignoring NULL')
   }

   .state
}

# the following three handler functions are prefixed with "pre" because
# they are used before the "post" handlers.  the "pre" handlers analyze
# the content.xml file, mostly finding where the code chunks are defined
# so the "post" handlers can more easily replace the appropriate XML
# subtrees with the raw code chunk text.
preStartElement <- function(name, atts, .state)
{
   .state$eventid <- .state$eventid + 1
   .state$curlevel <- .state$curlevel + 1
   .state$ipath <- c(.state$ipath, .state$eventid)
   .state$path <- c(.state$path, name)

   if (name == 'text:s')
   {
      debug('preStartElement: found a text:s element')
      count <- if ('text:c' %in% names(atts)) as.integer(atts['text:c']) else 1
      x <- paste(rep(' ', count), collapse='')
      .state <- preHandleText(x, .state)
   }

   # we also want to remember the ref-name of all sequences by name
   if (name == 'text:sequence')
   {
      .state$seqInfo[[atts['text:name']]] <-
         c(.state$seqInfo[[atts['text:name']]], atts['text:ref-name'])
   } else if (name == 'text:sequence-decl') {
      .state$seqInfo[[atts['text:name']]] <- character()
   }

   stopifnot(.state$curlevel == length(.state$ipath))
   stopifnot(.state$curlevel == length(.state$path))
   .state
}

preText <- function(x, .state)
{
   .state$eventid <- .state$eventid + 1
   .state <- preHandleText(x, .state)
   .state
}

preEndElement <- function(name, .state)
{
   .state$eventid <- .state$eventid + 1

   if (name == 'text:p' || name == 'text:line-break')
   {
      txt <- paste(.state$text, collapse='')
      if (nchar(txt) > 0)
      {
         debug('preEndElement: text:', txt)

         i <- .state$curlevel
         # XXX have to think about this
         while (.state$path[i] %in% c('text:line-break', 'text:span'))
            i <- i - 1

         if (i > 1 && .state$ipath[i - 1] == .state$textparent)
         {
            parentId <- .state$ipath[i - 1]
            if (.state$state == 'Start')
            {
               if (startOfCodeChunk(txt))
               {
                  debug('preEndElement: found start of code chunk')
                  .state$state <- 'CodeChunk'
                  cc <- list(parentId=parentId, nlines=1, txt=txt, startid=.state$startid, endid=.state$endid)
                  .state$codeChunks <- c(.state$codeChunks, list(cc))
               } else if (sweaveOpts(txt)) {
                  debug('preEndElement: found SweaveOpts')
                  cc <- list(parentId=parentId, nlines=1, txt=txt, startid=.state$startid, endid=.state$endid)
                  .state$codeChunks <- c(.state$codeChunks, list(cc))
               } else {
                  debug('preEndElement: still no start of code chunk')
               }
            } else if (.state$state == 'CodeChunk') {
               if (endOfCodeChunk(txt))
               {
                  debug('preEndElement: found end of code chunk')
                  .state$state <- 'Start'
               } else if (startOfCodeChunk(txt)) {
                  warning('found start of code chunk in a code chunk')
               } else {
                  debug('preEndElement: still in code chunk')
               }

               cc <- .state$codeChunks[[length(.state$codeChunks)]]
               stopifnot(cc$parentId == parentId)
               cc$nlines <- cc$nlines + 1
               cc$txt[cc$nlines] <- txt
               cc$endid <- .state$endid
               .state$codeChunks[[length(.state$codeChunks)]] <- cc
            } else {
               stop('illegal state: ', .state$state)
            }
            .state$text <- character()
            .state$textparent <- 0
            .state$startid <- 0
            .state$endid <- 0
         } else {
            debug('preEndElement: skipping this nested element')
         }
      } else {
         debug('preEndElement: no text collected')
      }
   }

   .state$ipath <- .state$ipath[-(.state$curlevel)]
   .state$path <- .state$path[-(.state$curlevel)]
   .state$curlevel <- .state$curlevel - 1
   stopifnot(.state$curlevel == length(.state$ipath))
   stopifnot(.state$curlevel == length(.state$path))
   .state
}

# the following three handler functions are prefixed with "post" because
# they are used after the "pre" handlers.  the "post" handlers do the
# actual XML generation using information about the location of code
# chunks that were found by the "pre" handlers.
postStartElement <- function(name, atts, .state)
{
   .state$eventid <- .state$eventid + 1
   .state$curlevel <- .state$curlevel + 1
   .state$ipath <- c(.state$ipath, .state$eventid)
   .state$path <- c(.state$path, name)

   if (.state$state == 'Start')
   {
      if (!is.null(.state$buffer))
      {
         openTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)
         .state$buffer <- NULL
      }
      .state$buffer <- list(name, atts)
   } else {
      debug('postStartElement: dumping because state is', .state$state)
      stopifnot(is.null(.state$buffer))
   }

   stopifnot(.state$curlevel == length(.state$ipath))
   stopifnot(.state$curlevel == length(.state$path))
   .state
}

postText <- function(x, .state)
{
   .state$eventid <- .state$eventid + 1

   # this handles cci getting bigger than codeChunks
   cc <- if (.state$cci <= length(.state$codeChunks))
            .state$codeChunks[[.state$cci]]
         else
            list(startid=0, endid=0)

   if (.state$state == 'Sexpr')
   {
      # check for an Sexpr
      if ((e <- endOfCodeExpr(x)) != -1)
      {
         processExpr(substr(x, 1, e - 1), .state$outfile)
         x <- substr(x, e, nchar(x))  # get the text after the Sexpr
         .state$state <- 'Start'  # unnecessary, but true
         .state$state <- processCodeExprs(x, .state$outfile)
      } else {
         # didn't find the end of the Sexpr so we just process it
         processExpr(x, .state$outfile)
      }
   } else if (.state$eventid == cc$startid) {
      # state could be 'Start' or 'Undumping'
      stopifnot(.state$state %in% c('Start', 'Undumping'))

      # close out the current paragraph if necessary.
      # if this is the first child of the paragraph,
      # then .state$buffer will not be NULL and the
      # last element of path will be text:p.
      if (is.null(.state$buffer) || .state$path[.state$curlevel] != 'text:p')
      {
         debug('postText: closing out current paragraph')
         if (!is.null(.state$buffer))
            openTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)
         i <- length(.state$path)
         repeat
         {
            closeTag(.state$path[i], .state$outfile)
            if (.state$path[i] == 'text:p') break
            i <- i - 1
         }
      } else {
         debug('postText: removing current paragraph')
      }
      .state$buffer <- NULL

      # now write out the code chunk
      cat('\n', file=.state$outfile)

      for (txt in .state$codeChunks[[.state$cci]]$txt)
         cat(txt, '\n', sep='', file=.state$outfile)

      .state$state <- 'Dumping'
   } else if (.state$state %in% c('Dumping', 'Undumping')) {
      if (.state$state == 'Dumping')
         debug('postText: dumping text that is part of code chunk')
      else
         debug('postText: dumping text that follows a code chunk in the same paragraph')
      stopifnot(is.null(.state$buffer))
   } else {
      stopifnot(.state$state == 'Start')
      if (!is.null(.state$buffer))
      {
         openTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)
         .state$buffer <- NULL
      }

      .state$state <- processCodeExprs(x, .state$outfile)
   }
   
   if (.state$eventid == cc$endid)
   {
      # this is the last bit of the code chunk, but we can only set the
      # state to 'Undumping'.  postEndElement for the 'text:p' will set
      # the state to 'Start' when he sees that the state is 'Undumping'.
      stopifnot(.state$state == 'Dumping')
      .state$state <- 'Undumping'
   }

   .state
}

postEndElement <- function(name, .state)
{
   .state$eventid <- .state$eventid + 1

   # this handles cci getting bigger than codeChunks
   cc <- if (.state$cci <= length(.state$codeChunks))
            .state$codeChunks[[.state$cci]]
         else
            NULL  # should never be used

   if (.state$state == 'Start')
   {
      if (!is.null(.state$buffer))
      {
         completeTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)
         .state$buffer <- NULL
      } else {
         closeTag(name, .state$outfile)
      }
   } else if (.state$state == 'Undumping') {
      if (cc$parentId == .state$ipath[.state$curlevel - 1])
      {
         debug('postEndElement: dumping, but changing state to Start')
         .state$state <- 'Start'
         .state$cci <- .state$cci + 1
      } else {
         debug('postEndElement: dumping while state is Undumping')
      }
   } else {
      debug('postEndElement: dumping while state is Dumping')
      stopifnot(.state$state == 'Dumping' || .state$state == 'Sexpr')
   }

   .state$ipath <- .state$ipath[-(.state$curlevel)]
   .state$path <- .state$path[-(.state$curlevel)]
   .state$curlevel <- .state$curlevel - 1
   stopifnot(.state$curlevel == length(.state$ipath))
   stopifnot(.state$curlevel == length(.state$path))
   .state
}

# this is the main function that turns the content.xml file into
# a file that can be processed by the Sweave function
preproc <- function(infile, outfile)
{
   if (is.character(outfile))
   {
       outfile <- file(outfile, 'wb')
       on.exit(close(outfile))
   }

   state <- list(state='Start', ipath=integer(), path=character(),
                 text=character(), eventid=0, curlevel=0, textparent=0,
                 startid=0, endid=0, codeChunks=list(), seqInfo=list(),
                 outfile=outfile)
   handlers <- list(startElement=preStartElement,
                    endElement=preEndElement,
                    text=preText)
   state <- xmlEventParse(infile, handlers=handlers, trim=FALSE, state=state)

   assign("seqInfo", state$seqInfo, env=.odfEnv)

   # sanity check the results of preprocessing
   for (cc in state$codeChunks)
   {
      if (verbose)
         print(cc)

      stopifnot(length(cc$nlines) == 1)
      stopifnot(length(cc$parentId) == 1)
      stopifnot(cc$endid >= cc$startid)
      stopifnot(cc$nlines > 0)
      stopifnot(cc$nlines == length(cc$txt))
   }

   # now for postprocessing where we actually transform content.xml
   state <- list(ipath=integer(), path=character(), eventid=0, curlevel=0,
                 state='Start', cci=1,
                 buffer=NULL, codeChunks=state$codeChunks, outfile=outfile)
   handlers <- list(startElement=postStartElement,
                    endElement=postEndElement,
                    text=postText,
                    startDocument=startDocument,
                    endDocument=endDocument)
   state <- xmlEventParse(infile, handlers=handlers, trim=FALSE, state=state)

   invisible(state)  # not sure whether I should encourage this
}
