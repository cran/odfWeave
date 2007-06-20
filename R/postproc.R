# this is data needed by the toRoman function

decimalDens <- c(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
upperRomanDens <- c('M', 'CM', 'D', 'CD', 'C', 'XC', 'L', 'XL', 'X',
                    'IX', 'V', 'IV', 'I')
lowerRomanDens <- tolower(upperRomanDens)

toRoman <- function(dec, lower=TRUE)
{
   if (dec <= 0)
      stop('it must be positive')
   else if (dec >= 4000)
      stop('it must be lower than MMMM (4000)')

   if (lower)
      decToRoman(dec, '', decimalDens, lowerRomanDens)
   else
      decToRoman(dec, '', decimalDens, upperRomanDens)
}

decToRoman <- function(num, s, decs, romans)
{
   if (length(decs) > 0)
   {
      if (num < decs[1])
         decToRoman(num, s, decs[-1], romans[-1])
      else
         decToRoman(num - decs[1], paste(s, romans[1], sep=''), decs, romans)
   } else {
      s
   }
}

convert <- function(val, tail, alpha)
{
   if (val <= 0) {
      tail
   } else {
      x <- val %/% length(alpha)
      y <- val %% length(alpha)
      convert(x, paste(alpha[y], tail, sep=''), alpha)
   }
}

formatText <- function(val, fmt, numsync)
{
   if (fmt == 'A' | fmt == 'a')
   {
      alpha <- if (fmt == 'A') LETTERS else letters
      if (val < 1) val = 1
      x <- (val - 1) %/% length(alpha)
      y <- (val - 1) %% length(alpha)

      if (numsync)
         paste(rep(alpha[y + 1], x + 1), collapse='')
      else
         convert(x, alpha[y + 1], alpha)
   } else if (fmt == 'i') {
      toRoman(val, lower=TRUE)
   } else if (fmt == 'I') {
      toRoman(val, lower=FALSE)
   } else {
      # primarily intended for converting integers to strings
      # when fmt == '1'
      as.character(val)
   }
}

strToBool <- function(s) s == 'true' | s == 't'

uniqueName <- function(nvec, prefix)
{
   i <- 1
   repeat {
      n <- paste(prefix, i, sep='')
      if (!any(n == nvec)) break
      i <- i + 1
   }
   n
}

# event handler functions

content2StartElement <- function(name, atts, .state)
{
   if (!is.null(.state$buffer))
      openTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)

   if (name == 'style:font-face')
   {
      # keep track of all of the fonts that are defined
      .state$fontsDefined <- c(.state$fontsDefined, atts['style:name'])
   } else if (name == 'text:sequence') {
      seqName <- atts['text:name']
      formula <- atts['text:formula']
      formula <- sub('^[a-z]+: *', '', formula)
      # XXX should handle errors better
      val <- eval(parse(text=formula), .state$seqEnv)
      .state$seqEnv[[seqName]] <- val
      fmt <- atts['style:num-format']
      numsync <- atts['style:num-letter-sync']
      numsync <- if (is.na(numsync)) FALSE else strToBool(numsync)
      .state$seqValue <- formatText(val, fmt, numsync)
   }

   # check if this is a "break element"
   if (any(.state$n == .state$breakNumbers))
   {
      # need to change the value of the style-name attribute
      n <- .state$breakElements[[as.character(.state$n)]]$newStyle
      if (name == 'text:p')
      {
         atts[['text:style-name']] <- n
      } else if (name == 'table:table') {
         atts[['table:style-name']] <- n
      } else {
         stop('internal error: unhandled break element: ', name)
      }
   }

   .state$buffer <- list(name, atts)
   .state$n <- .state$n + 1
   .state
}

content2Text <- function(x, .state)
{
   if (!is.null(.state$buffer))
      openTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)

   # do not write the text value of a text:sequence element
   if (is.null(.state$seqValue))
      processText(x, .state$outfile)
   .state$buffer <- NULL
   .state
}

getChild <- function(node, attr, value)
{
   for (child in node$children)
   {
      if (!is.null(child$attributes[attr]) && child$attributes[attr] == value)
         return(child)
   }
   NULL
}

handleBreakElements <- function(.state)
{
   for (i in seq(along = .state$breakElements))
   {
      pageStyle <- .state$breakElements[[i]]$style

      if (.state$breakElements[[i]]$name == 'text:p')
      {
         n <- uniqueName(.state$allocatedNames, 'P')
         .state$allocatedNames <- c(.state$allocatedNames, n)
         .state$breakElements[[i]]$newStyle <- n
         styleName <- .state$breakElements[[i]]$atts[['text:style-name']]

         ia <- which(styleName == .state$autoStyleNames)
         if (length(ia) > 0)
         {
            # we cannot subclass an automatic style, so we have to
            # define a new automatic style that is the same as the
            # original, but with the page break
            xnode <- getChild(.state$autoStyles, 'style:name', styleName)
         } else {
            xnode <- xmlNode('style:style', attrs=c('style:parent-style-name'=styleName))
         }

         xnode$attributes['style:name'] <- n
         xnode$attributes['style:family'] <- 'paragraph'

         if (!is.null(pageStyle))
         {
            xnode$attributes['style:master-page-name'] <- pageStyle
         } else if (!is.null(xnode$children[['style:paragraph-properties']])) {
            xnode$children[['style:paragraph-properties']]$attributes['fo:break-before'] <- 'page'
         } else {
            xnode$children[[xmlSize(xnode) + 1]] <-
               xmlNode('style:paragraph-properties',
                       attrs=c('fo:break-before'='page'))
         }

         saveXML(xnode, file=.state$outfile, prefix=NULL)
      } else if (.state$breakElements[[i]]$name == 'table:table') {
         n <- .state$breakElements[[i]]$atts[['table:name']]
         if (any(n == .state$allocatedNames))
            n <- uniqueName(.state$allocatedNames, 'TTable')
         .state$allocatedNames <- c(.state$allocatedNames, n)
         .state$breakElements[[i]]$newStyle <- n
         styleName <- .state$breakElements[[i]]$atts[['table:style-name']]

         ia <- which(styleName == .state$autoStyleNames)
         if (length(ia) > 0)
         {
            # we cannot subclass an automatic style, so we have to
            # define a new automatic style that is the same as the
            # original, but with the page break
            xnode <- getChild(.state$autoStyles, 'style:name', styleName)
         } else {
            xnode <- xmlNode('style:style', attrs=c('style:parent-style-name'=styleName))
         }

         xnode$attributes['style:name'] <- n
         xnode$attributes['style:family'] <- 'table'

         if (!is.null(pageStyle))
         {
            xnode$attributes['style:master-page-name'] <- pageStyle
         } else if (!is.null(xnode$children[['style:table-properties']])) {
            xnode$children[['style:table-properties']]$attributes['fo:break-before'] <- 'page'
         } else {
            xnode$children[[xmlSize(xnode) + 1]] <-
               xmlNode('style:table-properties',
                       attrs=c('fo:break-before'='page'))
         }

         saveXML(xnode, file=.state$outfile, prefix=NULL)
      } else {
         # complain very bitterly
         stop('internal error: automatic-styles: ',
               .state$breakElements[[i]]$name)
      }
   }
   .state
}

content2EndElement <- function(name, .state)
{
   if (name == 'office:automatic-styles')
   {
      # add the automatic styles that we need
      if (!is.null(.state$buffer))
      {
         openTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)
         .state$buffer <- NULL
      }

      # now we add the styles needed for implementing page breaks
      .state <- handleBreakElements(.state)

      cat(odfStyleGen(getStyleDefs(), 'content'),
         file=.state$outfile)

      closeTag(name, .state$outfile)
   } else if (name == 'office:font-face-decls') {
      # define all fonts that are needed but not already defined
      if (!is.null(.state$buffer))
      {
         openTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)
         .state$buffer <- NULL
      }
      fonts <- setdiff(.state$fontsNeeded, .state$fontsDefined)
      cat(paste(unlist(lapply(fonts,
         function(font) {
            completeTag('style:font-face',
               c('style:name'=font, 'svg:font-family'=font),
              .state$outfile)
         })), collapse=''), file=.state$outfile)
      closeTag(name, .state$outfile)
   } else if (name == 'text:sequence') {
      # first write the correctly formatted sequence value,
      # and then the closing tag
      stopifnot(!is.null(.state$seqValue))
      if (!is.null(.state$buffer))
      {
         openTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)
         .state$buffer <- NULL
      }
      processText(.state$seqValue, .state$outfile)
      closeTag(name, .state$outfile)
      .state$seqValue <- NULL
   } else if (name == 'odfWeave:pageBreak') {
      # this is how we delete the phony <odfWeave:pageBreak/> element
      stopifnot(!is.null(.state$buffer))
      .state$buffer <- NULL
   } else {
      # now handle the closing tag
      if (!is.null(.state$buffer))
      {
         completeTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)
         .state$buffer <- NULL
      } else {
         closeTag(name, .state$outfile)
      }
   }
   .state
}

# The following handlers are used to collect information about the content
# after being run through Sweave, in preparation for the final XML
# transformations.  We learned about the original input XML in the first
# pass, but the Sweave pass changed it, so now we have got to scan it again
# before we will be ready to do the final transformations.
#
# To support page breaks, we will look for a <odfWeave:pageBreak/> element.
# When we find one, we look for the next paragraph of table element.  We
# need to remember it, so we can changes its style on the next pass.
#
# During the next pass we have to:
#  - remove all odfWeave:pageBreak elements
#  - 

infoStartElement <- function(name, atts, .state)
{
   if (name == 'odfWeave:pageBreak')
   {
      .state$looking <- TRUE
      if (!is.null(atts) && !is.na(atts['style']))
         .state$breakStyle <- atts['style']
   } else if (.state$looking && any(name == c('text:p', 'table:table'))) {
      .state$breakElements[[as.character(.state$n)]] <-
            list(n=.state$n, name=name, atts=atts, style=.state$breakStyle)
      .state$looking <- FALSE
      .state$breakStyle <- NULL
   } else if (name == 'office:automatic-styles' || .state$autoLevel > 0) {
      if (.state$autoLevel == 0)
         .state$autoLevel <- 1

      xnode <- xmlNode(name, attrs=atts)
      m <- length(.state$node)
      if (.state$autoLevel > m)
      {
         m <- m + 1
         stopifnot(.state$autoLevel == m)
         .state$node[[m]] <- list(xnode)
      } else {
         stopifnot(.state$autoLevel == m)
         n <- length(.state$node[[m]]) + 1
         .state$node[[m]][[n]] <- xnode
      }
      .state$autoLevel <- .state$autoLevel + 1
   }

   .state$n <- .state$n + 1
   .state
}

infoText <- function(x, .state)
{
   if (.state$autoLevel > 0)
   {
      xnode <- xmlTextNode(x)
      m <- length(.state$node)
      if (.state$autoLevel > m)
      {
         m <- m + 1
         stopifnot(.state$level == m)
         .state$node[[m]] <- list(xnode)
      } else {
         stopifnot(.state$autoLevel == m)
         n <- length(.state$node[[m]]) + 1
         .state$node[[m]][[n]] <- xnode
      }
   }

   .state
}

infoEndElement <- function(name, .state)
{
   if (.state$autoLevel > 0)
   {
      .state$autoLevel <- .state$autoLevel - 1
      m <- length(.state$node)
      if (m > .state$autoLevel)
      {
         n <- length(.state$node[[.state$autoLevel]])
         .state$node[[.state$autoLevel]][[n]]$children <- .state$node[[m]]
         .state$node[[m]] <- NULL
      }

      if (name == 'office:automatic-styles')
      {
         stopifnot(.state$autoLevel == 1)
         .state$autoLevel <- 0
      }
   }

   .state
}

# this is the main function that turns the output from Sweave
# into the content.xml file of the ODF file

postproc <- function(infile, outfile)
{
   outcon <- file(outfile, open='w')

   state <- list(n=0,
                 autoLevel=0,
                 node=list(),
                 looking=FALSE,
                 breakElements=list())
   handlers <- list(startElement=infoStartElement,
                    endElement=infoEndElement,
                    text=infoText)
   state <- xmlEventParse(infile, handlers=handlers, trim=FALSE, state=state)

   fontsNeeded <- unique(unlist(lapply(getStyleDefs(),
         function(font) font$fontName)))
   fontsDefined <- character()

   # initialize all declared sequences to zero
   seqEnv <- new.env()
   for (seq in names(.odfEnv$seqInfo))
      seqEnv[[seq]] <- 0

   autoStyles <- state$node[[1]][[1]]
   autoStyleNames <- unlist(lapply(autoStyles$children, function(x) x$attributes[['style:name']]))

   state <- list(buffer=NULL, outfile=outcon, seqEnv=seqEnv, seqValue=NULL,
                 fontsDefined=fontsDefined, fontsNeeded=fontsNeeded,
                 autoStyles=autoStyles,
                 autoStyleNames=autoStyleNames,
                 allocatedNames=autoStyleNames,
                 breakElements=state$breakElements,
                 breakNumbers=as.integer(names(state$breakElements)),
                 n=0)
   handlers <- list(startElement=content2StartElement,
                    endElement=content2EndElement,
                    text=content2Text,
                    startDocument=startDocument,
                    endDocument=endDocument)
   state <- xmlEventParse(infile, handlers=handlers, trim=FALSE, state=state)

   close(outcon)
   invisible(state)  # not sure whether I should encourage this
}
