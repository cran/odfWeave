"addStyleDefs" <- function(x, style, dest = "styles", verbose = TRUE) {
   #input
   #
   #   x
   #      single-element character vector, the contents of a document
   #
   #value: x with inserted styles


   # add font declarations
   x <- addFontDecs(x, verbose)

   # we need to add our style info between <office:styles> tags
   # which may not exist
   startTag <- if(dest == "styles") "<office:styles>" else "<office:automatic-styles>"
   emptyTag <- sub(">", "/>", startTag)
   endTag <- sub("<", "</", startTag)
   if(verbose) cat(paste("\n  Looking for style information in " , dest, ".xml\n", sep = "")); flush.console()
   emptyTagIndex <- regexpr(emptyTag, x)
   if(emptyTagIndex > 0 )
   {
      if (emptyTagIndex == 1) {
         part1=""
      } else {
         part1 <- substr(x, 1, emptyTagIndex -1)
      }
      part2 <- substr(x, emptyTagIndex, nchar(x))
      x <- paste(c(part1, startTag, endTag, part2), collapse="")
   }
   locateAutoStart <- gregexpr(startTag, x, fixed = TRUE)[[1]]
   locateAutoStop <- gregexpr(endTag, x, fixed = TRUE)[[1]]
   if(length(locateAutoStart) > 1 & length(locateAutoStop) > 1)
      stop(paste("there are more that one ", startTag, "tags"))

   if(verbose) cat(paste("  Splitting file around", startTag, "at line", locateAutoStop, "\n")); flush.console()
   part1 <- substr(x, 1, locateAutoStop - 1) # xml before tag
   part2 <- substr(x, locateAutoStop, nchar(x)) # xml after tag
   newstyle <- odfStyleGen(style, type = dest)
   if(verbose) cat("\n")
   out <- paste(c(part1, newstyle, part2), collapse="")
   out
}


