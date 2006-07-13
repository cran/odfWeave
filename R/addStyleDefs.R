addStyleDefs <- function(x, style, dest = "styles", verbose = TRUE)
{
   # we need to add our style info between <office:styles> tags
   # which may not exist
   startTag <- if(dest == "styles") "<office:styles>" else "<office:automatic-styles>"
   emptyTag <- gsub(">", "/>", startTag)
   endTag <- gsub("<", "</", startTag)
   if(verbose) cat(paste("\n  Looking for style information in " , dest, ".xml\n", sep = "")); flush.console()           
   
   emptyTagIndex <- grep(emptyTag, x)
   if(length(emptyTagIndex) > 0 )
   {
      part1 <- x[1:(emptyTagIndex - 1)]
      part2 <- x[(emptyTagIndex+1):length(x)]
      x <- c(part1, startTag, endTag, part2)
   } 
   locateAutoStart <- grep(startTag, x, fixed = TRUE)
   locateAutoStop <- grep(endTag, x, fixed = TRUE)
   if(length(locateAutoStart) > 1 & length(locateAutoStop) > 1)
      stop(paste("there are more that one ", startTag, "tags"))

   if(verbose) cat(paste("  Splitting file around", startTag, "at line", locateAutoStop, "\n")); flush.console()           
   part1 <- x[1:(locateAutoStop - 1)] # xml before tag
   part2 <- x[locateAutoStop:length(x)] # xml after tag
   styleVec <- odfStyleGen(style, type = dest)            
   if(verbose) cat("\n")
  
   out <- if(dest == "styles") c(paste(part1, "\n"), styleVec, paste(part2, "\n"))  else c(part1, styleVec, part2)
   out
}

