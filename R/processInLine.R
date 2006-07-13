processInLine <- function(x)
{
   justXml <- unlist(strsplit(x, "\\\\Sexpr\\{([^\\}]*)\\}"))   
   # remove all the non-Sweave tag text
   noInLine <- x
   for(i in justXml) noInLine <- gsub(i, "", noInLine, fixed = TRUE)
   
   # split in case there are 2+ tags on the same line
   inLineContents <- unlist(strsplit(noInLine, "\\\\Sexpr"))
   inLineContents <- inLineContents[inLineContents != ""]
   
   # now this contains the in-line tag(s)
   finalInLine <- odfTranslate(stripXmlTag(inLineContents))
   
   # now resub these back into the original xml
   finalXml <- x
   for(i in seq(along = inLineContents)) finalXml <- gsub(inLineContents[i], finalInLine[i], finalXml, fixed = TRUE)
   finalXml
}
