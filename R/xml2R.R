xml2R <- function(x)
{
   if(length(grep("<text:line-break/>", x)) >= 1) x <- unlist(strsplit(x, "<text:line-break/>"))
   tmpString <- stripXmlTag(x)
   tmp <- odfTranslate(tmpString)
   tmp
}
