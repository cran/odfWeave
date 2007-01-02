#TODO:  rename file to tagTranslate
stripXmlTag <- function(x, leadWhite = FALSE, endWhite = FALSE)
{
   if(!is.character(x)) stop("x must be character")
   if(!is.vector(x)) stop("x must be a vector")
   #newlines are only whitespace in the opendocument format
   x <- gsub("\n", " ", x)

   # "newline" tags
   x <- gsub("<text:line-break[^>]*?>", "\n", x)
   x <- gsub("<text:p[^>]*?>", "\n", x, perl=TRUE)

   # annihilate all other xml tags
   x <- gsub("<[^>]*?>", "", x, perl=TRUE)
   if(!leadWhite) x <- sub("^[ \t]+", "", x)
   if(!endWhite) x <- sub("[ \t]+$", "", x)
   x
}

