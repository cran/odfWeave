stripXmlTag <- function(x, leadWhite = FALSE, endWhite = FALSE)
{
   if(!is.character(x)) stop("x must be character")
   if(!is.vector(x)) stop("x must be a vector")
   # annihilate anything between a < and a >
   x <- gsub("<[^>]*>", "", x)
   if(!leadWhite) x <- sub("^[ \t]+", "", x)  
   if(!endWhite) x <- sub("[ \t]+$", "", x)  
   x
}

