"odfTranslate" <-
function(x, toR = TRUE)
{
   if(toR)
   {
      x <- gsub("&gt;", ">", x)
      x <- gsub("&lt;", "<", x)
      x <- gsub("&quot;", "\"", x)
      x <- gsub("&apos;", "\'", x)
      x <- gsub("&amp;", "&", x)  
      x <- gsub("\342\200\223", "-", x)        
      x <- gsub("\342\200\235", "\"", x)  
      x <- gsub("\342\200\234", "\"", x)  
   } else {
      # & must be the first here
      x <- gsub("&","&amp;",  x)       
      x <- gsub(">", "&gt;", x)
      x <- gsub("<", "&lt;",  x)
      x <- gsub("\"", "&quot;",  x)
      x <- gsub( "\'", "&apos;", x)
      
      # ODF puts in a code for areas where there are 2+ characters 
      # of white space (bastards). First, we can figure out all of the lengths of
      # white space characters and convert this to 
      # '<text:s text:c="#"/>'  where # is the length
      spaceCount <- unique(unlist(lapply(strsplit(x, "[^ ]"), function(x) as.numeric(names(table(nchar(x)))))))
      spaceCount <- spaceCount[spaceCount > 1]
      
      for(i in sort(spaceCount, decreasing = TRUE))
      {
         from <- paste(rep(" ", i), collapse = "")
         to <- paste("<text:s text:c=\"", i, "\"/>", sep = "")
         x <- gsub(from, to, x)
      }      
   }
   x
}

