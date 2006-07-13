checkLength <- function(x, cutoff = 999)
{
   if(!is.character(x)) stop("x must be character")
   if(!is.vector(x)) stop("x must be a vector")
   
   lineLength <- nchar(x)
   isTooLong <- lineLength >= cutoff
   
   # when code is pasted in, it might put multiple lines
   # of test into one XML entry. If these lines contain
   # code chunks, we should not break them up
   startLines <- grep("&lt;&lt;(.*)&gt;&gt;=", x)
   if(length(startLines) > 0) isTooLong[startLines] <- FALSE
   
   if(any(isTooLong))
   {
      breakPoints <- which(isTooLong)
      
      for(i in seq(along = breakPoints))
      {
         out <- if(i == 1) x[1:(breakPoints[i] - 1)] 
            else c(out, x[(breakPoints[i - 1] + 1):(breakPoints[i] - 1)])
         tmp <- unlist(strsplit(x[breakPoints[i]], " "))
         tmp[2:length(tmp)] <- paste("  ", tmp[2:length(tmp)])
         out <- c(out, tmp)
      }
      out <- c(out, x[(max(breakPoints) + 1):length(x)])
   } else out <- x
   out
}




