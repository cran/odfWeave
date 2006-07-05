checkLength <- function(x, cutoff = 999)
{
   if(!is.character(x)) stop("x must be character")
   if(!is.vector(x)) stop("x must be a vector")
   
   lineLength <- nchar(x)
   isTooLong <- lineLength >= cutoff
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

#testing <- letters[1:10]
#testing[3] <- paste(1:10, collapse = " ")
#testing[7] <- paste(1:5, collapse = " ")
#checkLength(testing, 4)


