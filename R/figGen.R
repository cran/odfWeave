figGen <- function(type, device, plotName = NULL, ...)
{
   
   # make up a random name with prefix "rPlot"
   if(is.null(plotName)) plotName <- paste("rPlot", floor(runif(1) * 10000), ".", type, sep = "")
   
   deviceArgs <- vector(mode = "list", length = 0)
   if(device != "bitmap") deviceArgs$filename <- plotName else deviceArgs$file <- plotName   
   
   if(length(list(...)) > 0) deviceArgs <- c(deviceArgs, list(...))
   
   do.call(device, deviceArgs) 

}
