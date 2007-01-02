odfTmpDir <- function()
{
   tmpPath <- tempdir()
   suffix <- paste(
      "odfWeave", 
      format(Sys.time(), "%d%H%M%S"), 
      round(runif(1)*1000, 0),
      sep = "")
   newPath <- paste(tmpPath, "/", suffix, sep = "")
   if(!dir.create(newPath))
      stop(paste(
         "could not create",
         newPath))
   newPath
}

