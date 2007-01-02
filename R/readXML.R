readXML <- function(infile, method = "old", verbose = FALSE) {
   input_get <- function (infile, verbose = TRUE) {
      # readLines is more natural, but shows warnings due to no EOF (OO creates XML wo EOF)
      data <- readBin(infile, what='raw', n=100000, size=1)
      if (length(data)) c(data, input_get(infile))
      else data
   }
   infile <- file(infile, open="rb")
   xmlRaw <- rawToChar(input_get(infile))
   close(infile)
   xmlRaw
}
