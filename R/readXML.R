
readXML <- function(x, method = "old", verbose = TRUE)
{
   x <- file(x, "rb")
   if(method == "new")
	   {
	   xmlRaw <- scan(x, what = "raw", sep = "\n",  strip.white = TRUE, quiet = !TRUE)
	   xmlData <- unlist(strsplit(xmlRaw, "><"))
	   close(x)
	   #some xml has two bit strings as the first characters, so we split off the first line
	   xmlStart <- xmlData[1]
	   xmlData <- xmlData[-1]
	
	   xmlData <- ifelse(substr(xmlData, 1, 1) != "<", paste("<", xmlData, sep = ""), xmlData)
	   xmlData <- ifelse(substr(xmlData, nchar(xmlData), nchar(xmlData)) != ">", paste(xmlData, ">", sep = ""), xmlData)
	   
	   out <- c(xmlStart, xmlData)
   } else {
     out <- readLines(x)
   }
   close(x)
   out
}

