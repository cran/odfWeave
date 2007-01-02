"odfInsertPlot" <-
function(file, height, width,
   units = "in",
   anchor = c("<text:p>", "</text:p>"),
   name = paste("graphics", floor(runif(1) * 1000), sep = ""),
   externalFile = FALSE,
   dest = paste(getwd(), "/Pictures", sep = ""))
{

   if(getExt(file) %in% c("pdf")) stop("graphics format not supported")

   plotString <- c(
      "    <draw:frame ",
      paste("      draw:name=\"", name, "\"", sep = ""),
      "      text:anchor-type=\"paragraph\"",
      paste("      svg:width=\"", width, units, "\"", sep = ""),
      paste("      svg:height=\"", height, units, "\"", sep = ""),
      "      draw:z-index=\"0\">",
      "      <draw:image",
      paste("         xlink:href=\"Pictures/", basename(file), "\"", sep = ""),
      "         xlink:type=\"simple\"",
      "         xlink:show=\"embed\"",
      "         xlink:actuate=\"onLoad\"/>",
      "    </draw:frame>")

   out <- c(anchor[1], plotString, anchor[2])
   if(externalFile)
   {
      out <- paste(out, collapse = "\n")
      newPath <- paste(dest, "/", basename(file), sep = "")

      if(!file.exists(dest)) stop(paste(dest, "does not exist"))
      if(!file.copy(file, newPath,  overwrite = TRUE)) stop("Error copying file")
   }
   out
}

