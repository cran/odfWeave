odfWeaveControl <- function(
   zipCmd = c("zip -r $$file$$ .", "unzip -o $$file$$"),
   cleanup = FALSE,
   verbose = TRUE,
   plotType = "png",
   plotDevice = ifelse(.Platform$OS.type == "windows", "png", "bitmap"),
   plotHeight = ifelse(plotType %in% c("jpg", "png", "gif", "bmp"), 480, 6),
   plotWidth = ifelse(plotType %in% c("jpg", "png", "gif", "bmp"), 480, 6),
   dispHeight = 4.5,
   dispWidth = 4.5)
{
	# pass ... args to figGen here?
   list(
      zipCmd = zipCmd,
      cleanup = cleanup,
      verbose = verbose,
      plotType = plotType,
      plotDevice = plotDevice,
      plotHeight = plotHeight,
      plotWidth = plotWidth,
      dispHeight = dispHeight,
      dispWidth = dispWidth)
}

