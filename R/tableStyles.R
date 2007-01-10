tableStyles <- function(x, useRowNames = TRUE, header = NULL)
{

   styles <- getStyles()
   tableDim <- dim(x)
   if(useRowNames) tableDim[2] <- tableDim[2] + 1
   
   has <- function(x) !is.null(x) && x != ""


   textName <- ifelse(has(styles$cellText), styles$cellText, "")
   cellName <- ifelse(has(styles$cell), styles$cell, "")

   textForm <- if(has(styles$cellText)) matrix(rep(styles$cellText, tableDim[1] * tableDim[2]), nrow = tableDim[1]) else NULL
   cellForm <- if(has(styles$cell)) matrix(rep(styles$cell, tableDim[1] * tableDim[2]), nrow = tableDim[1]) else NULL

   if(!is.null(header))
   {
      headerText <- if(has(styles$headerText)) matrix(rep(styles$headerText, tableDim[2]), nrow = 1) else NULL
      headerCell <- if(has(styles$header)) matrix(rep(styles$header, tableDim[2]), nrow = 1) else NULL


   } else headerText <- headerCell <- NULL

   list(table = styles$table, text = textForm, cell = cellForm, header = headerText, headerCell = headerCell)
}

