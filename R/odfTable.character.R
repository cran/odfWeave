"odfTable.character" <-
function(
   x, 
   horizontal = length(x) < 5,
   colnames    = names(x),  # allow alt col names (what about dimnames[[1]]?)
   name = paste("Table", floor(runif(1) * 1000), sep = ""),
   styles = NULL,
   ...)
{
   xMat <- if(horizontal) as.matrix(t(x)) else as.matrix(x)
   colTypes <- apply(xMat, 2, odfDataType)   
   xChar <- format(xMat, ...)

   if(is.null(styles))    styles <- tableStyles(xChar, colnames)
   
   tbleText <- odfTableGen(xChar, colTypes, header = colnames, tableName = name, styles)
   structure(tbleText, class = "odfTable")     
}


"odfTable.factor" <-
function(
   x, 
   horizontal = length(x) < 5,
   colnames    = names(x),  # allow alt col names (what about dimnames[[1]]?)
   name = paste("Table", floor(runif(1) * 1000), sep = ""),
   styles = NULL,
   ...)
{
   odfTable.character(as.character(x), horizontal = horizontal, colnames = colnames, name = name, styles = styles, ...)
}

