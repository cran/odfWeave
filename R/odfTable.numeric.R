"odfTable.numeric" <-
function(
   x,
   horizontal = length(x) < 5,
   colnames    = names(x),  # allow alt col names (what about dimnames[[1]]?)
   digits      = max(3, getOption("digits") - 3),
   name = paste("Table", floor(runif(1) * 1000), sep = ""),
   styles = NULL,
   ...)
{
   if(!is.null(colnames)) colnames <- odfTranslate(colnames, toR = FALSE)  
   xMat <- if(horizontal) as.matrix(t(x)) else as.matrix(x)
   colTypes <- apply(xMat, 2, odfDataType)
   xChar <- format(xMat, digits = digits, ...)

   if(is.null(styles))    styles <- tableStyles(xChar, colnames)

   tbleText <- odfTableGen(xChar, colTypes, header = colnames, tableName = name, styles)
   structure(tbleText, class = "odfTable")
}

