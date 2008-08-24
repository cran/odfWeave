"odfTable.matrix" <-
function(
   x,
   colnames    = NULL,  # allow alt col names (what about dimnames[[1]]?)
   useRowNames = TRUE,  # discard or not
   digits      = max(3, getOption("digits") - 3),
   name = paste("Table", floor(runif(1) * 1000), sep = ""),
   styles = NULL,
   ...)
{
   if(!is.null(colnames)) colnames <- odfTranslate(colnames, toR = FALSE)
   colTypes <- apply(x, 2, odfDataType)

   theDots <- list(...)
   if(!any(names(theDots) == "justify")) theDots$justify <- "none"
   if(!any(names(theDots) == "trim")) theDots$trim <- TRUE
   
   args <- list(x = x, digits = digits)
   args <- c(args, theDots)
   xChar <- as.matrix(do.call("format", args))
  
   if(useRowNames & !is.null(rownames(x)))
   {
      xChar <- cbind(rownames(x), xChar)
      colTypes <- c("string", colTypes)
      if(!is.null(rownames(x))) colnames(xChar)<- c("", dimnames(x)[[2]])
   }

   if(!is.null(colnames) && length(colnames) != dim(xChar)[2])
      stop("wrong length of column names")
   if(!is.null(colnames)) dimnames(xChar)[[2]] <- colnames

   if(is.null(styles))    styles <- tableStyles(xChar, useRowNames = FALSE, dimnames(xChar)[[2]])

   tbleText <- odfTableGen(xChar, colTypes, header = dimnames(xChar)[[2]], tableName = name, styles)
   structure(tbleText, class = "odfTable")
}

