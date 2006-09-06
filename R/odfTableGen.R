"odfTableGen" <-
function(x, dataType, header = NULL, tableName, styles)
{

   tableDim <- dim(x)

   has <- function(x) !is.null(x) && x != ""
   makeMatrix <- function(text, dims) matrix(rep(text,prod(dims)),nrow = dims[1])

   endQuote <- makeMatrix("\" ", dim(styles$cell))


   # build text tags and formatting
   textNameStart <- makeMatrix(" text:style-name=\"", dim(styles$text))
   textNameStart <- ifelse(styles$text == "", "", textNameStart)
   textNameEnd <- ifelse(styles$text == "", "", endQuote)
   textName <- matrixPaste(textNameStart, styles$text, textNameEnd, sep = c("", ""))
   textStart <- makeMatrix("      <text:p", dim(styles$text))
   textEnd <- makeMatrix(">", dim(styles$text))
   tagEnd <- makeMatrix(" </text:p>\n", dim(styles$text))
   textMatrix <- matrixPaste(textStart, textName, textEnd, x, tagEnd)

   # cell properties
   cellNameStart <- makeMatrix(" table:style-name=\"", dim(styles$cell))
   cellNameStart <- ifelse(styles$cell == "", "", cellNameStart)
   cellNameEnd <- ifelse(styles$cell == "", "", endQuote)
   cellName <- matrixPaste(cellNameStart, styles$cell, cellNameEnd, sep = c("", ""))
   valueType <- matrix(rep(dataType, each = tableDim[1]), nrow = tableDim[1])
   valueTypeStart <- makeMatrix(" <table:table-cell office:value-type=\"", dim(styles$cell))
   cellStart <- matrixPaste(valueTypeStart, valueType, endQuote, sep = c("", ""))
   cellEnd <- makeMatrix(">\n", dim(styles$cell))
   tagEnd <- makeMatrix(" </table:table-cell>\n", dim(styles$cell))
   cellMatrix <- matrixPaste(cellStart, cellName, cellEnd, textMatrix, tagEnd)

   # wrap in row tags
   leftRowTags <- matrix(
      c(
         rep("<table:table-row>", tableDim[1]),
         rep("", (tableDim[2] - 1) * tableDim[1])),
      nrow = tableDim[1])
   rightRowTags <- matrix(
      c(
         rep("", (tableDim[2] - 1) * tableDim[1]),
         rep("</table:table-row>\n", tableDim[1])),
      nrow = tableDim[1])
   rowMarkup <- matrixPaste(leftRowTags, cellMatrix, rightRowTags, sep = rep("\n", 2))

   if(has(styles$table)) tableStyle <- paste(" table:style-name=\"", styles$table, "\" ", sep = "") else tableStyle <- ""

   if(!is.null(header))
   {
      cellHeaderStyle <- paste(" table:style-name=\"", styles$headerCell, "\" ", sep = "")
      cellHeaderStyle <- ifelse(styles$headerCell == "", "", cellHeaderStyle)


      textHeaderStyle <- paste(" text:style-name=\"", styles$header, "\" ", sep = "")
      textHeaderStyle <- ifelse(styles$header == "", "", textHeaderStyle)

      headLine01 <- paste("\n      <text:p ", textHeaderStyle, "> ", header, " </text:p>", sep = "")
      headLine02 <- paste(
         "\n    <table:table-cell ",
         cellHeaderStyle,
         "office:value-type=\"string\">",
         headLine01,
         "\n    </table:table-cell>",
         sep = "")
      headLine03 <- paste(
         "\n  <table:table-header-rows>\n    <table:table-row>\n",
         paste(headLine02, collapse = ""),
         "\n    </table:table-row>\n   </table:table-header-rows>\n",
         sep = "")

   } else headLine03 <-  NULL


   startText <- paste(
      "\n<table:table table:name=\"",  tableName, "\" ", tableStyle, ">",
      "\n  <table:table-column ",
      "table:number-columns-repeated=\"", length(dataType), "\"/>",
      sep = "")

   list(
      start = startText,
      header = headLine03,
      cells = rowMarkup,
      end = "\n</table:table>\n")
}
