"odfItemize" <-
function(data)
{
   if(!is.vector(data)) stop("data must be a vector")
   styles <- getStyles()
   has <- function(x) !is.null(x) && x != ""

   if(has(styles$bullet)) listStyle <- paste(" text:style-name=\"", styles$bullet, "\" ", sep = "")
      else listStyle <- ""
   if(has(styles$paragraph)) textStyle <- paste(" text:style-name=\"", styles$paragraph, "\" ", sep = "")
      else textStyle <- ""

   x <- paste(
      "    <text:list-item>\n",
      "      <text:p ",
      textStyle,
      ">",
      data,
      "</text:p>\n",
      "    </text:list-item>\n",
      sep = "")

   out <- paste(
      "\n  <text:list ",
      listStyle,
      ">\n",
      paste(x, collapse = ""),
      "  </text:list>\n",
      sep = "")
   structure(out, class = "odfItemize")
}

print.odfItemize <- function(x, ...) cat(x)

