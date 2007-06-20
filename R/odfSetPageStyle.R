odfSetPageStyle <- function(style="Standard")
{
   x <- list(text=sprintf('<odfWeave:pageBreak style="%s"/>', escape(style)))
   return(structure(x, class='odfPageBreak'))
}
