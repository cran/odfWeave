odfPageBreak <- function()
{
   x <- list(text='<odfWeave:pageBreak/>')
   return(structure(x, class='odfPageBreak'))
}

print.odfPageBreak <- function(x, ...)
{
   cat(x$text, '\n', sep='')
}
