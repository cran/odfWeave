# event handler functions

stylesStartElement <- function(name, atts, .state)
{
   if (!is.null(.state$buffer))
      openTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)
   .state$buffer <- list(name, atts)
   .state
}

stylesText <- function(x, .state)
{
   if (!is.null(.state$buffer))
      openTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)
   processText(x, .state$outfile)
   .state$buffer <- NULL
   .state
}

stylesEndElement <- function(name, .state)
{

   if (name == 'office:styles')
   {
      # add the styles that we need
      if (!is.null(.state$buffer))
         openTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)
      cat(odfStyleGen(getStyleDefs(), 'styles'),
         file=.state$outfile)
      closeTag(name, .state$outfile)
   } else if (name == 'office:automatic-styles') {
      if (!is.null(.state$buffer))
         openTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)
      cat(odfStyleGen(getStyleDefs(), 'page'),
         file=.state$outfile)
      closeTag(name, .state$outfile)
   } else if (name == 'office:master-styles') {
      if (!is.null(.state$buffer))
         openTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)
      # add a style:master-page element for each style:page-layout element
      # that we added to the automatic styles
      cat(odfStyleGen(getStyleDefs(), 'master'),
         file=.state$outfile)
      closeTag(name, .state$outfile)
   } else {
      if (!is.null(.state$buffer))
         completeTag(.state$buffer[[1]], .state$buffer[[2]], .state$outfile)
      else
         closeTag(name, .state$outfile)
   }
   .state$buffer <- NULL
   .state
}

# this is the main function that adds styles to the styles.xml file
# of the output ODF file

procstyles <- function(infile, outfile)
{
   outcon <- file(outfile, open='w')

   state <- list(buffer=NULL, outfile=outcon)
   handlers <- list(startElement=stylesStartElement,
                    endElement=stylesEndElement,
                    text=stylesText,
                    startDocument=startDocument,
                    endDocument=endDocument)
   state <- xmlEventParse(infile, handlers=handlers, trim=FALSE, state=state)

   close(outcon)
   invisible(state)  # not sure whether I should encourage this
}
