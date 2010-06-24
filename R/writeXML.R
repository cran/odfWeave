# This writes an XML file using the specified top level node
writeXML <- function(node, file=stdout())
{
   if (is.character(file))
   {
      file <- file(file, open='w')
      on.exit(close(file))
   }

   writeXML.recurse <- function(node)
   {
      if (inherits(node, 'XMLTextNode'))
      {
         if (!is.null(node$raw))
         {
            cat(xmlValue(node), file=file)
         } else {
            cat(escape(xmlValue(node)), file=file)
         }
      } else {
         cat(sprintf('<%s', xmlName(node, full=TRUE)), file=file)
         atts <- xmlAttrs(node)

         # This won't be necessary for "minixml" package
         ns <- xmlNamespaceDefinitions(node)
         if (! is.null(ns) && length(ns) > 0)
         {
            xatts <- sapply(ns, function(n) n$uri)
            nms <- names(xatts)
            if (! is.null(nms) && length(nms) > 0) {
               names(xatts) <- paste('xmlns:', nms, sep='')
               atts <- c(xatts, atts)
            }
         }

         cat(genXMLAttributes(atts), file=file)

         if (xmlSize(node) == 0)
         {
            cat('/>', file=file)
         } else {
            cat('>', file=file)
            for (child in xmlChildren(node))
            {
               writeXML.recurse(child)
            }
            cat(sprintf('</%s>', xmlName(node, full=TRUE)), file=file)
         }
      }
   }

   cat('<?xml version="1.0" encoding="UTF-8"?>\n', file=file)
   writeXML.recurse(node)
   cat('\n', file=file)
}
