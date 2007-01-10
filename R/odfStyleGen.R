"tagattr" <- function(name, val) paste(c(name, '="', val, '"'), collapse="")
"element" <- function(tag, atts, content="") {
   #tag:  character string, name the element
   #atts: character, attributes of a tag in the form name="value"
	#   empty items are discarded 
   #content: character string, text of an element
   #value: character string, and properly-formed element
   atts <- paste(atts[atts!=""], collapse=" ")
   if (all(content == "")) {
      close <- "/>"
      endtag <- ""
   } else {
      close <- ">"
      endtag <- paste(c('</', tag, '>'), collapse="")
   }
   paste(c('<', tag, ' ', atts, close, content, endtag), collapse="" )
}
"odfStyleGen" <- function(x, type = "styles") {
   out <- ""
   if(is.null(x)) return(out)
   "has" <- function(x) !is.null(x) && x != ""
   styles <-  unlist(lapply(x, function(x) x$type))
   if(type == "styles") {
      paraStyles <- styles[styles == "Paragraph"]
      for(i in seq(along = paraStyles)) {
         thisStyle <- x[[names(paraStyles)[i]]]

         if(has(thisStyle$fontType)) {
            fontText <- ""
            if(length(grep("bold", thisStyle$fontType)))
               fontText <- c(fontText, tagattr("fo:font-weight", "bold"))
            if(length(grep("italic", thisStyle$fontType)))
               fontText <- c(fontText, tagattr("fo:font-style", "italic"))
            if(length(grep("underline", thisStyle$fontType)))
               fontText <- c(
                  fontText,
                  tagattr("style:text-underline-style", "solid"),
                  tagattr("style:text-underline-width", "auto"),
                  tagattr("style:text-underline-color", "font-color"))
            if(length(grep("shadow", thisStyle$fontType)) == 1)
               fontText <- c(fontText, tagattr("fo:text-shadow", "1pt 1pt"))
            if(length(grep("superscript", thisStyle$fontType)) == 1)
               fontText <- c(
                  fontText, tagattr("style:text-position", "super 58%"))
            if(length(grep("subscript", thisStyle$fontType)) == 1)
               fontText <- c(
                  fontText, tagattr("style:text-position", "sub 58%"))
         } else fontText <- ""

         style_style <- 'style:style'
         style_style_attr <- c(
            tagattr('style:name', paste(names(paraStyles)[i], collapse=" ")),
            tagattr('style:family', 'paragraph'),
            if(has(thisStyle$parentStyleName))
               tagattr('style:parent-style-name', thisStyle$parentStyleName)
         )
         style_text <- 'style:text-properties'
         style_text_attr <- c(
            if(has(thisStyle$fontColor))
               tagattr('fo:color', thisStyle$fontColor),
            if(has(thisStyle$fontSize))
               tagattr('fo:font-size', thisStyle$fontSize),
            fontText,
            if(has(thisStyle$fontName))
               tagattr('style:font-name', thisStyle$fontName))
         style_paragraph <- 'style:paragraph-properties'
         style_paragraph_attr <- c(
            tagattr('fo:text-align', thisStyle$textAlign))
         out <- paste(
            c(
            out,
            element(style_style, style_style_attr,
               c(
                  element(
                     style_text,
                     style_text_attr
                  ),
                  if(has(thisStyle$textAlign))
                     element(style_paragraph, style_paragraph_attr)
                  )
               )
            ),
            collapse=""
         )
      }

      bulletStyles <- styles[styles == "Bullet List"]
      for(i in seq(along = bulletStyles))
      {
         thisStyle <- x[[(bulletStyles)[i]]]
         list_style <- 'text:list-style'
         list_style_attr <- c(
            tagattr("style:name", paste(names(bulletStyles)[i], collapse=" "))
         )
         list_level <- "text:list-level-style-bullet"
         list_level_attr <- c(
            if(has(thisStyle$level))
               tagattr("text:level", thisStyle$level),
            if(has(thisStyle$styleName))
               tagattr("text:style-name", thisStyle$styleName),
            if(has(thisStyle$bulletChar))
               tagattr("text:bullet-char", thisStyle$bulletChar)
         )
         list_level_properties <- 'style:list-level-properties'
         list_level_properties_attr <- c(
            if(has(thisStyle$spaceBefore))
               tagattr("text:space-before", thisStyle$spaceBefore),
            if(has(thisStyle$minLabelWidth))
               tagattr("text:min-label-width", thisStyle$minLabelWidth)
         )
         style_text_properties <- 'style:text-properties'
         style_text_properties_attr <- c(
            if(has(thisStyle$fontName))
               tagattr("style:font-name", thisStyle$fontName))
         out <- paste(
            c(
               out,
               element(list_style, list_style_attr,
                  element(list_level, list_level_attr,
                     c(
                        element(list_level_properties,
                           list_level_properties_attr),
                        element(style_text_properties,
                           style_text_properties_attr))))
            ),
            collapse="")
      }
   } else {
   

      tableStyles <- styles[styles == "Table"]
      for(i in seq(along = tableStyles)) {
         thisStyle <- x[[names(tableStyles)[i]]]
         style_style <- 'style:style'
         style_style_attr <- c(
            tagattr("style:name", paste(names(tableStyles)[i], collapse=" ")),
            tagattr("style:family", "table"))
         table_properties <- 'style:table-properties'
         table_properties_attr <- c(
            if(has(thisStyle$marginLeft))
               tagattr("fo:margin-left", thisStyle$marginLeft),
            if(has(thisStyle$marginRight))
               tagattr("fo:margin-right", thisStyle$marginRight),
            if(has(thisStyle$marginTop))
               tagattr("fo:margin-top", thisStyle$marginTop),
            if(has(thisStyle$marginBottom))
               tagattr("fo:margin-bottom", thisStyle$marginBottom),
            if(has(thisStyle$align))
               tagattr("table:align", thisStyle$align))
         out <- paste(
            c(
               out,
               element(style_style, style_style_attr,
                  element(table_properties, table_properties_attr))
            ),
            collapse="")
      }
   
      cellStyles <- styles[styles == "Table Cell"]
      for(i in seq(along = cellStyles))
      {
         thisStyle <- x[[names((cellStyles)[i])]]
         style_style <- "style:style"
         style_style_attr <- c(
            tagattr("style:name", paste(names(cellStyles)[i], collapse=" ")),
            tagattr("style:family", "table-cell"))
         table_cell <- "style:table-cell-properties"
         table_cell_attr <- c(
            if(has(thisStyle$verticalAlign))
               tagattr("style:vertical-align", thisStyle$verticalAlign),
            if(has(thisStyle$leftBorder))
               tagattr("fo:border-left", thisStyle$leftBorder),
            if(has(thisStyle$rightBorder))
               tagattr("fo:border-right", thisStyle$rightBorder),
            if(has(thisStyle$topBorder))
               tagattr("fo:border-top", thisStyle$topBorder),
            if(has(thisStyle$bottomBorder))
               tagattr("fo:border-bottom", thisStyle$bottomBorder),
            if(has(thisStyle$padding))
               tagattr("fo:padding", thisStyle$padding),
            if(has(thisStyle$backgroundColor))
               tagattr("fo:background-color", thisStyle$backgroundColor)               
         )
         out <- paste(
            c(
               out,
               element(
                  style_style,
                  style_style_attr,
                  element(
                     table_cell,
                     table_cell_attr))),
            collapse="")
      }
   }
   out
}

