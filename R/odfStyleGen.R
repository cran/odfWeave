"odfStyleGen" <- function(x, type = "styles")
{
   out <- vector(mode = "character", length = 0)
   if(is.null(x)) return(out)
   
   has <- function(x) !is.null(x) && x != ""
     
   styles <-  unlist(lapply(x, function(x) x$type))
   
   if(type == "styles")
   {
      paraStyles <- styles[styles == "Paragraph"]
      for(i in seq(along = paraStyles))
      { 
         thisStyle <- x[[ names(paraStyles)[i] ]]
         if(has(thisStyle$fontType))
         {
            fontText <- ""
            if(length(grep("bold", thisStyle$fontType)) == 1)
               fontText <- c(fontText, "fo:font-weight=\"bold\"")
            if(length(grep("italic", thisStyle$fontType)) == 1)
               fontText <- c(fontText, "fo:font-style=\"italic\"")      
            if(length(grep("underline", thisStyle$fontType)) == 1)
               fontText <- c(
                  fontText, 
                  "style:text-underline-style=\"solid\"",
                  "style:text-underline-width=\"auto\"",
                  "style:text-underline-color=\"font-color\"")
            if(length(grep("shadow", thisStyle$fontType)) == 1)
               fontText <- c(fontText, "fo:text-shadow=\"1pt 1pt\"")     
            if(length(grep("superscript", thisStyle$fontType)) == 1)
               fontText <- c(fontText, "style:text-position=\"super 58%\"")           
            if(length(grep("subscript", thisStyle$fontType)) == 1)
               fontText <- c(fontText, "style:text-position=\"sub 58%\"")                 
         } else fontText <- ""                  
         
         paraFormat <- paste(
            "  <style:style style:name=\"",
            names(paraStyles)[i],
            "\" style:family=\"paragraph\"",
            ifelse(has(thisStyle$parentStyleName),
               paste(" style:parent-style-name=\"", thisStyle$parentStyleName,"\"", sep = ""),
               ""),         
            ">\n",
            "   <style:text-properties ", 
            ifelse(has(thisStyle$fontColor),
               paste(" fo:color=\"", thisStyle$fontColor,"\"", sep = ""),
               ""),    
            ifelse(has(thisStyle$fontSize),
               paste(" fo:font-size=\"", thisStyle$fontSize,"\"", sep = ""),
               ""),               
            paste(fontText, collapse = " "),     
            ifelse(has(thisStyle$fontName),
               paste(" style:font-name=\"", thisStyle$fontName,"\"", sep = ""),
               ""),    
            "/>",
            ifelse(has(thisStyle$textAlign),
               paste("\n   <style:paragraph-properties  fo:text-align=\"", thisStyle$textAlign,"\"/>", sep = ""),
               ""),
            "\n  </style:style>\n",         
            sep = "")      
         out <- c(out, paraFormat)
      }
           
      tableStyles <- styles[styles == "Table"]
      for(i in seq(along = tableStyles))
      {
         thisStyle <- x[[ names(tableStyles)[i] ]]
         tableFormat <- paste(
            "  <style:style style:name=\"",
            names(tableStyles)[i],
            "\" style:family=\"table\">\n   <style:table-properties ", 
            ifelse(has(thisStyle$marginLeft),
               paste(" fo:margin-left=\"", thisStyle$marginLeft,"\"", sep = ""),
               ""),
            ifelse(has(thisStyle$marginRight),
               paste(" fo:margin-right=\"", thisStyle$marginLeft,"\"", sep = ""),
               ""),
            ifelse(has(thisStyle$marginTop),
               paste(" fo:margin-top=\"", thisStyle$marginTop,"\"", sep = ""),
               ""),
            ifelse(has(thisStyle$marginBottom),
               paste(" fo:margin-bottom=\"", thisStyle$marginBottom,"\"", sep = ""),
               ""), 
            " />\n  </style:style>\n",
            sep = "")
         out <- c(out, tableFormat)          
      }    
      
      bulletStyles <- styles[styles == "Bullet List"]
      for(i in seq(along = bulletStyles))
      {
         thisStyle <- x[[ names(bulletStyles)[i] ]]
         bulletListFormat <- paste(
            "  <text:list-style style:name=\"",
            names(bulletStyles)[i],
            "\">\n   <text:list-level-style-bullet ",
            ifelse(has(thisStyle$level),
               paste(" text:level=\"", thisStyle$level,"\"", sep = ""),
               ""),         
            ifelse(has(thisStyle$styleName),
               paste(" text:style-name=\"", thisStyle$styleName,"\"", sep = ""),
               ""),                     
            ifelse(has(thisStyle$bulletChar),
               paste(" text:bullet-char=\"", thisStyle$bulletChar,"\"", sep = ""),
               ""),
            ">\n    <style:list-level-properties ",
            
            ifelse(has(thisStyle$spaceBefore),
               paste(" text:space-before=\"", thisStyle$spaceBefore,"\"", sep = ""),
               ""),
            ifelse(has(thisStyle$minLabelWidth),
               paste(" text:min-label-width=\"", thisStyle$minLabelWidth,"\"", sep = ""),
               ""),
            "/>\n    <style:text-properties ",
            ifelse(has(thisStyle$fontName),
               paste(" style:font-name=\"", thisStyle$fontName,"\"", sep = ""),
               ""), 
            " />\n   </text:list-level-style-bullet>\n  </text:list-style>\n",
            sep = "")
         out <- c(out, bulletListFormat)          
      }
   }  else {
      cellStyles <- styles[styles == "Table Cell"]
      for(i in seq(along = cellStyles))
      {
         thisStyle <- x[[ names(cellStyles)[i] ]]
         
         cellTextFormat <- paste(
            "  <style:style style:name=\"",
            names(cellStyles)[i],
            "\" style:family=\"table-cell\"",
            ">\n",
            "   <style:table-cell-properties ", 
            ifelse(has(thisStyle$verticalAlign),
               paste(" style:vertical-align=\"", thisStyle$verticalAlign,"\"", sep = ""),
               ""),            
            ifelse(has(thisStyle$leftBorder),
               paste(" fo:border-left=\"", thisStyle$leftBorder,"\"", sep = ""),
               ""), 
            ifelse(has(thisStyle$rightBorder),
               paste(" fo:border-right=\"", thisStyle$rightBorder,"\"", sep = ""),
               ""), 
            ifelse(has(thisStyle$topBorder),
               paste(" fo:border-top=\"", thisStyle$topBorder,"\"", sep = ""),
               ""), 
            ifelse(has(thisStyle$bottomBorder),
               paste(" fo:border-bottom=\"", thisStyle$bottomBorder,"\"", sep = ""),
               ""),     
            ifelse(has(thisStyle$padding),
               paste(" fo:padding=\"", thisStyle$padding,"\"", sep = ""),
               ""),                 
                                            
            "/>\n  </style:style>\n",         
            sep = "")      
         out <- c(out, cellTextFormat)
      }   
   }
      out
}


