"addFontDecs" <- function (content, verbose)
{
   #input
   #
   #   content:  character string, contents of a document
   #   verbose:  logical for printing progress
   #
   #value: character string, x with additional font declarations appended.
   #    Fonts listed in odfEnv.R are checked and added, if necessary
   #

   announce(verbose, "  adding font declarations...\n")

   #get styles used in odfEnv
   styles <- getStyleDefs()
   #extract font names from styles
   fonts <- unique(unlist(lapply(styles, function(font) font$fontName)))

   #get font declarations
   #assert:  content only has one such font-face-decls element
   match <- regexpr(
      "(?s)(?U)<office:font-face-decls>.*</office:font-face-decls>",
      content, perl=TRUE)
   matchLength <- attr(match, "match.length")
   fontDecBlock <- substr(content, match, match + matchLength -1)

   #get only fonts which don't have a declaration
   fonts <-fonts[!unlist(lapply(lapply(fonts, grep, fontDecBlock), any))]

   #create a string of font declarations to insert
   fontDecs <- paste(
      unlist(
         lapply(fonts,
            function(font){
               element(
                  "style:font-face",
                  c(
                     tagattr("style:name", font),
                     tagattr("svg:font-family", font)
                  )
               )
            }
         )
      ),
      collapse=""
   )
   #insert font declarations
   sub("</office:font-face-decls>",
      paste(fontDecs, "</office:font-face-decls>", sep=""),
      content,
      fixed=TRUE
   )
}

