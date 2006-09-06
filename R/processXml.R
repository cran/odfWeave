# R code in ODF files get wrapped in xml and some of the characters
# may be changed from what the user types in.
# This function processes the xml files and writes out Rnw files
# (hopefully) consistent with Sweave's expectations.

"processXml" <- function(x, matches) {

   # for in-line Sweave tags, make sure that any converted character
   # are fixed prior to the Sweave call

   sexprmatches <- getByAttR(matches, "match.type", "sexpr")[[1]]
   if(tagsExist(sexprmatches))
      x <- processInLine(x, sexprmatches)
   # process lines with options. In this case, we will
   # nuke everything else on the line

   matches <- tagsIdxs(x)
   optmatches <- getByAttR(matches, "match.type", "option")[[1]]
   if(tagsExist(optmatches)){
      pieces <- odfTranslate(stripXmlTag(tagsGet(x, optmatches)))
      x <- subin(x, optmatches, pieces)
   }

   # search thru for lines with <<>>=
   matches <- tagsIdxs(x)
   chunkmatches <- getByAttR(matches, "match.type", "chunk")[[1]]
   if(tagsExist(chunkmatches)){
      pieces <- odfTranslate(stripXmlTag(tagsGet(x, chunkmatches)))
      pieces <- sub("^", "\n", pieces)
      pieces <- sub("$", "\n", pieces)
      pieces <- gsub("\n *\n", "\n", pieces)
      x <- subin(x, chunkmatches, pieces)
   }

   #add newline at end
   #this seems to be useless
   x <- gsub("\n*$", "\n\n\000", x)

   x
}
