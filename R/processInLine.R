"processInLine" <- function(x, matches) {
   pieces <- tagsGet(x, matches)
   pieces <- odfTranslate(stripXmlTag(pieces))
   subin(x, matches, pieces)
}
