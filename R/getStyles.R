
getStyles <- function() get("odfStyles", envir = .odfEnv)

setStyles <- function(style)
{
   styleNames <- names(style)
   required <- c("paragraph", "input", "output", "table", "cell", "header", "cellText", "headerText")
   hasTypes <- required %in% styleNames
   
   if(any(!hasTypes))
   {
      missingList <- vector(mode = "list", length = sum(!hasTypes))
      names(missingList) <- required[!hasTypes]
      missingList <- lapply(missingList, function(x) "")
      style <- c(style, missingList)
   }
   
#   if(any(!hasTypes)) 
#      stop(
#         cat("There must be styles for:",
#            paste(required, collapse = ", "), "\n"))
#   typeCounts <- table(styleNames)
#   if(any(typeCounts > 1)) stop("only one style name can be declared here")
   assign( "odfStyles",  style, env = .odfEnv)
}


getStyleDefs <- function() get("styleDefs", envir = .odfEnv)

setStyleDefs <- function(def)
{
   styleTypes <- unique(unlist(lapply(def, function(x) x$type)))
   required <- c("Paragraph", "Table Cell", "Table", "Bullet List")
   hasTypes <- required %in% styleTypes
   if(any(!hasTypes)) 
      stop(
         cat("There must be at least one style definintion for:",
            paste(required, collapse = ", "), "\n"))
   assign( "styleDefs",  def, env = .odfEnv)
}


