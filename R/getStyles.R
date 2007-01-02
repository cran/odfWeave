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

getImageDefs <- function() get("imageDefs", envir = .odfEnv)




setImageDefs <- 
function (def, verbose = TRUE) 
{
    if (!(all(names(def) %in% c("type", "device", "plotHeight", 
        "plotWidth", "dispHeight", "dispWidth", "args")))) 
        stop("arguments were included. see ?setImageDefs")
    if (!is.null(def$args)) {
        if (!is.list(def$args)) 
            stop("args must be a list")
        if (any(names(def$args) %in% c("width", "height"))) 
            stop("these options should be specified without using the args list")
    }
    if (def$device %in% c("pdf", "svg")) 
        stop("pdf and svg formats not supported by OpenOffice")
    if (def$device %in% c("png", "bmp", "jpeg")) {
        if (def$device == "png" & !capabilities("png")) 
            stop("R png device not enabled")
        if (def$device %in% c("bmp", "jpeg") & !capabilities("jpeg")) 
            stop(paste("R", def$device, "device not enabled"))
    }
    if (def$device == "postscript" & verbose) {
        psArgs <- names(def$args) %in% c("horizontal", "onefile", 
            "paper")
        psNote <- paste("you will probabiliy need to set", "\nhorizontal = FALSE, onefile = FALSE,", 
            "and paper = \"special\" to", "generate ps graphics for OpenOffice\n")
        if (length(psArgs) == 0 | any(!psArgs)) 
            cat(psNote)
    }
    current <- getImageDefs()
    for (i in names(current)) 
    {
      if(i != "args")
      {    
         current[[i]] <- def[[i]]
      } else {
         argNames <- c(names(def[[i]]), names(current[[i]]))
         for(j in argNames)
         {
            if (!is.null(def[[i]][[j]]) && current[[i]][[j]] != def[[i]][[j]]) current[[i]][[j]] <- def[[i]][[j]]
         }      
      }
  
    }
    if (current$device %in% c("png", "bmp", "jpeg") & verbose) {
        if (current$plotHeight <= 30 | current$plotWidth <= 30) 
            cat(paste("an image size of", current$plotHeight, 
                "pixels by", current$plotWidth, "pixels has been requested.\n"))
    }
    else {
        if (current$plotHeight >= 30 | current$plotWidth >= 30) 
            cat(paste("an image size of", current$plotHeight, 
                "inches by", current$plotWidth, "inches has been requested.\n"))
    }
    flush.console()
    assign("imageDefs", current, env = .odfEnv)
}
