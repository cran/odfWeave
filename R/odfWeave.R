"odfWeave" <- 
function(file, dest, workDir=odfTmpDir(), control=odfWeaveControl())
{ 
   #configure
   currentLoc <- getwd()
   zipCmd <- control$zipCmd
   zipCmd <- gsub("$$file$$", shellQuote(basename(file)), zipCmd, fixed=TRUE)
   
   currentLocale <- c(Sys.getlocale("LC_CTYPE"), Sys.getlocale("LC_COLLATE"))
   Sys.setlocale("LC_CTYPE", "C")
   Sys.setlocale("LC_COLLATE", "C")

   if(dirname(dest) == ".") dest <- paste(currentLoc, "/", dest, sep = "")
   
   verbose <- control$verbose

   #check for an unzipping utility
   if(all(zipCmd == c("zip -r $$file$$ .", "unzip -o $$file$$")))
   {
      errorText <- paste(
         "unzip could not be found.",
         "If installed, check your path.",
         "If not installed, either go to",
         "www.info-zip.org and install or",
         "use another utility (like jar)")
      if(.Platform$OS.type == "windows")
      {
         zipTest <- class(try(system("unzip", intern=TRUE, invisible=TRUE),
            silent=TRUE))
         if(class(zipTest) == "try-error") stop(errorText)
      } else {
         zipTest <- system("unzip", intern = TRUE)[1]
         if(is.na(zipTest) || length(grep("UnZip", zipTest)) == 0)
            stop(errorText)
      }
   }

   # create temp dir
   if(!file.exists(workDir))
   {
      announce(verbose, "  Creating ", workDir, "\n")
      dir.create(workDir, showWarnings = TRUE, recursive = FALSE)
      if(!file.exists(workDir)) stop("Error creating working directory")
   }

   announce(verbose, "  Setting wd to ", workDir, "\n")
   
   workingCopy <- paste(basename(file), sep = "")  
   
   # copy file to the tmp dir
   if(!file.exists(file)) stop(paste(file, "does not exist"))
   announce(verbose, "  Copying ", file, "\n")
   if(!file.copy(file, paste(workDir, "/", workingCopy, sep = ""), overwrite = TRUE)) stop("Error copying odt file")
   
   setwd(workDir)

   # unpack the file
   announce(verbose, "  Decompressing ODF file using", zipCmd[2], "\n")
   if(.Platform$OS.type == "windows")
   {
      if(system(zipCmd[2], invisible = TRUE) != 0) stop("Error unzipping file")
   } else {
      if(system(zipCmd[2]) != 0) stop("Error unzipping odt file")
   }

   # remove original file
   announce(verbose, "\n  Removing ", workingCopy, "\n")
   file.remove(workingCopy)
   if (file.exists(workingCopy)) stop("Error removing original file")

   #configure Pictures directory
   if(!file.exists(paste(workDir, "/Pictures", sep = "")))
   {
      announce(verbose, "  Creating a Pictures directory\n")
      picDir <- dir.create("Pictures", showWarnings = TRUE, recursive = FALSE)
      if(!picDir)  stop("Error creating Pictures directory")
   }
   assign(
      "picPath",  
      paste(workDir, "/Pictures", sep = ""), 
      env = .odfEnv)

   # find xml files
   fileList <- list.files(workDir)
   xmlFiles <- fileList[grep(".xml", tolower(fileList), fixed = TRUE)]
   if(length(xmlFiles) == 0) stop("Something is wrong - no xml")

   # load xml into list
   xmlContents <- vector(mode = "character", length = length(xmlFiles))
   for(i in seq(along = xmlContents)) {
      xmlContents[i] <- readXML(xmlFiles[i], verbose = control$verbose) }
   names(xmlContents) <- xmlFiles

   # add style information, if any, to styles.xml and content.xml
   styles <- getStyleDefs()
   styleInfo <- xmlContents["styles.xml"]
   xmlContents["styles.xml"] <- addStyleDefs(styleInfo, styles, "styles", control$verbose)

   styleInfo <- xmlContents["content.xml"]
   xmlContents["content.xml"] <- addStyleDefs(styleInfo, styles, "content", control$verbose)

   #get indexes and lengths of sweave expression matches
   stags <- tagsIdxs(xmlContents, verbose=control$verbose)
   #for convenience
   tagsFound <- tagsExist(stags)

   if(any(tagsFound)) {
      announce(verbose, "\n  Sweave tags found in: ", xmlFiles[tagsFound], "\n")
      sweaveFiles <- xmlFiles[tagsFound]
      sweaveContents <- xmlContents[tagsFound]

      for(j in seq(along = sweaveFiles)) {
         announce(verbose, "  Removing xml around <<>>= for ", sweaveFiles[j], "\n")
         if(!is.null(sweaveContents[j])) sweaveContents[j] <-
         processXml(sweaveContents[j], stags[,j])
         # write processed lines to Rnw file
         rnwFileName <- (gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]))
         announce(verbose, "  Writing ", sweaveFiles[j], " to ", rnwFileName, "\n")
         rnwFile <- file(rnwFileName, "wb")
         writeXML(sweaveContents[[j]], rnwFile)

         #nuke the xml file
         announce(verbose, "\n  Removing ", sweaveFiles[j], "\n")
         file.remove(sweaveFiles[j], recursive = TRUE)
         if (file.exists(sweaveFiles[j])) stop("Error removing xml file file")

         #Sweave results to new xml file
         announce(verbose, "  Sweaving ",rnwFileName, "\n\n")

         Sys.setlocale("LC_CTYPE", currentLocale[1])
         Sys.setlocale("LC_COLLATE", currentLocale[2])

         Sweave(
            file =   paste(rnwFileName, sep = ""),
            output = paste(sweaveFiles[j], sep = ""),
            quiet = !control$verbose,
            driver = RweaveOdf(), control = control)

         Sys.setlocale("LC_CTYPE", "C")
         Sys.setlocale("LC_COLLATE", "C")

         # remove sweave file
         announce(verbose, "  Removing ", rnwFileName, "\n")
         file.remove(rnwFileName, recursive = TRUE)
         if (file.exists(rnwFileName)) stop("Error removing xml file file")
      }
   }

   # if there was no Sweave tags in styles.xml, write that out too
   if(!tagsFound[which(xmlFiles == "styles.xml")]) {
      styleFile <- file("styles.xml", "wb")
      sink(styleFile)
      cat(xmlContents[[which(xmlFiles == "styles.xml")]])
      sink()
      close(styleFile)
   }

   announce(verbose, "\n\  Packaging file using", zipCmd[1], "\n")
   if(.Platform$OS.type == "windows") {
      if(system(zipCmd[1], invisible = TRUE) != 0)  stop("Error zipping file")
   } else {
      if(system(zipCmd[1]) != 0) stop("Error zipping file")
   }

   # copy final file to destination
   if(!file.exists(workingCopy))  stop(paste(workingCopy, "does not exist"))
   announce(verbose, "  Copying ", workingCopy, "\n")
   if(!file.copy(workingCopy, dest, overwrite = TRUE))  stop("Error copying odt file")

   announce(verbose, "  Resetting wd\n")
   setwd(currentLoc)

   Sys.setlocale("LC_CTYPE", currentLocale[1])
   Sys.setlocale("LC_COLLATE", currentLocale[2])

   assign(
      "picPath",  
      NA, 
      env = .odfEnv)

   # delete working dir
   if(control$cleanup)
   {
      announce(verbose, "  Removing ", workDir, "\n")
      unlink(workDir, recursive = TRUE)
      if (file.exists(workDir)) unlink(shQuote(workDir), recursive=TRUE)
      if (file.exists(workDir)) unlink(shellQuote(workDir), recursive=TRUE)
      if (file.exists(workDir)) stop("Error removing work dir")
   }
   invisible(NULL)
}

"announce" <- function (verbose = TRUE, ...) 
{
   if (verbose) cat(...)
   flush.console()
   invisible()
} 

"attR<-" <- function (x, name, value) {
   #apply an attribute lists in a list
   #value always gets unlisted before assignment
   if( (length(value) != length(x)) || (length(value) == 1))
      lapply(x, function(x) {attr(x, name) <- value[[1]]; x})
   else
      lapply(seq(x), function(y) { attr(x[[y]], name) <- value[[y]]; x[[y]]})
}

"attR" <- function (x, name) {
   #return the values of attribute "name" items in list
   lapply(x, function(x) attr(x, name))
}

"getByAttR" <- function (x, name, value) {
 #return items in list x with attribute called "name"
 x[unlist(lapply(x, function(y) attr(y, name) == value))]
}

"debugWrite" <- function(filename, content){
   #used at breakpoints during debugging
   outFile <- file(filename, "wb")
   writeBin(content, outFile)
   close(outFile)
}

"shellQuote" <- function(x) {
   if (.Platform$OS.type == "windows"){
      return(paste('"', x, '"', sep=""))
   } else {
      return(gsub(" ", "\\\\ ", x))
   }
}

"subin" <- function(x, matches, pieces){
	#input
	#   x:  character string, the content
	#   matches:  TODO 
	#   pieces: strings to substitute into x 
	#value:  x, with pieces inserted

   piece <- length(pieces)
   if (piece == 0) return(x)
	#TODO:  matches should be a vector, so [[ can be replaced by [
   matchstart <- matches[[piece]]
   matchend <- matchstart + attr(matches, "match.length")[piece]
   part1 <- substr(x, 1, matchstart - 1)
   part2 <- substr(x, matchstart, matchend -1)
   part3 <- substr(x, matchend, nchar(x))
   paste(
      c(
         subin(
            part1,
            matches,
            pieces[-piece]),
         pieces[piece],
         part3),
      collapse = "")
}

"tagsIdxs" <- function(x, verbose=FALSE) {
      #input:  character
      #output: 2-way integer list of offsets where tags were found
      #  "match.length" attribute contains a length for each offset
      #  See. "gregexpr", under "grep" in the R reference manual

      matchtype = "match.type"

      announce(verbose, "   Regular Expression:  Sexpr\n")
      out1 <- gregexpr("(?s)\\\\Sexpr\\{[^\\}]*?\\}", x, perl=TRUE)
      attR(out1, matchtype) <- "sexpr"

      #find the innermost "text:p" block containing a chunk, and all subseqent
      #   content up to and including the "text:p" block containing the "@",
      #   which marks the end of a chunk
      #breaking down the regular expression:
      #   (?s)
      #      make repetition metacharacters match a newline
      #   (?U)
      #      make repetition characters ungreedy
      #   ((?>[^>]*)|(?:(?!<text:p).))*
      #      match anything not containing "<text:p".  It has been optimized
      #      to reduce recursion of the regular expression
      #   (?:(?!&gt;&gt(?!=)).)
      #      match any character as long as two "greater-than" characters not
      #      followed by an "equals" sign don't come next
      #   (/text:p>|.*</text:p)
      #      the closing tag might immediately follow the "@", or it might be
      #      preceded by some other non-block tags
      #
      #test data
      #"hello you <text:p> what <b>do</b> you <text:p>there is all this <text:p> and <i>there</i> and <code>yucka</code> but  &lt;&lt; what </text:p> in the world </text:p> the end </text:p> blather.hello you <text:p> what <b>do</b> you <text:p>there is all this <text:p> and <i>there</i> and <code>yucka</code> but  &lt;&lt; what </text:p> in the world </text:p> the end </text:p> blather."

      announce(verbose, "  Regular Expression:  <<>>\n")

      #The following expression works, and is a little less recursive
      #also replaced on 20060821
      #out2 <- gregexpr("(?s)(?U)<text:p((?>[^<]*)|(?:(?!<text:p).))*&lt;&lt;(?:(?!&gt;&gt(?!=)).)*&gt;&gt;=.*>@<(/text:p>|.*</text:p>)", x, perl=TRUE)
      #the following expression works, but is highly recursive.
      #It was replaced on 20060821
      #out2 <- gregexpr("(?s)(?U)<text:p(?:(?!<text:p).)*&lt;&lt;(?:(?!&gt;&gt(?!=)).)*&gt;&gt;=.*>@<(/text:p>|.*</text:p>)", x, perl=TRUE)

      #verbose regular expression, but lazy evaluation down the list of
      #alternative expressions avoids most recursion
      ##(replaced 2006-10-13)## out2 <- gregexpr("(?s)(?U)<text:p([^<]|<[^t]|<t[^e]|<te[^x]|<tex[^t]|<text[^:]|text: [^p])*&lt;&lt;(?:(?!&gt;&gt(?!=)).)*&gt;&gt;=.*>@<(/text:p>|.*</text:p>)", x, perl=TRUE)
		#Thanks to Philip Hazel for this example of eliminating regular
		#expression backtraking into runs of non-< and non-& characters
      out2 <- gregexpr("(?s)(?U)<text:p((?>[^<&]*)(?(?=<text:p)(?!)|.))*&lt;&lt;(?:(?!&gt;&gt(?!=)).)*&gt;&gt;=.*>@[ \t]*<(/text:p>|.*</text:p>)", x, perl=TRUE)
      attR(out2, matchtype) <- "chunk"
      announce(verbose, "  Regular Expression:  SweaveOpts\n")
      out3 <- gregexpr("(?s)\\\\SweaveOpts\\{[^\\}]*?\\}", x, perl=TRUE)
      attR(out3, matchtype) <- "option"
      mapply(list, out1, out2, out3)
      }

"tagsExist" <- function(x) {
   #input:  single list or list of lists
   #output:  boolean
   tagsExist_ <- function(y) any(attr(y, "match.length") != -1)
   if (is.null(dims <- dim(x))) return(tagsExist_(x))
   x <- unlist(lapply(x, tagsExist_))
   dim(x) <- dims
   apply(x, 2, function(y) any(y)) }


"tagsGet" <- function (x, matches) {
   #input:
   #
   #   x: character
   #
   #   matches:  list of integers with atribute 'match.length'
   #
   #output: character vector of substrings of x
   unlist(
      lapply(
         seq(length(matches)),
         function (y) {
            len <- attr(matches, "match.length")[y]
            if (len < 0) return(-1)
            substr(x, matches[y],
               matches[y] + attr(matches, 'match.length')[y] - 1)
         }
      )
   )
}

"writeXML" <- function(content, outFile, method="old", verbose=FALSE){
   writeBin(content, outFile)
   close(outFile)
}
