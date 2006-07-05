# put in check for optimized files
# documentation and beta testing
# code review
# add check for slim style file

"odfWeave" <- function(file, dest, workDir = tempdir(), control = odfWeaveControl())
{
   currentLoc <- getwd()
   # create a temp dir (or have dir specified)
   if(!file.exists(workDir)) 
   {
      if(control$verbose) cat("  Creating ", workDir, "\n"); flush.console()
      if(!dir.create(workDir, showWarnings = TRUE, recursive = FALSE))
         stop("Error creating working directory")
   }
   
   if(control$verbose) cat("  Setting wd\n"); flush.console()
   setwd(workDir)
      
   workingCopy <- paste(workDir, "/", basename(file), sep = "")      
      
   # copy file to the tmp dir       
   if(!file.exists(file)) stop(paste(file, "does not exist"))   
   if(control$verbose) cat("  Copying ", file, "\n"); flush.console()      
   if(!file.copy(file, workingCopy, overwrite = TRUE))
      stop("Error copying odt file")

   # unpack the file 
   zipCmd <- control$zipCmd
   zipCmd[2] <- gsub(
      "$$file$$", 
      paste(
         ifelse(.Platform$OS.type == "windows", "\"", ""), 
         workDir, "/", basename(file),
         ifelse(.Platform$OS.type == "windows", "\"", ""), 
         sep = ""), 
      zipCmd[2], 
      fixed = TRUE)   
      
   if(control$verbose) cat("  Decompressing ODF file using", zipCmd[2], "\n"); flush.console()  
   if(.Platform$OS.type == "windows")
   {   
      if(system(zipCmd[2], invisible = TRUE) != 0) stop("Error unzipping file")
   } else {
      if(system(zipCmd[2]) != 0) stop("Error unzipping odt file")   
   }   
   
   # remove original file
   if(control$verbose) cat("\n  Removing ", workingCopy, "\n"); flush.console()      
   if(unlink(workingCopy, recursive = TRUE) == 1)  stop("Error removing original file")   
   
   #check for Pictures directory
   if(!file.exists(paste(workDir, "/Pictures", sep = "")))
   {
      if(control$verbose) cat("  Creating a Pictures directory\n"); flush.console()                
      picDir <- dir.create(paste(workDir, "/Pictures", sep = ""), showWarnings = TRUE, recursive = FALSE)
      if(!picDir)  stop("Error creating Pictures directory")   
   }
   
   # find xml files
   fileList <- list.files(workDir)
   xmlFiles <- fileList[grep(".xml", tolower(fileList), fixed = TRUE)]
   if(length(xmlFiles) == 0) stop("Something is wrong - no xml") 
   # should make not enough look for specific files
   
   # load xml into list
   xmlContents <- vector(mode = "list", length = length(xmlFiles))
   
   # readLines is more natural, but shows warnings due to no EOF (OO creates XML wo EOF)
   for(i in seq(along = xmlContents))
   {
      xmlContents[[i]] <- readXML(xmlFiles[i], verbose = control$verbose)
   }

   # R can have a problem writing out lines longer than 999 characters, so collapse if needed
   if(control$verbose) cat("  Breaking long lines...\n"); flush.console()           
   
   for(i in seq(along = xmlContents)) xmlContents[[i]] <- checkLength(xmlContents[[i]])
   
   # add style information, if any, to styles.xml and content.xml
   styles <- getStyleDefs()
      
   styleInfo <- xmlContents[[which(xmlFiles == "styles.xml")]]
   xmlContents[[which(xmlFiles == "styles.xml")]] <- addStyleDefs(styleInfo, styles, "styles", control$verbose)        
   
   styleInfo <- xmlContents[[which(xmlFiles == "content.xml")]]     
   xmlContents[[which(xmlFiles == "content.xml")]] <- addStyleDefs(styleInfo, styles, "content", control$verbose)          
  
   
   findTags <- function(x) (length(c(grep("\\Sexpr\\{([^\\}]*)\\}", x), grep("&lt;&lt;(.*)&gt;&gt;=", x))) > 0)
   hasTags <- unlist(lapply(xmlContents, findTags))
     
   if(any(hasTags))
   {
      if(control$verbose) cat("\n  Sweave tags found in: ", xmlFiles[hasTags], "\n"); flush.console()      
      sweaveFiles <- xmlFiles[hasTags]
      sweaveContents <- xmlContents[hasTags]
      
      for(j in seq(along = sweaveFiles))
      {
         if(control$verbose) cat("  Removing xml around <<>>= for ", sweaveFiles[j], "\n"); flush.console()           
         if(findTags(sweaveContents[[j]])) sweaveContents[[j]] <- processXml(sweaveContents[[j]])       
         # write processed lines to Rnw file
         if(control$verbose) cat("  Writing ", sweaveFiles[j], " to ", gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), "\n"); flush.console()           
         rnwFile <- file(paste(workDir, "/", gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), sep = ""), "wb")
         writeLines(sweaveContents[[j]], rnwFile)
         close(rnwFile)
         
         #nuke the xml file
         if(control$verbose) cat("\n  Removing ", sweaveFiles[j], "\n"); flush.console()           
         if(unlink(paste(workDir, "/", sweaveFiles[j], sep = ""), recursive = TRUE) == 1) 
            stop("Error removing xml file file") 
         if(control$verbose) cat("  Sweaving ",gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), "\n\n"); flush.console()            
         
         #Sweave results to new xml file
         Sweave(
            file =   paste(workDir, "/", gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), sep = ""),
            output = paste(workDir, "/", sweaveFiles[j], sep = ""),
            quiet = !control$verbose,
            driver = RweaveOdf(), control = control)
         
         # remove sweave file
         if(control$verbose) cat("  Removing ", gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), "\n"); flush.console()           
         if(unlink(paste(workDir, "/", gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), sep = ""), recursive = TRUE) == 1) 
            stop("Error removing xml file file") 
         }
   }
   
   # if there was no Sweave tags in styles.xml, write that out too
   if(!hasTags[which(xmlFiles == "styles.xml")])       
   {
      styleFile <- file(paste(workDir, "/styles.xml", sep = ""), "wb")
      sink(styleFile)   
      cat(xmlContents[[which(xmlFiles == "styles.xml")]])
      sink()
      close(styleFile)
   }
   
   zipCmd[1] <- gsub(
      "$$file$$", 
      paste(
         ifelse(.Platform$OS.type == "windows", "\"", ""),      
         workDir, 
         "/", 
         basename(file), 
         ifelse(.Platform$OS.type == "windows", "\"", ""),         
         sep = ""), 
      zipCmd[1], 
      fixed = TRUE)

   if(control$verbose) cat("\n\  Packaging file using", zipCmd[1], "\n"); flush.console()   
   if(.Platform$OS.type == "windows")
   {   
      if(system(zipCmd[1], invisible = TRUE) != 0)  stop("Error zipping file")
   } else {
      if(system(zipCmd[1]) != 0) stop("Error zipping file")   
   }

   # copy final file to destination      
   if(!file.exists(workingCopy))  stop(paste(workingCopy, "does not exist"))   
   if(control$verbose) cat("  Copying ", workingCopy, "\n"); flush.console()      
   if(!file.copy(workingCopy, dest, overwrite = TRUE))  stop("Error copying odt file")

   if(control$verbose) cat("  Resetting wd\n"); flush.console()
   setwd(currentLoc)

   # delete working dir
   if(control$cleanup)
   {
      if(control$verbose) cat("  Removing ", workDir, "\n"); flush.console()
      if(unlink(workDir, recursive = TRUE) == 1) stop("Error removing work dir")
   }
   invisible(NULL)
}

