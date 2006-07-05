# R code in ODF files get wrapped in xml and some of the characters
# may be changed from what the user types in.
# This function processes the xml files and writes out Rnw files
# (hopefully) consistent with Sweave's expectations.

"processXml" <-
function(x)
{   
   
   # for in-line Sweave tags, make sure that any converted character
   # are fixed prior to the Sweave call
   SexpLines <- grep("\\Sexpr\\{([^\\}]*)\\}", x)
   for(i in SexpLines) x[i] <- processInLine(x[i])
   
   # process lines with options. In this case, we will 
   # nuke everything else on the line

   SoptLines <- grep("\\SweaveOpts\\{([^\\}]*)\\}", x)
   x[SoptLines] <- odfTranslate(stripXmlTag(x[SoptLines]))
     
   # search thru for lines with <<>>=
   startLines <- grep("&lt;&lt;(.*)&gt;&gt;=", x)
   if(length(startLines) > 0)
   {
      for(i in seq(along = startLines))
      {
         #rnwOut is the test of the file that Sweave will run on
         if(i == 1) rnwOut <- x[1:(startLines[1] - 1)]
         
         # create a container for the pure R code
         pureR <- vector(mode = "character", length = 0)
         
         # count will contain the number of lines of xml that
         # contain R code
         count <- 0
             
         # scan the xml until the end of the code chunk (a line with only a @)
         for(j in startLines[i]:length(x))
         {
         
            count <- count + 1
            
            # convert the line of xml to pure R and possible
            # expand the number of lines of pure R, i.e, 
            # length(x) may !=  length(xml2R(x[j))         
            rCode <- xml2R(x[j])
            
            # append this/these lines of R to the rest of
            # the code chunk
            pureR <- c(pureR, rCode)
            
            if(length(grep("^@", rCode)) > 0)
            {         
               # append R chunk to existing xml
               rnwOut <- c(rnwOut, pureR)
               
               # if there is more xml between the end of the chunk
               # and the next chunk, add it
               if(i < length(startLines))
               {
                  rnwOut <- c(rnwOut, x[(startLines[i] + count):(startLines[i + 1] - 1)])
               } else rnwOut <- c(rnwOut, x[(startLines[i] + count):length(x)])
               break            
            }
         }
      }
   } else rnwOut <- x      
   rnwOut
}

