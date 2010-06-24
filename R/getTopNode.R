# Parse the specified input file and return the top level node
getTopNode <- function(infile)
{
   # Parse the file
   doc <- xmlTreeParse(infile, trim=FALSE, addAttributeNamespaces=TRUE)
   doc$doc$children[[1]]
}
