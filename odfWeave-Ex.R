pkgname <- "odfWeave"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('odfWeave')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("PreProc")
### * PreProc

flush(stderr()); flush(stdout())

### Name: odfTranslate
### Title: Translation of text to XML
### Aliases: odfTranslate
### Keywords: utilities

### ** Examples


y <- "This is an &quot;interesting&quot; function."

cat(odfTranslate(y))



cleanEx()
nameEx("adjustImageSize")
### * adjustImageSize

flush(stderr()); flush(stdout())

### Name: adjustImageSize
### Title: Automate image size adjustments
### Aliases: adjustImageSize
### Keywords: utilities

### ** Examples

getImageDefs()
adjustImageSize(2, 5, scale = 1.75)
getImageDefs()



cleanEx()
nameEx("announce")
### * announce

flush(stderr()); flush(stdout())

### Name: announce
### Title: cat and flush console
### Aliases: announce
### Keywords: utilities

### ** Examples

announce()
announce(FALSE, "this will not be printed")
announce(TRUE, "but this will be")




cleanEx()
nameEx("listString")
### * listString

flush(stderr()); flush(stdout())

### Name: listString
### Title: Convert a vector to a textual list
### Aliases: listString
### Keywords: utilities

### ** Examples

listString(letters[1])
listString(letters[1:2])
listString(letters[1:4])



cleanEx()
nameEx("matrixPaste")
### * matrixPaste

flush(stderr()); flush(stdout())

### Name: matrixPaste
### Title: Element-Wise Paste of Conforming Matrices
### Aliases: matrixPaste
### Keywords: utilities

### ** Examples

mat1 <- matrix(letters[1:6], nrow = 2)
mat2 <- matrix(LETTERS[1:6], nrow = 2)
mat3 <- matrix(paste(1:6), nrow = 2)

matrixPaste(mat1, mat2)
matrixPaste(mat1, mat2, sep = "+")
matrixPaste(mat1, mat2, mat3, sep = c("+", "plus"))




cleanEx()
nameEx("odfCat")
### * odfCat

flush(stderr()); flush(stdout())

### Name: odfCat
### Title: Concatenate and Print in Native ODF
### Aliases: odfCat
### Keywords: utilities

### ** Examples

odfCat("\"hello world\"")
odfCat("these are the first letters", letters[1:5])
odfCat("some random normal data:", rnorm(5))




cleanEx()
nameEx("odfFigureCaption")
### * odfFigureCaption

flush(stderr()); flush(stdout())

### Name: odfFigureCaption
### Title: Provide a Caption for a Figure
### Aliases: odfFigureCaption
### Keywords: utilities

### ** Examples

## Not run: 
##D odfFigureCaption("This is a very interesting figure")
## End(Not run)



cleanEx()
nameEx("odfInsertPlot")
### * odfInsertPlot

flush(stderr()); flush(stdout())

### Name: odfInsertPlot
### Title: Write XML for image inseration
### Aliases: odfInsertPlot
### Keywords: utilities

### ** Examples

odfInsertPlot("plot.png", 4, 4)



cleanEx()
nameEx("odfItemize")
### * odfItemize

flush(stderr()); flush(stdout())

### Name: odfItemize
### Title: Write XML for one layer lists
### Aliases: odfItemize
### Keywords: utilities

### ** Examples

odfItemize(levels(iris$Species))



cleanEx()
nameEx("odfPageBreak")
### * odfPageBreak

flush(stderr()); flush(stdout())

### Name: odfPageBreak
### Title: Generate a Page Break
### Aliases: odfPageBreak
### Keywords: utilities

### ** Examples

## Not run: 
##D odfPageBreak()
## End(Not run)



cleanEx()
nameEx("odfSetPageStyle")
### * odfSetPageStyle

flush(stderr()); flush(stdout())

### Name: odfSetPageStyle
### Title: Set the Page Style
### Aliases: odfSetPageStyle
### Keywords: utilities

### ** Examples

## Not run: 
##D odfSetPageStyle("RlandscapePage")
## End(Not run)



cleanEx()
nameEx("odfTable")
### * odfTable

flush(stderr()); flush(stdout())

### Name: odfTable
### Title: Create an Open Document Format table
### Aliases: odfTable odfTable.data.frame odfTable.matrix
###   odfTable.character odfTable.factor odfTable.numeric
### Keywords: utilities

### ** Examples

odfTable(iris[1:5,])



cleanEx()
nameEx("odfTableCaption")
### * odfTableCaption

flush(stderr()); flush(stdout())

### Name: odfTableCaption
### Title: Provide a Caption for a Table
### Aliases: odfTableCaption
### Keywords: utilities

### ** Examples

## Not run: 
##D odfTableCaption("This is a very boring table")
## End(Not run)



cleanEx()
nameEx("odfWeave")
### * odfWeave

flush(stderr()); flush(stdout())

### Name: odfWeave
### Title: Sweave processing of Open Document Format (ODF) files
### Aliases: odfWeave
### Keywords: utilities

### ** Examples


## Not run: vignette("odfWeave")

## Not run: 
##D demoFile <- system.file("examples", "examples.odt", package = "odfWeave")
##D demoFile <- system.file("examples", "testCases.odt", package ="odfWeave")
##D demoFile <- system.file("examples", "formatting.odt", package = "odfWeave")
## End(Not run)

demoFile <- system.file("examples", "simple.odt", package = "odfWeave")
outputFile <- gsub("simple.odt", "output.odt", demoFile)

library(odfWeave)
odfWeave(demoFile, outputFile)



cleanEx()
nameEx("odfWeaveControl")
### * odfWeaveControl

flush(stderr()); flush(stdout())

### Name: odfWeaveControl
### Title: Control odfWeave options
### Aliases: odfWeaveControl
### Keywords: utilities

### ** Examples

odfWeaveControl(cleanup = TRUE)



cleanEx()
nameEx("pkgVersions")
### * pkgVersions

flush(stderr()); flush(stdout())

### Name: pkgVersions
### Title: List packages currently used
### Aliases: pkgVersions
### Keywords: utilities

### ** Examples

pkgVersions()
pkgVersions("matrix")
pkgVersions("data frame")



cleanEx()
nameEx("styles")
### * styles

flush(stderr()); flush(stdout())

### Name: setStyles
### Title: Style Definitions and Assignments
### Aliases: getStyles setStyles getStyleDefs setStyleDefs getImageDefs
###   setImageDefs
### Keywords: utilities

### ** Examples

currentStyleDefs <- getStyleDefs()
currentStyleDefs$ArialNormal$fontSize <- "10pt"
setStyleDefs(currentStyleDefs)



cleanEx()
nameEx("tableStyles")
### * tableStyles

flush(stderr()); flush(stdout())

### Name: tableStyles
### Title: Generate Table Styles
### Aliases: tableStyles
### Keywords: utilities

### ** Examples

irisStyles <- tableStyles(iris, useRowNames = TRUE, header = names(iris))
irisStyles$text[2,3] <- "ttRed"
odfTable(iris, useRowNames = TRUE, styles = irisStyles)




### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
