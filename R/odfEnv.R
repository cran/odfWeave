# the idea for this strucure came form the lattice package

.odfEnv <- new.env()

# todo: style values should be validated


assign(
   "imageDefs",  
   list(
   
      type = "png",
      device = if(capabilities("png")) "png" else "bitmap",
      plotHeight = if(capabilities("png")) 480 else 480/72,
      plotWidth = if(capabilities("png")) 480 else 480/72,      
      dispHeight = 5,
      dispWidth = 5,      
      args = list()     
      ), 
   env = .odfEnv)
   

assign(
   "odfStyles",  
   list(
      paragraph = "ArialNormal",
      input = "ttRed",
      output = "ttBlue",  
      table = "Rtable1",
      cell = "noBorder",
      header = "lowerBorder",
      cellText = "ArialCentered",
      headerText = "ArialCenteredBold",
      bullet = "Rbullet"
      ), 
   env = .odfEnv)
   
assign(
   "styleDefs",  
   list(
   
# There are distinct text and paragraph properties that can be set (with some
# overlapping elements, such as background color). To specifiy text styles,
# these two sets of properties are specified under the "Paragraph" style here.   
      ArialCenteredBold = list(
         type = "Paragraph",
         parentStyleName = "",
         textAlign = "center",
         fontName = "Arial", 
         fontSize = "12pt",
         fontType = "bold",
         fontColor = "#000000"),
      ArialNormal = list(
         type = "Paragraph",
         parentStyleName = "",
         textAlign = "left",
         fontName = "Arial", 
         fontSize = "12pt",
         fontType = "normal",
         fontColor = "#000000"),      
      ArialCentered = list(
         type = "Paragraph",
         parentStyleName = "",
         textAlign = "center",
         fontName = "Arial", 
         fontSize = "12pt",
         fontType = "normal",
         fontColor = "#000000"),      
      ArialHighlight = list(
         type = "Paragraph",
         parentStyleName = "",
         textAlign = "center",
         fontName = "Arial", 
         fontSize = "12pt",
         fontType = "bold",
         fontColor = "#ff0000"),         
      ttBlue = list(
         type = "Paragraph",
         parentStyleName = "",
         textAlign = "left",
         fontName = "Courier New", 
         fontSize = "10pt",      
         fontType = "normal",
         fontColor = "#000080"),  
      ttRed = list(
         type = "Paragraph",
         parentStyleName = "",
         textAlign = "left",
         fontName = "Courier New", 
         fontSize = "10pt",      
         fontType = "normal",
         fontColor = "#800000"),           
         
# Cell specifications are also allowed to include text and paragraph
# properties. The "Table Cell" style will not include these properties
          
      noBorder = list(
         type = "Table Cell",
         backgroundColor="transparent",
         padding = "0.0382in",
         verticalAlign = "auto",  
         padding = "0.0382in",   
         leftBorder = "none",
         rightBorder = "none",
         topBorder = "none",
         bottomBorder = "none"),      
         
      lowerBorder = list(
         type = "Table Cell",
         backgroundColor="#0000ff",
         padding = "0.0382in",
         verticalAlign = "auto",
         leftBorder = "none",
         rightBorder = "none",
         topBorder = "none",
         bottomBorder = "0.0007in solid #000000"),      
         
         
            
      RTable1 = list(
         type = "Table",
#background style         
         marginLeft = "0.05in",
         marginRight = "0.05in",
         marginTop = "0.05in",
         marginBottom = "0.05in"),
         
      Rbullet = list(
         type = "Bullet List",
         level = "1",
         textStyleName = "Bullet_20_Symbols",
         bulletChar="\342\227\217",
         spaceBefore="0.25in",
         minLabelWidth="0.25in",
         fontName="StarSymbol")),
   env = .odfEnv)  


