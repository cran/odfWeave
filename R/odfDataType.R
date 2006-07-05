"odfDataType" <-
function(x)
{
   internalMode <- typeof(x)
   dataMode <- internalMode
   dataMode[dataMode == "character"] <- "string"
   dataMode[dataMode == "integer"] <- "double"   
   # possible office:value-type values are:
   #float, double, percentage, currency, date, time, boolean, string, void   
   if(any(internalMode == "integer" & is.factor(x))) dataMode[internalMode == "integer" & is.factor(x)] <- "string"
   dataMode
}

