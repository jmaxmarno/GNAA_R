##  Max Marno
# North Line GIS LLC.
#  9/14/2017
# 
#  Clean and if necessary add XY coordinates to the 
# NOTE THAT PART OF THE SCRIPT IS COMMENTED OUT DUE TO RESTRICTIONS ON NUMBER OF QUERIES TO GOOGLE API (GEOCODE)
#   CHECK APTREF COLNAMES FOR CONSISTENCY IN LINES 89-91
rm(list = ls())
require(readxl)
require(stringi)
require(ggmap)
require(arcgisbinding)
arc.check_product()
####   INPUT    ####
workspace <- "D:/Projects/GNAA/Data/1stQtr2017Data"
setwd(workspace)
quarterdate<-"2017-03-31"
# Feature Class path with verified locations:
AptsLocVer<- "D:\\Projects\\GNAA\\AGProGNAA\\AGProGNAA.gdb\\AptCommsLocVer"
# Input exported apartment property list (from Access)
AptPropList <- "export/ApartmentPropertyList.xlsx"
###############################

fc<- arc.open(AptsLocVer)
AptRef.df<- arc.select(fc, names(fc@fields))
AptRef.df<- as.data.frame(AptRef.df)
# clean the Q1 2017 apartments.df Property List 
# previous name, ", The" substring fixed, space at end of NAME string, 
# length<=30 character only name field (no spaces or special)

apartments.df<- read_excel(AptPropList)
#apartments.df<- apartments.df[which(apartments.df$ID %in% AptRef.df$USER_ID),]

prevname<- function(indf){
  for (i in 1:nrow(indf)){
    rowi<- indf[i,]
    # SMOOTH BRACKET INDICATES PREVIOUS NAME INCLUDED IN NAME STRING
    if (grepl("\\(", rowi$NAME)){
      splitname<- unlist(strsplit(rowi$NAME, split = "\\("))
      keepname<- splitname[1]
      previousname<- unlist(splitname[2])
      indf$NAME[i]<- unlist(keepname)
      indf$previous[i]<- unlist(strsplit(previousname, split = "\\)")[1])
    }else{
      indf$previous[i]<- "NA"
    }
    # REMOVE SPACE FROM END OF NAME STRING
    if (stri_sub(indf$NAME[i], -1, -1)== " "){
      indf$NAME[i]<- unlist(stri_sub(indf$NAME[i], 1, -2))
    }
    # REMOVE 'The' FROM NAME STRING
    if (grepl(", The", indf$NAME[i])==TRUE){
      indf$NAME[i]<- unlist(paste("The", indf$NAME[i]))
      indf$NAME[i]<- unlist(gsub(", The", "", indf$NAME[i]))
    }
  }
  return(indf)
}

tester<- prevname(apartments.df)
tester<-as.data.frame(tester)
# REMOVE SPECIAL CHARACTERS FROM NAME STRING
tester$strname<- gsub("\\.|\\_|\\-|\"|\'|&|$|*|,|\\s|!", "", tester$NAME)
tester$strname<- gsub("@", "at", tester$strname)
tester$strname<- gsub("/", "_", tester$strname)
# PARE T0 30 CHARACTERS
tester$strname<- stri_sub(tester$strname, 1, 30)

# ADD LAT LON BASED ON GOOGLE GEOCODER, INCLUDING THE APT COMMUNITY NAME FOR REFERENCE
addxyfun<- function(inputdf){
  for (i in 1:nrow(inputdf)){
    row.i<- inputdf[i,]
    geostring<- paste(row.i$Address, row.i$CityAddress, "Tennessee", row.i$PostalCode, sep = ", ")
    geostring<- gsub(", NA", "", geostring)
    #print(geostring)
    geocoords<- geocode(geostring, source = 'google')
    inputdf$LAT[i]<- geocoords$lat
    inputdf$LON[i]<- geocoords$lon
    print(paste0(as.character(i), "/", as.character(nrow(inputdf))," -- ",inputdf$NAME[i]))
    print(geostring)
  }
  return(inputdf)
}

GeoApts<- tester
GeoApts$LAT<- unlist(sapply(GeoApts$ID, function(x) if (x %in% AptRef.df$USER_ID==TRUE){
  return(AptRef.df[which(AptRef.df$USER_ID==x),]$POINT_Y))
  }else{
    return(NA)
  })

GeoApts$LON<- unlist(sapply(GeoApts$ID, function(x) if (x %in% AptRef.df$USER_ID){
  AptRef.df[which(AptRef.df$USER_ID==x),]$POINT_X))
} else{
  "NA"
}
# GeoApts$LON<- unlist(sapply(GeoApts$ID, function(x) AptRef.df[which(AptRef.df$USER_ID==x),]$POINT_X))
GeoApts$SUBMARKET<- unlist(sapply(GeoApts$ID, function(x) if (x %in% AptRef.df$USER_ID){
  AptRef.df[which(AptRef.df$USER_ID==x),]$GNAA_NewSubmarkets_NAME))
} else{
  "NA"
}
# GeoApts$SUBMARKET<- unlist(sapply(GeoApts$ID, function(x) AptRef.df[which(AptRef.df$USER_ID==x),]$GNAA_NewSubmarkets_NAME))

# ONLY USE IF GEOCODING ADDRESSES IS NEEDED!
##########################################################################
# BE AWARE OF NUMBER OF GOOGLE API QUERIES REMAINING BEFORE RUNNING BELOW
#geocodeQueryCheck()
#GeoApts<- addxyfun(tester)
###########################################################################

write.csv(GeoApts, paste0(gsub("-", "_",quarterdate), "eee_AptsClean.csv"), row.names = FALSE)

# test to feature class
require(sp)
pj4 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
spdf<- SpatialPointsDataFrame(coords = GeoApts[,c("LON", "LAT")], proj4string = pj4, data = GeoApts[,1:5])
arc.write(path = "D:\\Projects\\GNAA\\AGProGNAA\\AGProGNAA.gdb\\TESTOUT2", data = spdf)



















