#
# GEOCODE ADDRESSES AND ADD XY FIELDS
# NOTE THAT PART OF THE SCRIPT IS COMMENTED OUT DUE TO RESTRICTIONS ON NUMBER OF QUERIES TO GOOGLE API (GEOCODE)
# 
rm(list = ls())
require(readxl)
require(stringi)
require(plyr)
require(dplyr)
require(ggmap)
require(arcgisbinding)

projws= projws= "D:/Projects/Tennessee/GNAA/Data/1stQtr2017Data"
setwd(projws)
quarterdate<-"2017-03-31"

arc.check_product()
fc<- arc.open("D:\\Projects\\Tennessee\\AGProGNAA\\AGProGNAA.gdb\\AptCommsLocVer")
AptRef.df<- arc.select(fc, names(fc@fields))
AptRef.df<- as.data.frame(AptRef.df)
# clean the Q1 2017 Apartments Property List 
# previous name, ", The" substring fixed, space at end of NAME string, 
# length<=30 character only name field (no spaces or special)

# The Q1_2017_Apts.csv table is just a csv version of the ApartmentsPropertyList

#aptsq12017<- read.csv("export/Q1_2017_Apts.csv", stringsAsFactors = FALSE)
aptsq12017<- read_excel("export/ApartmentPropertyList.xlsx")
aptsq12017<- aptsq12017[which(aptsq12017$ID %in% AptRef.df$USER_ID),]

prevname<- function(indf){
  for (i in 1:nrow(indf)){
    rowi<- indf[i,]
    if (grepl("\\(", rowi$NAME)){
      splitname<- unlist(strsplit(rowi$NAME, split = "\\("))
      keepname<- splitname[1]
      previousname<- unlist(splitname[2])
      indf$NAME[i]<- unlist(keepname)
      indf$previous[i]<- unlist(strsplit(previousname, split = "\\)")[1])
    }else{
      indf$previous[i]<- "NA"
    }
    if (stri_sub(indf$NAME[i], -1, -1)== " "){
      indf$NAME[i]<- unlist(stri_sub(indf$NAME[i], 1, -2))
    }
    if (grepl(", The", indf$NAME[i])==TRUE){
      indf$NAME[i]<- unlist(paste("The", indf$NAME[i]))
      indf$NAME[i]<- unlist(gsub(", The", "", indf$NAME[i]))
    }
  }
  return(indf)
}

tester<- prevname(aptsq12017)
tester<-as.data.frame(tester)
tester$strname<- gsub("\\.|\\_|\\-|\"|\'|&|$|*|,|\\s|!", "", tester$NAME)

tester$strname<- gsub("@", "at", tester$strname)
tester$strname<- gsub("/", "_", tester$strname)
tester$strname<- stri_sub(tester$strname, 1, 30)
# Need to figure out how to handle the Address whent the Unit Number is included and contains a comma and/or special characters....
#tester$apt_num<- ""

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
GeoApts$LAT<- unlist(sapply(GeoApts$ID, function(x) AptRef.df[which(AptRef.df$USER_ID==x),]$POINT_Y))
GeoApts$LON<- unlist(sapply(GeoApts$ID, function(x) AptRef.df[which(AptRef.df$USER_ID==x),]$POINT_X))
GeoApts$SUBMARKET<- unlist(sapply(GeoApts$ID, function(x) AptRef.df[which(AptRef.df$USER_ID==x),]$GNAA_NewSubmarkets_NAME))

                            
# 
# ONLY USE IF GEOCODING ADDRESSES IS NEEDED!
#####################################################################################################################################################
# BE AWARE OF NUMBER OF GOOGLE API QUERIES REMAINING BEFORE RUNNING BELOW
geocodeQueryCheck()
#GeoApts<- addxyfun(tester)
# For ID==2046 ; Revere at Hidden Creek (2067 Springdale Lane	Gallatin	TN	37066) USE following Coordinates: 36.366047	-86.537999
# For ID==1918 ; Lenox Creekside 8131 Lenox Creekside Dr, Apt. N-10	Antioch	TN	37013 USE following Coordinates: 36.011209	-86.699743
# for ID==1704 ; Summerfield 36.226336, -86.345896
# For ID==154 ; Hickory Chase 36.266093	-86.723239
# 202	Littlestone at Village Green I ; 36.379689, -86.472769
# Need to find a way to eliminate the Apartment Numbers from the Address field (special characters are throwing everything off)
# GeoApts[which(GeoApts$ID==2046),]$LAT<- 36.366047
# GeoApts[which(GeoApts$ID==2046),]$LON<- -86.537999
# GeoApts[which(GeoApts$ID==1918),]$LAT<- 36.011209
# GeoApts[which(GeoApts$ID==1918),]$LON<- -86.699743
# GeoApts[which(GeoApts$ID==1704),]$LAT<- 36.226336
# GeoApts[which(GeoApts$ID==1704),]$LON<- -86.345896
# GeoApts[which(GeoApts$ID==154),]$LAT<- 36.266093
# GeoApts[which(GeoApts$ID==154),]$LON<- -86.723239
# GeoApts[which(GeoApts$ID==202),]$LAT<- 36.379689
# GeoApts[which(GeoApts$ID==202),]$LON<- -86.472769
# WRITE OUTPUT
##################################################################################################################################################### 

write.csv(GeoApts, paste0(gsub("-", "_",quarterdate), "_AptsClean.csv"), row.names = FALSE)

