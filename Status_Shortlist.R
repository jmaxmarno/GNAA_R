#  Max Marno
# North Line GIS LLC.
#  5/25/2017

'!!!!!!!!!!!!!!! USES $FINANCING AS TAB_NAME FIELD !!!!!!!!!!!!!!!!!!!'

# The primary function is to generate a csv file that matches the shortlist story map template, 
# containing html formatting in the Description fields for display.

# This script requires some manual input and setup. 
# It requires the Apartment Property List, Unit Mix and Rents, and Occupancy Management tables to be in csv format

# New Submarket boundaries must be in field with label 'SUBMARKET' - to replace old 'SUBMARKET' distictions
# Use aptdf subsettting at beginning to correctly generate shortlist data for all desired apartments

# change inputs, workspace, output file name, etc... NOT all at beginning of script (working on it)
rm(list=ls())
require(readxl)
require(plyr)
require(htmlTable)
require(arcgisbinding)

quarterdate<-"2017-03-31"  # MAY NEED TO VERIFY DATE FORMAT...

projws= "D:/Projects/GNAA/GNAA_R"
setwd(projws)

arc.check_product()
fc<- arc.open("D:\\Projects\\GNAA\\AGProGNAA\\AGProGNAA.gdb\\Apts_2017Q1")
Apt.fc<- arc.select(fc, names(fc@fields))
Apt.fc<- as.data.frame(Apt.fc)

## Now import csv's as data frames
aptdf<- read.csv("D:/Projects/GNAA/Data/1stQtr2017Data/2017_03_31_AptsClean.csv", stringsAsFactors = FALSE)
# List of desired apartment ID's
unitmixall<- read_excel("D:/Projects/GNAA/Data/1stQtr2017Data/export/UnitMixandRents.xlsx")
colnames(unitmixall)<- gsub(" ", ".", colnames(unitmixall))
occupancymanagement<- read_excel("D:/Projects/GNAA/Data/1stQtr2017Data/export/OccupancyManagement.xlsx")
colnames(occupancymanagement)<- gsub(" ", ".", colnames(occupancymanagement))
#############################################################################################
# GNAA do not collect - exclude from analysis - additional criteria can be specified here
aptdf<- aptdf[which(aptdf$GNAAnotcollect==FALSE),]
aptids<- aptdf$ID
#############################################################################################
# New Submarket Boundaries
aptdf$SUBMARKET<- lapply(aptids, function(x) Apt.fc[which(Apt.fc$ID==x),]$SUBMARKET)
aptdf$SUBMARKET<- unlist(aptdf$SUBMARKET)

#############################################################################################

OccMang.df<- occupancymanagement[which(as.character(occupancymanagement$OccupancyDate)==quarterdate),]

# get Shortlist data for each ID in aptids
####################################################################################################
# Process Apartment Property details, and Unit Mix data (by aptID, and # bdrms) into a new table

## This function takes an apartment ID argument, and a dataframe argument (with 'ID' field),
## and returns the rows that have that ID (one or many).
unitIDfun<- function(x, unitdf) {(unitdf[which(unitdf$ID==x),])}
# if object is empty ("") - return 'NA'
ifn<-function(x){
  x=toString(x)
  if (x==""){
    return(NA)
  } else {
    return(x)
  }
}

# get basic apartment info in vector with appropriate labels, and return html table of output
getbasicinfo<- function(inputID, AptPropList){
  tempsub<- unitIDfun(inputID, AptPropList)
  basinf<-c(if(!is.na(tempsub$previous)){paste("Previous Name:",ifn(tempsub$previous))}, 
            if(!is.na(tempsub$Address)){tempsub$Address}, 
            paste(tempsub$CityAddress, ", ",tempsub$State," ", tempsub$PostalCode, sep = ""), 
            if(!is.na(tempsub$SITEPHONE)){paste("Phone:",tempsub$SITEPHONE)}, 
            if(!is.na(tempsub$Fax)){paste("Fax:",tempsub$Fax)}, 
            if(!is.na(tempsub$email)){paste("Email:", tempsub$email)},
            if(!is.na(tempsub$Management)){paste("Management Company:",tempsub$Management)},
            if(!is.na(tempsub$LastName)){paste("Property Manager:",tempsub$FirstName, tempsub$LastName)})
  basinf_df<- data.frame("Contact"=basinf, stringsAsFactors = FALSE)
  colnames(basinf_df)<- lapply(colnames(basinf_df), function(x) paste("  ",gsub("\\.", " ", x),"  "))
  basinfhtml<- htmlTable(basinf_df, rnames=FALSE, align = "left", align.header='l')
  return(basinfhtml)
}

# Get property Info as vector and return as html table

getPropInf0<- function(inputID, AptPropList){
  tempsubset<- unitIDfun(inputID, AptPropList)
  num_unocc<- if (inputID %in% OccMang.df$ID){
    round((1-((OccMang.df$Number.Unoccupied[which(OccMang.df$ID==inputID)]/
                 aptdf$NUMBER[which(aptdf$ID==inputID)])))*100, digits = 2)
  } else {NA}
  pInfo<- c(if(!is.null(tempsubset$SUBMARKET)){paste('Submarket:',ifn(tempsubset$SUBMARKET))},
            # paste("Occupancy: ", num_unocc, if (length(num_unocc)<4 & !is.na(num_unocc)){"%"}, sep = ""),
            if(!is.na(tempsubset$Status)){paste('Status:', ifn(tempsubset$Status))},
            if(tempsubset$Status!= 'Complete'| is.na(tempsubset$Status)){
              paste('Construction Status -', ifn(tempsubset$ConstrStatus))},
            if(!is.na(tempsubset$Financing)){paste("Financing:", 
                                                   ifn(if (tempsubset$Financing=='Subsidized' | tempsubset$Financing=='Tax Credit'){"Affordable"
            } else{"Conventional"}))},
            if(!is.null(tempsubset$Yb)){paste('Year Built:', ifn(tempsubset$Yb))}, 
            if(!is.null(tempsubset$Year_Renovated)){paste('Renovated:', ifn(tempsubset$Year_Renovated))},
            if(!is.null(tempsubset$Number)){paste('Number of Units:', tempsubset$NUMBER)},
            if(!is.null(tempsubset$Ac)){paste('Site Acreage:', ifn(tempsubset$Ac))}
  )
  propdf <- data.frame("Property Information"=""," "=pInfo, stringsAsFactors = FALSE )
  colnames(propdf)<- lapply(colnames(propdf), function(x) gsub("\\.", " ", x))
  prophtml<- htmlTable(propdf, rnames = FALSE, align = "left", align.header = "lc")
  return(gsub("X ", "",prophtml))
}
###########################################################

# Now need the data from the Unit Mix table (in html table form)

## get df subset of unitmix for Date = 12/31/2016 (4th Quarter 2016)
# this date value can be replaced and the entire script can be run for the different dateq

qunitmix <- unitmixall[which(as.character(unitmixall$DATE)==quarterdate), ]

## add fields with weighted size and rent values by number of units
qunitmix$unitXsize<-(qunitmix$Number.Units*qunitmix$Unit.Size)
qunitmix$unitXrent<-(qunitmix$Number.Units*qunitmix$Rent)

## now group by and summarize based on number of bedrooms
unitmix_bdrm<- ddply(qunitmix, c("ID", "Number.Bedrooms"), summarize,
                     num_units=sum(Number.Units),
                     cdate = toString(DATE[1]),
                     UnitXSize=sum(unitXsize),
                     UnitXRent=sum(unitXrent))

#Add fields and calculate average rent, size, and rent psf
unitmix_bdrm$avgRent<- (unitmix_bdrm$UnitXRent/unitmix_bdrm$num_units)
unitmix_bdrm$avgSize<- (unitmix_bdrm$UnitXSize/unitmix_bdrm$num_units)
unitmix_bdrm$Rent_PSF<- (unitmix_bdrm$avgRent/unitmix_bdrm$avgSize)


## Generate table of unit mix averages for all bedrooms, (each apartment ID)
unitmix_apt<- ddply(unitmix_bdrm, "ID", summarize,
                    cDate = (cdate[1]),
                    num_units = sum(num_units),
                    UnitXSize=sum(UnitXSize),
                    UnitXRent=sum(UnitXRent))

# Add fields and calculate average rent, size, and rent psf
unitmix_apt$avgRent<-(unitmix_apt$UnitXRent/unitmix_apt$num_units)
unitmix_apt$avgSize<-(unitmix_apt$UnitXSize/unitmix_apt$num_units)
unitmix_apt$Rent_PSF<- (unitmix_apt$avgRent/unitmix_apt$avgSize)

# Done with Data Prep
############################################################################################
# Converts dataframe to html table output with proper formating for the Unit Mix table
tohtmltab<- function(inputdf){
  rtable<- htmlTable(inputdf, align ="|c|c|c|c|", align.header = "|c|c|c|c|", 
                     rnames = FALSE,css.cell = rbind(rep("background: lightgrey; font-size: 1em; padding-left: .5em; padding-right: .2em;", 
                                                         times=ncol(inputdf)),matrix("", ncol=ncol(inputdf), nrow=nrow(inputdf))))
  return(rtable)
}
# used to pad the table headers:
#http://stackoverflow.com/questions/32896977/how-to-set-padding-in-header-cells-of-htmltable-package


# Master Unit Mix and Apartment Average Unit Mix function:
# uses tohtmltab fucntion
getUnitMixAvg<- function(inputID, unitavgtable, unitmixtable){
  ## note that the unit mix and averages should already be subset by DATE if applicable
  # Both df's subsetted by given Apartment ID
  uavg_ID<- unitIDfun(inputID, unitavgtable)
  umix_ID <- unitIDfun(inputID, unitmixtable)
  # df's displaying data in appropriate format
  uavgdf<- data.frame("Bedrooms"="Overall", 
                      "Units"=ifn(uavg_ID$num_units), 
                      "Average Size"= round(uavg_ID$avgSize),
                      "Average Rent" = paste("$",round(uavg_ID$avgRent),sep = ""),
                      "Rent PSF"= paste("$", round(uavg_ID$Rent_PSF, digits = 2), sep = ""))
  umixdf<- data.frame("Bedrooms"=umix_ID$Number.Bedrooms,
                      "Units"=umix_ID$num_units,
                      "Average Size"= round(umix_ID$avgSize),
                      "Average Rent"= paste("$", round(umix_ID$avgRent), sep = ""),
                      "Rent PSF"= paste("$",round(umix_ID$Rent_PSF, digits = 2), sep = ""))
  uAll<- rbind(umixdf, uavgdf)
  colnames(uAll)<- lapply(colnames(uAll), function(x) gsub("\\.", " ", x))
  uhtml<- tohtmltab(uAll)
  return(uhtml)
}

# List of field names for ALL amenities.- Replace script at end subs for readable naming convention 
amenities <- c('SportsCourt', 'CoffeeBar', 'SandVolleyball', 'DogPark', 'BusinessCenter', 'Spa.HotTub', 'Clubhouse','AlarmSystem',
               'Carport.Garage', 'ControlledAccess', 'WalkingTrail', 'CarWashArea', 'TVLounge', 'Fireplace', 'Playground', 'FitnessCenter',
               'OnSiteLaundry', 'W.DConnections', 'W.DProvided', 'GrillArea', 'GameRoom', 'TanningBeds', 'Lake.Ponds')

getAmenities<- function(inputID, AptPropList){
  amsub<-unitIDfun(inputID, AptPropList)
  numpools<- paste("Number of Pools:", amsub$NumPools)
  amslice<- amsub[amenities]
  amTrue<- amslice[which(amslice[1,]==TRUE)]
  all.amenities<-colnames(amTrue)
  ## Replace poor column headers with english
  all.amenities<- gsub('SportsCourt', 'Sports Court', all.amenities)
  all.amenities<- gsub('CoffeeBar', 'Coffee Bar', all.amenities)
  all.amenities<- gsub('SandVolleyball', 'Sand Volleyball', all.amenities)
  all.amenities<- gsub('DogPark', 'Dog Park', all.amenities)
  all.amenities<- gsub('BusinessCenter', 'Business Center', all.amenities)
  all.amenities<- gsub('Spa.HotTub', 'Spa/Hot Tub', all.amenities)
  all.amenities<- gsub('AlarmSystem', 'Alarm System', all.amenities)
  all.amenities<- gsub('Carport.Garage', 'Carport/Garage', all.amenities)
  all.amenities<- gsub('ControlledAccess', 'Controlled Access', all.amenities)
  all.amenities<- gsub('WalkingTrail', 'Walking Trail', all.amenities)
  all.amenities<- gsub('CarWashArea', 'Car Wash Area', all.amenities)
  all.amenities<- gsub('TVLounge', 'TV Lounge', all.amenities)
  all.amenities<- gsub('FitnessCenter', 'Fitness Center', all.amenities)
  all.amenities<- gsub('OnSiteLaundry', 'On-Site Laundry', all.amenities)
  all.amenities<- gsub('W.DConnections', 'Washer/Dryer Connections', all.amenities)
  all.amenities<- gsub('W.DProvided', 'Washer/Dryer Provided', all.amenities)
  all.amenities<- gsub('GrillArea', 'Grill Area', all.amenities)
  all.amenities<- gsub('GameRoom', 'Game Room', all.amenities)
  all.amenities<- gsub('TanningBeds', 'Tanning Beds', all.amenities)
  all.amenities<- gsub('Lake.Ponds', 'Lake/Ponds', all.amenities)
  #
  ams<-toString(c(numpools, all.amenities))
  amshtml<- htmlTable(ams, rnames=FALSE, caption = "<strong>Amenities:</strong>")
}

#Generate each Each individual vector then bind

NAME<- lapply(aptids, function(x) aptdf$NAME[which(aptdf$ID==x)])
TAB_NAME <- lapply(aptids, function(x) aptdf$Financing[which(aptdf$ID==x)])
DESC1<- lapply(aptids, function(x) getbasicinfo(x, aptdf))
DESC2<- lapply(aptids,function(x) getPropInf0(x, aptdf))
DESC3= lapply(aptids, function(x) if (x %in% qunitmix$ID){
  getUnitMixAvg(x, unitmix_apt, unitmix_bdrm)
} else {
  "Unit Mix Data Not Available"
})
DESC4<- lapply(aptids, function(x) toString(getAmenities(x, aptdf)))
WEBSITE<- "http://www.gnaa.org/"
rooturl<-"https://s3.amazonaws.com/gnaaphotos/"
imgs01<- read.csv("D:\\Projects\\GNAA\\Photos\\Images01.csv", stringsAsFactors = FALSE)
imgs02<- read.csv("D:\\Projects\\GNAA\\Photos\\Images02.csv", stringsAsFactors = FALSE)
PIC_URL<-lapply(aptids, function(x) if(x%in%imgs01$aptID){
  paste(rooturl, imgs01[which(imgs01$aptID==x),]$ImgName,sep = "")
}else{
  paste(rooturl, sample(imgs02$ImgName, 1) ,sep = "")})

THUMB_URL<-lapply(aptids, function(x) if(x%in%imgs02$aptID){
  paste(rooturl, imgs02[which(imgs02$aptID==x),]$ImgName,sep = "")
}else{
  paste(rooturl, sample(imgs01$ImgName, 1) ,sep = "")
})
LAT<- lapply(aptids, function(x) Apt.fc$LAT[which(Apt.fc$ID==x)])
LONG<- lapply(aptids, function(x) Apt.fc$LON[which(Apt.fc$ID==x)])

#Combine all
tdf<- data.frame("ID"=aptids,
                 "NAME"=unlist(NAME),
                 "TAB_NAME"=unlist(TAB_NAME),
                 "DESC1"=unlist(DESC1),
                 "DESC2"=unlist(DESC2),
                 "DESC3"=unlist(DESC3),
                 "DESC4"=unlist(DESC4),
                 "WEBSITE"="http://www.gnaa.org/",
                 "PIC_URL"=unlist(PIC_URL),
                 "THUMB_URL"=unlist(THUMB_URL),
                 "LAT"=unlist(LAT),
                 "LONG"=unlist(LONG)
)
tdf<- tdf[order(tdf$TAB_NAME),]
write.csv(tdf, "Status_shortlist.csv",row.names = FALSE )

