#  Max Marno
# North Line GIS LLC.
#  4/1/2017

# Used to generate the Permanent Unit Mix table - most recent Unit Mix data available for each apartment (ID)
rm(list=ls())
require(readxl)
require(plyr)
require(htmlTable)
quarterdate<-"2017-06-30"  # MAY NEED TO VERIFY DATE FORMAT...

projws= "D:/Projects/GNAA/Data/2ndQtr2017Data"
options(tz='MST')
setwd(projws)

## Now import csv's as data frames
aptdf<- read.csv("2017_06_30_AptsClean.csv", stringsAsFactors = FALSE)
occupancymanagement<- read_excel("D:/Projects/GNAA/Data/2ndQtr2017Data/export/OccupancyManagement.xlsx")
colnames(occupancymanagement)<- gsub(" ", ".", colnames(occupancymanagement))

## clean the Unit Mix table for erroneous values and NA's
OccMan.clean<- occupancymanagement
# Remove all rows with blank or NA values in the DATE column
OccMan.clean<- OccMan.clean[which(nchar(OccMan.clean$OccupancyDate)>1),]
# Convert to date format
OccMan.clean$OccupancyDate<- as.Date(OccMan.clean$OccupancyDate, '%m/%d/%Y')
# remove dates that are listed as after collection date (errors)
OccMan.clean<- OccMan.clean[which(OccMan.clean$OccupancyDate<=quarterdate),]
## Now get frequency  table to inspect which extra collection dates to omit
fqdates<-count(OccMan.clean, 'OccupancyDate')
# fqdates
## Collections Dates are on 3-31, 6-30, 9-30, and 12-31 of every year
## Subset accordingly
UMC_03_31<- OccMan.clean[which(format(OccMan.clean$OccupancyDate, "%m")=="03" & format(OccMan.clean$OccupancyDate, "%d")=="31"),]
UMC_06_30<- OccMan.clean[which(format(OccMan.clean$OccupancyDate, "%m")=="06" & format(OccMan.clean$OccupancyDate, "%d")=="30"),]
UMC_09_30<- OccMan.clean[which(format(OccMan.clean$OccupancyDate, "%m")=="09" & format(OccMan.clean$OccupancyDate, "%d")=="30"),]
UMC_12_31<- OccMan.clean[which(format(OccMan.clean$OccupancyDate, "%m")=="12" & format(OccMan.clean$OccupancyDate, "%d")=="31"),]
OccMan.rbind<-rbind(UMC_12_31, UMC_09_30, UMC_06_30, UMC_03_31)

# create table with only most recent Unit Mix data for each apartment ID
# 'Most Recent' table 
latest<- aggregate(OccMan.rbind$OccupancyDate, list(OccMan.rbind$ID), max)
names(latest)[1]<- "ID"
names(latest)[2]<- "OccupancyDATE"

# subset on latest
mostrecent<- function(x){
  umsub<- OccMan.rbind[which(OccMan.rbind$ID==x & OccMan.rbind$OccupancyDate==latest$OccupancyDATE[which(latest$ID==x)]),]
  return(umsub)
  }

bindfun<- function(inlist){
  tobind<- OccMan.rbind[0,]
  for (id in inlist){
    tobind<-rbind(tobind, mostrecent(id))
  }
  tobind
}
# Run function
PermanentOccMan<- bindfun(latest$ID)
# Write Ouput to csv
write.csv(PermanentOccMan, paste("MostRecentOccMan", quarterdate, ".csv", sep = ""))

