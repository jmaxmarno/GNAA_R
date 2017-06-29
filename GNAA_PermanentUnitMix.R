#  Max Marno
# North Line GIS LLC.
#  4/1/2017

# Used to generate the Permanent Unit Mix table - most recent Unit Mix data available for each apartment (ID)
rm(list=ls())
require(readxl)
require(plyr)
require(htmlTable)
quarterdate<-"2017-03-31"  # MAY NEED TO VERIFY DATE FORMAT...

projws= "D:/Projects/GNAA/GNAA_R"
options(tz='MST')
setwd(projws)

## Now import csv's as data frames
aptdf<- read.csv("D:/Projects/GNAA/Data/1stQtr2017Data/2017_03_31_AptsClean.csv", stringsAsFactors = FALSE)
# List of desired apartment ID's
apids<- aptdf$ID
unitmixall<- read_excel("D:/Projects/GNAA/Data/1stQtr2017Data/export/UnitMixandRents.xlsx")
colnames(unitmixall)<- gsub(" ", ".", colnames(unitmixall))
occupancymanagement<- read_excel("D:/Projects/GNAA/Data/1stQtr2017Data/export/OccupancyManagement.xlsx")
colnames(occupancymanagement)<- gsub(" ", ".", colnames(occupancymanagement))

## clean the Unit Mix table for erroneous values and NA's
UnitMix.clean<- unitmixall
# Remove all rows with blank or NA values in the DATE column
UnitMix.clean<- UnitMix.clean[which(nchar(UnitMix.clean$DATE)>1),]
# Convert to date format
UnitMix.clean$DATE<- as.Date(UnitMix.clean$DATE, '%m/%d/%Y')
# remove dates that are listed as after collection date (errors)
UnitMix.clean<- UnitMix.clean[which(UnitMix.clean$DATE<=quarterdate),]
## Now get frequency  table to inspect which extra collection dates to omit
fqdates<-count(UnitMix.clean, 'DATE')
# fqdates
## Collections Dates are on 3-31, 6-30, 9-30, and 12-31 of every year
## Subset accordingly
UMC_03_31<- UnitMix.clean[which(format(UnitMix.clean$DATE, "%m")=="03" & format(UnitMix.clean$DATE, "%d")=="31"),]
UMC_06_30<- UnitMix.clean[which(format(UnitMix.clean$DATE, "%m")=="06" & format(UnitMix.clean$DATE, "%d")=="30"),]
UMC_09_30<- UnitMix.clean[which(format(UnitMix.clean$DATE, "%m")=="09" & format(UnitMix.clean$DATE, "%d")=="30"),]
UMC_12_31<- UnitMix.clean[which(format(UnitMix.clean$DATE, "%m")=="12" & format(UnitMix.clean$DATE, "%d")=="31"),]
UnitMix.rbind<-rbind(UMC_12_31, UMC_09_30, UMC_06_30, UMC_03_31)

# create table with only most recent Unit Mix data for each apartment ID
# 'Most Recent' table 
latest<- aggregate(UnitMix.rbind$DATE, list(UnitMix.rbind$ID), max)
names(latest)[1]<- "ID"
names(latest)[2]<- "DATE"

# subset on latest
mostrecent<- function(x){
  umsub<- UnitMix.rbind[which(UnitMix.rbind$ID==x & UnitMix.rbind$DATE==latest$DATE[which(latest$ID==x)]),]
  umsub}

bindfun<- function(inlist){
  tobind<- UnitMix.rbind[0,]
  for (id in inlist){
    tobind<-rbind(tobind, mostrecent(id))
  }
  tobind
  }
# Run function
PermanentUnitMix<- bindfun(latest$ID)
# Frequency count of ID's for # of unit styles per ID
idfreq<- count(PermanentUnitMix, 'ID')
# Add freqency count for number of unit styles (for use in Survey123)
PermanentUnitMix<- merge(PermanentUnitMix, idfreq, by="ID")

# add 'style' ID as unique identifier based on the unit ID as SID
PermanentUnitMix<-ddply(PermanentUnitMix, .(ID), mutate, SID=seq_along(PlanId))
PermanentUnitMix$SID<- paste(PermanentUnitMix$ID, "_", PermanentUnitMix$SID, sep = "")

# Write Ouput to csv

write.csv(PermanentUnitMix, "MostRecentUnitMix.csv")

