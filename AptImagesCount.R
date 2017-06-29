# GENERATE TABLE TO IDENTIFY MISSING IMAGES & THUMBS FOR EACH APARTMENT COMMUNITY

rm(list=ls())
require(readxl)
require(dlyr)
require(htmlTable)
require(arcgisbinding)

quarterdate<-"2017-03-31"  # MAY NEED TO VERIFY DATE FORMAT...

projws= "D:/Projects/Tennessee/RProj"
setwd(projws)

## Now import csv's as data frames
aptdf<- read.csv("D:/Projects/Tennessee/GNAA/Data/1stQtr2017Data/2017_03_31_AptsClean.csv", stringsAsFactors = FALSE)
# List of desired apartment ID's

imgs<- read.csv("D:\\Projects\\Tennessee\\Photos\\ImagesAll_rev.csv", stringsAsFactors = FALSE)
imgs01<-imgs[which(grepl("_01.",as.character(imgs$ImgName))),]
imgs02<-imgs[which(grepl("_02.",as.character(imgs$ImgName))),]

IM01<- sapply(aptdf$ID, function(x) (x%in%imgs01$aptID))
IM02<- sapply(aptdf$ID, function(x) (x%in%imgs02$aptID))

img.df<- data.frame('AptID'=aptdf$ID, 'AptName'= aptdf$NAME, 'IMG_01'=IM01, 'IMG_02'=IM02)
write.csv(img.df, "ApartmentImages.csv", row.names = FALSE)

nrow(dplyr::filter(img.df, IMG_01==TRUE | IMG_02==TRUE))
nrow(dplyr::filter(img.df, IMG_02==TRUE ))
nrow(dplyr::filter(img.df, IMG_01==TRUE ))
img.df[which(img.df$IMG_01 != img.df$IMG_02),]


