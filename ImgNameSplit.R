##  Max Marno
# North Line GIS LLC.
#  9/14/2017
# 
# MATCH IMAGE FILE NAMES TO APARTMENT NAMES FOR URL GENERATION...
rm(list=ls())
require(stringi)
require(stringdist)

###########   INPUT   ###########
ImagesFolder<- "D:\\Projects\\GNAA\\Photos\\Images"
workdir<- "D:\\Projects\\GNAA\\Photos"
# REVISED LIST OF IMAGES FROM PREVIOUS RUN
RevisedImages <- "ImagesAll_rev.csv"
# APARTMENTS PROPERTY TABLE:
AptPropList <- "D:\\Projects\\GNAA\\Data\\2ndQtr2017Data\\2017_06_30_AptsClean.csv"
# ROOT URL TO BE PREPENDED TO IMAGE NAME:
rooturl<-"https://s3.amazonaws.com/gnaaphotos/"
###############   DONE  ###############
setwd(workdir)
imglist<- list.files(path = ImagesFolder)
###################################################
# ALL STRINGDIST METHODS/METRICS:
# mms<-c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
# INCLUDED AS A REFERENCE IN OUTPUT TABLE:
mmethod <- 'lcs'
###################################################
# NO BASE MODE FUNTION IN R
mode_fun <- function(input) {
  uniqv <- unique(input)
  uniqv[which.max(tabulate(match(input, uniqv)))]}
# READ DATA
RevImgs<- read.csv(RevisedImages, stringsAsFactors = FALSE)
apts.df<- read.csv(AptPropList, stringsAsFactors = FALSE)
# READ IMAGE FILENAMES NOT ALREADY PRESENT IN 'REVIMGS' (THEN WILL APPEND)
img.df<- data.frame("ImgName"=imglist, "imgsplit"="", stringsAsFactors = FALSE)
# FUNCTION TO RETURN BEST MATCH BETWEEN IMAGE NAME AND APARTMENT COMMUNITIES
allmatch_fun = function(instring){
  mms<-c("dl","lcs","qgram", "cosine", "jaccard", "jw", "soundex")
  # osa.m<- amatch(instring, apts.df$NAME, method = 'osa', maxDist = 9)
  vals<- unlist(lapply(mms, function(xx) amatch(instring, apts.df$NAME, method = xx, maxDist = 4)))
  modevalue<- mode_fun(vals)
  return (modevalue)}
# USING '[' IN SAPPLY/LAPPLY ALLOWS YOU TO EXTRACT INDEXED ELEMENT, (IN THIS CASE 1) FROM A LIST OF CHARACTER VECTORS WITH MULTIPLE ELEMENTS
for (i in 1:nrow(img.df)){
  img.df$imgsplit<-sapply(strsplit(as.character(img.df$ImgName),"\\."),"[",1)
}
# REPLACE '_' WITH ' ', AND CLIP LAST 4 CHARACTERS OF IMAGE NAME, IE THE '.JPG'
img.df$imgname<- gsub("_", " ", stri_sub(img.df$imgsplit, 1, -4))
# REPLACE GENERIC WORDS IN IMAGENAME, NOTE \\s CAN BE USED AS REGEX FOR SPACE
img.df$imgname<- gsub("The\\s|\\sThe|Street|Place", "", img.df$imgname, ignore.case = TRUE)
apts.df$StrName<-   gsub("The\\s|\\sThe|Street|Place", "", apts.df$NAME, ignore.case = TRUE)
# GET INDEX OF APARTMENT COMM THAT BEST MATCHES IMAGE NAME - allmatch_fun
img.df$aptindex<- unlist(lapply(img.df$imgname, function(x) allmatch_fun(x)))
# BELOW CAN BE USED TO RUN USING ONLY ONE STRINGDIST METRIC
#img.df$aptindex<- amatch(img.df$imgname, apts.df$NAME, method = mmethod, maxDist = 10)

img.df$aptID<- sapply(img.df$aptindex, function(x) apts.df$ID[x])
img.df$aptstrname<- sapply(img.df$aptindex, function(x) apts.df$StrName[x])
img.df$fName<- sapply(img.df$aptindex, function(x) apts.df$NAME[x])
img.df$stringdist_lcs<- stringdist(img.df$imgname, img.df$aptstrname, method = mmethod)
img.df$matchtype<- sapply(img.df$aptID, function(x) if(!is.na(x)){'auto'}else{NA})

img.df<- img.df[order(img.df$stringdist, decreasing = TRUE),]

# BIND NEW RECORDS WITH EXISTING DATAFRAME FROM REVIMGS
img.df<- img.df[!which(img.df$ImgName %in% RevImgs$ImgName),]
img.df<- rbind(RevImgs, img.df)

Images01<-img.df[which(grepl("_01.",as.character(img.df$ImgName))),]
Images02<-img.df[which(grepl("_02.",as.character(img.df$ImgName))),]
write.csv(Images01, "Images01.csv", row.names = FALSE)
write.csv(Images02, "Images02.csv", row.names = FALSE)
write.csv(img.df, "ImagesAll.csv", row.names = FALSE)




