
# MATCH IMAGE FILE NAMES TO APARTMENT NAMES FOR URL GENERATION...
rm(list=ls())
require(stringr)
require(stringdist)

iflen<- function(xx){
  if (length(xx)!=0){
    return(xx)
  }else{
    return(NA)
  }
}
Mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

workdir<- "D:\\Projects\\GNAA\\Photos\\Images"
setwd(workdir)
imglist<- list.files()
workdir<- "D:\\Projects\\GNAA\\Photos"
setwd(workdir)
rooturl<-"https://s3.amazonaws.com/gnaaphotos/"

###################################################
mms<-c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
mmethod <- 'lcs'

###################################################


###################################################
# CAN USE FOLLOWING LINES TO READ APARTMENTS FC FROM ARCGIS PRO

# arc.check_product()
# fc<- arc.open("C:\\Users\\Max\\Documents\\Projects\\GNAA\\AGProGNAA\\AGProGNAA.gdb\\Q1_2017_Apts_rev_Geo_SJoin")
# aptfc.df<- arc.select(fc, names(fc@fields))
# Apartments.df<-aptfc.df[c("USER_ID", "USER_NAME")]

apts.df<- read.csv("D:\\Projects\\GNAA\\Data\\1stQtr2017Data\\2017_03_31_AptsClean.csv", stringsAsFactors = FALSE)
img.df<- data.frame("ImgName"=imglist, "imgsplit"="", stringsAsFactors = FALSE)
revImages<- read.csv("ImagesAll_rev.csv", stringsAsFactors = FALSE)

allmeth = function(instring){
  #mms<-c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
  mms<-c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
  vals<- unlist(sapply(mms, function(xx) amatch(instring, apts.df$NAME, method = xx, maxDist = 2)))
  return (vals)
}
  
mms<-c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
allmatch = function(instring){
  mms<-c("dl","lcs","qgram", "cosine", "jaccard", "jw", "soundex")
  # osa.m<- amatch(instring, apts.df$NAME, method = 'osa', maxDist = 9)
  vals<- unlist(lapply(mms, function(xx) amatch(instring, apts.df$NAME, method = xx, maxDist = 4)))
  modevalue<- Mode(vals)
  return (modevalue)
  }

for (i in 1:nrow(img.df)){
  img.df$imgsplit<-sapply(strsplit(as.character(img.df$ImgName),"\\."),"[",1)
}
img.df$imgname<- gsub("_", " ", str_sub(img.df$imgsplit, 1, -4))
img.df$imgname<- gsub("The\\s|\\sThe|Street|Place", "", img.df$imgname, ignore.case = TRUE)
apts.df$NAME<-   gsub("The\\s|\\sThe|Street|Place", "", apts.df$NAME, ignore.case = TRUE)
# img.df$imgname<- gsub(" The", "", img.df$imgname, ignore.case = TRUE)

img.df$aptindex<- unlist(lapply(img.df$imgname, function(x) allmatch(x)))
#img.df$aptindex<- amatch(img.df$imgname, apts.df$NAME, method = mmethod, maxDist = 10)
img.df$aptID<- sapply(img.df$aptindex, function(x) apts.df$ID[x])
img.df$aptname<- sapply(img.df$aptindex, function(x) apts.df$NAME[x])
img.df$stringdist<- stringdist(img.df$imgname, img.df$aptname, method = mmethod)
img.df$matchtype<- sapply(img.df$aptID, function(x) if(!is.na(x)){'auto'}else{NA})
#img.df
###########################################################################
# ADD MANUAL MATCHES FROM REVIMAGES
# file.rename("Creekwood_I_&_II_01.jpg", "Creekwood_I_&_ll_01.jpg")
# file.rename("Creekwood_I_&_II_02.jpg", "Creekwood_I_&_ll_02.jpg")
# file.rename("1418_E_Main_01.jpg", "1418_E_Main_Street_01.jpg")
# file.rename("1418_E_Main_02.jpg", "1418_E_Main_Street_02.jpg")
img.df<- img.df[order(img.df$stringdist, decreasing = TRUE),]


Images01<-img.df[which(grepl("_01.",as.character(img.df$ImgName))),]
Images02<-img.df[which(grepl("_02.",as.character(img.df$ImgName))),]
write.csv(Images01, "Images01.csv", row.names = FALSE)
write.csv(Images02, "Images02.csv", row.names = FALSE)
write.csv(img.df, "ImagesAll.csv", row.names = FALSE)




