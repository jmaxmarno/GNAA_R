###


###
# GET ALL CONCESSIONS IN ONE FIELD BY SUBMARKET
require(arcgisbinding)
require(dplyr)
arc.check_product()
subm.fc<- arc.open("D:\\Projects\\Tennessee\\AGProGNAA\\AGProGNAA.gdb\\GNAA_NewSubmarkets")
subm.fc<- arc.select(subm.fc, fields = c('Shape', 'GNAA_NewSubmarkets_NAME'))

fc<- arc.open("D:\\Projects\\Tennessee\\AGProGNAA\\AGProGNAA.gdb\\Apts_2017Q1")
aptfc.df<- arc.select(fc, names(fc@fields))
aptcon<- aptfc.df[c("ID", "SUBMARKET", "Concessions")]
aptcon<-filter(aptcon, Concessions!='NA')

concessions<- lapply(aptfc.df$Concessions, function(x) if(nchar(x)>4 & !is.na(x) & as.character(x)!= "NA" & is.null(x)==FALSE){
  x
}
)

SConcessions.df<- ddply(aptcon, c("SUBMARKET"), summarize,
                                Conc=paste("<p>",Concessions, "</p>", collapse = "  "))
SConcessions.df$Conc<-gsub("  ", "\\\n", SConcessions.df$Conc)

write.csv(SConcessions.df, "SubmarketConcessionsQ1_2017.csv", row.names = FALSE)

#Testing to write output directly to gdb as feature class - bad row value in [Conc] - could be length of characters
# subm.fc<- arc.data2sp(subm.fc)
# subm.fc@data<- merge(subm.fc@data, SConcessions.df, by.x = 'GNAA_NewSubmarkets_NAME', by.y = 'SUBMARKET', all.x = TRUE)
# subm.fc@data$Conc<- gsub("\"", "\"\'\"", subm.fc@data$Conc)
# subm.fc@data$Conc<- as.character(unlist(subm.fc@data$Conc))
# #subm.fc@data$Conc<- "TEST"
# arc.write("D:\\Projects\\Tennessee\\AGProGNAA\\AGProGNAA.gdb\\Submarkets_w_Concessions", subm.fc)

