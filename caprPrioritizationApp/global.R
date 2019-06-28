library(shiny)
library(ggtree)
library(dqshiny)
library(data.table)
library(diversitree)

MatchDataObj<- readRDS("AppData/MatchDataObj.rds")
MatchDataObj$data[is.na(MatchDataObj$data$Counties),]$Counties <- ""
CountyCodes <- fread("AppData/tblCNPSCountyCodes_2019-Jun-21_1830.csv")
# 
# # Playing with examples
# 
# famFilter <- which(grepl("Lamiaceae",as.character(MatchDataObj$data$family)))
# #countyFilter <- which(grepl("SDG",as.character(MatchDataObj$data$Counties)))
# instFilter <- which(grepl("SDZG",as.character(MatchDataObj$data$institutions)))
# 
# MatchDataObj$data[Reduce(intersect, list(famFilter,countyFilter,instFilter)),]


MatchDataObj$data$SeedCollYN <- as.integer(MatchDataObj$data$SeedCollYN)-1
MatchDataObj$data$AnyCollYN <- as.integer(MatchDataObj$data$AnyCollYN)-1

# trait.plot(collapse.singles(MatchDataObj$phy), MatchDataObj$data, cols = list(SeedCollYN = c("pink", "red"), 
#                                                              AnyCollYN = c("lightblue", "blue")),cex.lab=0.1)

# 
# input <- NULL
# input$Family <- c("Asteraceae","Fabaceae")
# 
# if(is.null(input$Family)){
#   famFilter <- seq(1,length(MatchDataObj$data$family))
#   }else{
#   famFilter <- which(MatchDataObj$data$family%in%input$Family)
# }
#  countyFilter <- which(MatchDataObj$data$Counties%in%c(""))
# # #countyFilter <- which(grepl(input$countyAuto,as.character(MatchDataObj$data$Counties)))
#  instFilter <- which(grepl('SDZG',as.character(MatchDataObj$data$institutions)))
# # 
# filters <- Reduce(intersect, list(famFilter,countyFilter,instFilter))