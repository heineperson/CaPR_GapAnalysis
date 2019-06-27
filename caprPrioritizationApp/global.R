library(shiny)
library(ggtree)
library(dqshiny)
library(data.table)

MatchDataObj<- readRDS("AppData/MatchDataObj.rds")
MatchDataObj$data[is.na(MatchDataObj$data$Counties),]$Counties <- ""
CountyCodes <- fread("AppData/tblCNPSCountyCodes_2019-Jun-21_1830.csv")

# Playing with examples

famFilter <- which(grepl("Lamiaceae",as.character(MatchDataObj$data$family)))
#countyFilter <- which(grepl("SDG",as.character(MatchDataObj$data$Counties)))
instFilter <- which(grepl("SDZG",as.character(MatchDataObj$data$institutions)))

MatchDataObj$data[Reduce(intersect, list(famFilter,countyFilter,instFilter)),]
