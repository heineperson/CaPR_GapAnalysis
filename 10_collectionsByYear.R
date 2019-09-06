library(data.table)
library(DT)
library(lubridate)
library(httr)
library(RCurl)
library(rjson)
library(jsonlite)

# Reading in Caspio Token
source("Tokens/caspioSource.R")
# Reading in caspio GET command (don't know if this works for things > 1000)
source("caspioFunctions.R")

tableNew <- caspio_get_view("caprViewNoLists",login1,1)
TableCombined = NULL
pagenumber=1

while (dim(tableNew)[1]>0)
{
  pagenumber = pagenumber + 1
  TableCombined <- rbind(TableCombined,tableNew)
  tableNew <- caspio_get_view("caprViewNoLists",login1,pagenumber)
}

CaPRAcc <- TableCombined

CaPRAcc[,Date:=gsub("T00:00:00","",eventDate)]
CaPRAcc[,Date:=as.Date(Date,"%Y-%m-%d")]
CaPRAcc[,Year:=year(Date)]

CaPRAcc[,Year:=]
CaPRAcc[grepl("Seed",basisofRecord) & Year>2015,.(Count=.N,SppCount=uniqueN(taxonID)),by=c("collectingInstituteCode","Year")]