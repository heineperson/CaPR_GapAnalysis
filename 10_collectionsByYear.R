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

cnps <- caspio_get_table_all("tblCNPSRanks")

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

CaPRAcc <- merge(CaPRAcc,cnps,by.x="taxonID",by.y="JepID",all.x=T)

CaPRAcc[,Date:=gsub("T00:00:00","",eventDate)]
CaPRAcc[,Date:=as.Date(Date,"%Y-%m-%d")]
CaPRAcc[,Year:=year(Date)]
CaPRAcc[,inst:=collectingInstituteCode]
CaPRAcc[inst=="",inst:=institutionCode]


CaPRAcc[grepl("Seed",basisofRecord) & Year>2015,.(Count=.N,SppCount=uniqueN(taxonID), EOCount=uniqueN(cnddbEOIndex)),by=c("inst","Year")]

CaPRAcc[grepl("Seed",basisofRecord) & Year>2015,.(Count=.N,SppCount=uniqueN(taxonID), RareSppCount= uniqueN(taxonID[!is.na(CRPR)]), EOCount=uniqueN(cnddbEOIndex)),by=c("inst","Year")][order(inst,Year)]
