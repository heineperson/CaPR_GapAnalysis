library(data.table)
library(DT)
library(stringr)
library(lubridate)
library(httr)
library(RCurl)
library(rjson)
library(jsonlite)

# Reading in Caspio Token
source("Tokens/caspioSource.R")
# Reading in caspio GET command (don't know if this works for things > 1000)
source("caspioFunctions.R")

# Reading in Data
sdzgOcc <- caspio_get_table_all("Occurrence")
sdzgSpp <- caspio_get_table_all("PlantNames")
caprSpp <- caspio_get_table_all("tblSpeciesJepCAPR")
