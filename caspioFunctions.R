## Caspio Functions

# Exctracting Table from Caspio API
# Deafut number of pages is one
caspio_get_table = function(TableName,login1,pagenumber=1){
  # This table reads the table defined by TableName into R
  # Query parameters specify the row limit (default = 100 & max = 1000)
  # url1 = sprintf("https://c4axa460.caspio.com/rest/v1/tables/%s/rows/?q={%s:%s,limit:1000}",TableName,
  #                shQuote("orderby"),shQuote("PK_ID"))
  url1 = sprintf("https://c4axa460.caspio.com/rest/v1/tables/%s/rows/?q={%s:%s,%s:1000}",TableName,
                                 shQuote("pageNumber"),pagenumber, shQuote("pageSize"))
  print(url1)
  table_get = GET(url = url1,
                  #url=paste("https://c4axa460.caspio.com/rest/v1/tables/",TableName,"/rows?q={limit:1000,'where':",query,"}",sep=""),
                  add_headers(Authorization=paste("Bearer", content(login1)$access_token),Accept="application/json"))
  #,query=list(Genus_ITIS="Abronia"))
  
  # This converts JSON Object to text
  table_json = content(table_get, as="text")
  # This converts JSON Object to dataframe
  table_dataframe = jsonlite::fromJSON(table_json,simplifyDataFrame=TRUE)
  #This converts dataframe to data.table
  table_DT = as.data.table(table_dataframe$Result)
  
  #while(dim(table_DT)[1]>=1000)
    return(table_DT)
  print(dim(table_DT))
  
}


# Extracting View from Caspio API
caspio_get_view = function(TableName,login1,pagenumber=1){
  # This table reads the table defined by TableName into R
  # Query parameters specify the row limit (default = 100 & max = 1000)
  
  url1 = sprintf("https://c4axa460.caspio.com/rest/v1/views/%s/rows/?q={%s:%s,%s:1000}",TableName,
                 shQuote("pageNumber"),pagenumber, shQuote("pageSize"))
  print(url1)
  table_get = GET(url = url1,
                  add_headers(Authorization=paste("Bearer", content(login1)$access_token),Accept="application/json"))

  # This converts JSON Object to text
  table_json = content(table_get, as="text")
  # This converts JSON Object to dataframe
  table_dataframe = jsonlite::fromJSON(table_json,simplifyDataFrame=TRUE)
  #This converts dataframe to data.table
  table_DT = as.data.table(table_dataframe$Result)
  
  #while(dim(table_DT)[1]>=1000)
  return(table_DT)
  print(dim(table_DT))
  
}

# Caspio Getting multiple pages for tables
caspio_get_table_all = function(TableName){
  
  tableNew <- caspio_get_table(TableName,login1,1)
  TableCombined = NULL
  pagenumber=1
  
  while (dim(tableNew)[1]>0)
  {
    pagenumber = pagenumber + 1
    TableCombined <- rbind(TableCombined,tableNew)
    tableNew <- caspio_get_table(TableName,login1,pagenumber)
  }
  
  DatReturn <- TableCombined
  return(DatReturn)
}

# Caspio getting view pages
caspio_get_view_all = function(ViewName){
  
  tableNew <- caspio_get_view(ViewName,login1,1)
  TableCombined = NULL
  pagenumber=1
  
  while (dim(tableNew)[1]>0)
  {
    pagenumber = pagenumber + 1
    TableCombined <- rbind(TableCombined,tableNew)
    tableNew <- caspio_get_view(ViewName,login1,pagenumber)
  }
  
  DatReturn <- TableCombined
  return(DatReturn)
  
}

