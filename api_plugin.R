library("curl")
library("XML")

wd <- "C:/git/housing-analysis/"
setwd(wd)

zws.id <- readLines("zwsid.txt")

GetSearchResults <- function(address,citystatezip,rentzestimate=FALSE){
  base <- "http://www.zillow.com/webservice/GetSearchResults.htm"
  url <- paste(base,zws.id,sep="?zws-id=")
  url <- paste(url,URLencode(address),sep="&address=")
  url <- paste(url,URLencode(citystatezip),sep="&citystatezip=")
  if(rentzestimate){
    url <- paste(url,"true",sep="&rentzestimate=")
  }
  con <- curl(url)
  open(con)
  text <- readLines(curl(url))
  data <- xmlParse(text)
  xml_data <- xmlToList(data)
  closeAllConnections()
  return(xml_data)
}

GetDeepComps <- function(zpid,count=1,rentzestimate=FALSE){
  base <- "http://www.zillow.com/webservice/GetDeepComps.htm"
  url <- paste(base,zws.id,sep="?zws-id=")
  url <- paste(url,zpid,sep="&zpid=")
  url <- paste(url,count,sep="&count=")
  if(rentzestimate){
    url <- paste(url,"true",sep="&rentzestimate=")
  }
  con <- curl(url)
  open(con)
  text <- readLines(curl(url))
  data <- xmlParse(text)
  xml_data <- xmlToList(data)
  closeAllConnections()
  return(xml_data)
}

GetChart <- function(zpid,chartDuration="10years",unitType="dollar"){
  base <- "http://www.zillow.com/webservice/GetChart.htm"
  url <- paste(base,zws.id,sep="?zws-id=")
  url <- paste(url,zpid,sep="&zpid=")
  url <- paste(url,unitType,sep="&unit-type=")
  url <- paste(url,chartDuration,sep="&chartDuration=")
  con <- curl(url)
  open(con)
  text <- readLines(curl(url))
  data <- xmlParse(text)
  xml_data <- xmlToList(data)
  closeAllConnections()
  return(xml_data)
}

GetPriceDF <- function(address,citystatezip){
  result <- GetSearchResults(address,citystatezip)$response$results$result
  property <- GetDeepComps(result$zpid)$response$properties$principal
  zestimate <- as.double(property$zestimate$amount$text)
#   zlow <- as.double(property$zestimate$valuationRange$low$text)
#   zhigh <- as.double(property$zestimate$valuationRange$high$text)
  z.date <- as.Date(property$zestimate$`last-updated`,format="%m/%d/%Y")
  tax.assessment <- as.double(property$taxAssessment)
  t.date <- as.Date(paste0("01/01/",property$taxAssessmentYear),format="%m/%d/%Y")
  sold.price <- as.double(property$lastSoldPrice$text)
  s.date <- as.Date(property$lastSoldDate,format="%m/%d/%Y")
  v.change <- as.double(property$zestimate$valueChange$text)
  change.price <- zestimate-v.change
  date.change <- as.double(property$zestimate$valueChange$.attrs[["duration"]])
  c.date <- z.date-date.change
  price <- c(zestimate,change.price,tax.assessment,sold.price)
  date <- c(z.date,c.date,t.date,s.date)
  type <- c("zEstimate","zEstimate Change","Tax assessment","Last sale price")
  df <- data.frame(price,date,type,result$address)
  df <- df[order(df$date),]
  return(df)
}


# df <- GetPriceDF("600 Roosevelt Blvd., Apt 404","22044")
# plot(price~date,df,type="o")
# 
# df2 <- GetPriceDF("13123 Lazy Glen Ct.","20171")
# plot(price~date,df2,type="o")
# 
# df3 <- GetPriceDF("3859 Appaloosa Drive","22192")
# plot(price~date,df3,type="o")
