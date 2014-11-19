



calcProf <- function(spotPrice,strikePrice,size, ask){
  value   <- (spotPrice - strikePrice)
  rev     <- size * (value - ask)
  cost    <- ask*size
  profit  <- max(rev-cost,-1* cost)
  return(profit)
}

ask         <- 7.3
size        <- 100
spotPrice   <- 36.93
strikePrice <- 35

##### Test return over a range of spot prices
calcProf(currPrice,strikePrice,size, ask)

prof <- mapply(calcProf,c(20:60),strikePrice,size,ask)
overPrices <- data.frame(price=c(20:60),prof=prof)
plot(overPrices)
####


getQuotes <- function(quotes,selectedTags){
  
  #get lookup file
  link <- "https://gist.githubusercontent.com/devintjones/7025db970719051a0c57/raw/abe19ec170850a5fd20d1c63d0d1108b731d6d02/props"
  temporaryFile <- tempfile()
  download.file(link,destfile=temporaryFile)
  props <- read.delim(temporaryFile,stringsAsFactors=FALSE)
  
  selectedTags <- data.frame(name=selectedTags)
  tags <- merge(selectedTags,props,by="name")
  
  url <- paste0("http://download.finance.yahoo.com/d/quotes.csv?s="
                ,paste(quotes,collapse=",")
                ,"&f="
                ,paste(tags$tag,collapse="")
                ,"&e=.csv")
  
  require(RCurl)
  response        <- getURL(url)
  data <- read.csv(text=response,header=F)
  
  names(data)     <- tags$name
  row.names(data) <- NULL
  data$Ask        <- as.numeric(data$Ask)
  data$Bid        <- as.numeric(data$Bid)
  return(data)
}

quotes <- c("GOOG","AAPL")
selectedTags <- c("Name","Symbol","Ask","Bid")

option <- c("MSFT150117C00040000")
getYQL(option)


getYQL <- function(quotes){
  baseURL <- "http://query.yahooapis.com/v1/public/yql"
  yql_query <- "select * from yahoo.finance.quotes where symbol in (\"!sub!\")"
  yql_query <- gsub("!sub!",paste(quotes,collapse="\",\""),yql_query)
  YQL <- URLencode(paste0(baseURL,
                          "?q=",
                          yql_query,
                          "&format=json&env=store://datatables.org/alltableswithkeys"))
  
  response <- getURL(YQL)
  
  require(rjson)
  query <- fromJSON(response)[["query"]]
  results <- query[["results"]]

  data <- crunchList(results[["quote"]])
  data
}

start <- proc.time()
data<- getYQL(quotes)
proc.time()-start

start <- proc.time()
data <- suppressMessages(getQuotes(quotes,selectedTags))
proc.time()-start


raw_data <- getURL("http://www.google.com/finance/option_chain?q=AAPL&output=json")
raw_data <- gsub("(\\w+)\\s*:",'"\\1":',raw_data) 
require(rjson)
data <- fromJSON(raw_data)

crunchList <- function(list){
  require(plyr)
  data <- rbind.fill(lapply(list, function(f) {
    as.data.frame(Filter(Negate(is.null), f))
  }))
  data
}



puts  <- crunchList(data[["puts"]])
calls <- crunchList(data[["calls"]])

underlying_id <- data[["underlying_id"]]
underlying_price <- data[["underlying_price"]]

expirations <- crunchList(data[["expirations"]])





