



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

crunchList <- function(list){
  require(plyr)
  data <- rbind.fill(lapply(list, function(f) {
    as.data.frame(Filter(Negate(is.null), f))
  }))
  data
}



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


###speed test for csv & lookup vs json
start <- proc.time()
data<- getYQL(quotes)
proc.time()-start

start <- proc.time()
data <- suppressMessages(getQuotes(quotes,selectedTags))
proc.time()-start
#### json/yql wins



getOptionsChain <- function(quote){
  #can pass only 1 quote at a time
  url <- paste0("http://www.google.com/finance/option_chain?q=",
                quote,
                "&output=json")
  #more params: &expy=2014&expm=9&expd=20
  
  require(RCurl)
  raw_data <- getURL(url)
  
  #fix json structure
  raw_data <- gsub("(\\w+)\\s*:",'"\\1":',raw_data) 
  
  require(rjson)
  data     <- fromJSON(raw_data)

  underlying_id    <- data[["underlying_id"]]
  underlying_price <- data[["underlying_price"]]
  
  puts        <- crunchList(data[["puts"]])
  calls       <- crunchList(data[["calls"]])
  expirations <- crunchList(data[["expirations"]])
  
  optionsChain <- list(quote=underlying_id,
                       underlying_price=underlying_price,
                       puts=puts,calls=calls,expirations=expirations)
  return(optionsChain)
}

quote = "AAPL"
getOptionsChain(quote)


