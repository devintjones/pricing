



valueCall <- function(spotPrice,strikePrice,ask){
  value   <- (spotPrice - strikePrice)
  rev     <- (value - ask)/ask
  profit  <- max(rev,-1)
  return(profit)
}
valueCall <- Vectorize(valueCall)



ask         <- 7.3
spotPrice   <- 36.93
strikePrice <- 35

##### Test return over a range of spot prices
prof <- mapply(valueCall,c(20:60),strikePrice,ask)
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

  #underlying_id    <- data[["underlying_id"]]
  #underlying_price <- data[["underlying_price"]]
  
  puts        <- crunchList(data[["puts"]])
  puts        <- cbind(type="put",puts)
  calls       <- crunchList(data[["calls"]])
  calls       <- cbind(type="call",calls)
  
  optionsDF   <- rbind(puts,calls)
  optionsDF   <- cbind(quote,optionsDF)
  
  optionsDF   <- within(optionsDF,{
    a      <- as.numeric(as.character(a))
    strike <- as.numeric(as.character(strike))
    expiry <- as.Date(expiry, "%B %d, %Y")
    })
  
  return(optionsDF)
}

getOptionsDF <- function(quotes){
  optionsList <- lapply(quotes,getOptionsChain)
  optionsDF   <- do.call("rbind",optionsList)
  return(optionsDF)
}
quotes <- c("BAC","JPM")
optionsDF <- getOptionsDF(quotes)


getHistoricalQuotes <- function(quotes,
                                startDate,
                                endDate){
  startDate <- as.character(startDate)
  endDate   <- as.character(endDate)
  
  url <- paste0("http://query.yahooapis.com/v1/public/yql?q=",
                "select * from yahoo.finance.historicaldata where symbol in ( \"!sub!\" ) and startDate = \"",startDate,"\" and endDate = \"",endDate,"\"",
                "&format=json&env=store://datatables.org/alltableswithkeys")
  
  url <- gsub("!sub!",paste(quotes,collapse="\",\""),url)
  url <- URLencode(url)
  
  require(RCurl)
  raw_data <- getURL(url)
  require(rjson)
  data     <- fromJSON(raw_data)
  query    <- data[["query"]]
  
  count    <- query[["count"]]
  created  <- query[["created"]]
  lang     <- query[["lang"]]
  results  <- query[["results"]]
  
  quote    <- results[["quote"]]
  
  data     <- crunchList(quote)
  data     <- within(data,{
    Close <- as.numeric(levels(Close))[Close]
    Open  <- as.numeric(levels(Open))[Open]
    High  <- as.numeric(levels(High))[High]
    Low   <- as.numeric(levels(Low))[Low]
    Volume<- as.numeric(levels(Volume))[Volume]
    Adj_Close <- as.numeric(levels(Adj_Close))[Adj_Close]
    Date  <- as.Date(Date)  
  })

  return(data)
}

quotes        <- c("BAC","JPM")
endDate       <- Sys.Date()
startDate     <- endDate -30
historical <- getHistoricalQuotes(quotes,startDate,endDate)

plotHistorical <- function(historical){
  require(ggplot2)
  plot <- ggplot(data=historical,aes(x=Date,y=Close,color=Symbol)) + geom_line()
  plot
}

lagpad <- function(x,k){
  if(k > 0) {
    newX <- c(rep(NA,k),x[1:length(x)-k])
  } else if (k < 0) {
    newX <- c(x[1+abs(k):length(x)]) }
  return(newX)
}


getParams <- function(historical){
  detach("package:dplyr")
  suppressPackageStartupMessages(library(dplyr))
  dplyrHist <- historical %>% 
                arrange(Symbol, desc(Date)) %>% 
                group_by(Symbol) %>% 
                mutate(lagClose = lagpad(Close,-1),
                       diff  = Close-lagClose ) %>%
                summarise(stdev = sd(Close,na.rm=T),
                          drift = mean(diff,na.rm=T),
                          days  = n(),
                          date  = max(Date),
                          price = first(Close))
  return(dplyrHist)
}

params <- getParams(historical)

#Geometric Brownian Motion.
#http://en.wikipedia.org/wiki/Geometric_Brownian_motion
GBM <- function(price,drift,stdev,t=1){
  price * exp( ( drift - .5*stdev**2 ) * t + stdev * rnorm(1,0,t) )
}

#Simpler GBM
#http://www.columbia.edu/~ks20/FE-Notes/4700-07-Notes-GBM.pdf
#http://www.r-bloggers.com/quantitative-finance-applications-in-r-5-an-introduction-to-monte-carlo-simulation/
simpleGBM <- function(price,drift,stdev,t=1){ #t is in years
  rands <- rnorm(1e4)
  priceSim <- price * exp(  drift * t + stdev * rands * sqrt(t) )
  return(priceSim)
}


#expect formate from optionsDF$expiry
countBizDays <- function(exp){ 
  require(timeDate)
  today     <- Sys.Date()
  inBetween <- timeDate(seq(today,exp,"days"))
  numBizDays<- length(which(isBizday(inBetween)))
  return(numBizDays)
}
days <- countBizDays(unique(optionsDF$expiry))


priceSim <- with(params, mapply(simpleGBM, price, drift, stdev,days/365,SIMPLIFY=F))
names(priceSim) <- unique(params$Symbol)
#hist(priceSim[["BAC"]])



callUtil <- function(spotPrice,strikePrice,ask){
  max( ((spotPrice - strikePrice) - ask)/ask,-1)
}


projectUtility <- function(sym,optionsDF,priceSim){
  calls       <- subset(optionsDF,quote == sym & type == "call")
  spot        <- priceSim[[sym]]

  makeData <- function(strike,ask){
    cbind(spot,strike,ask)
  }
  
  data        <- mapply(makeData,calls$strike, calls$a,SIMPLIFY=F)
  data        <- data.frame(do.call("rbind",data))
  #names(data) <- c("spot","strike","ask")
  data        <- within(data, utility <- mapply(callUtil,spot,strike,ask))
}

sym <- "BAC"
data <- projectUtility(sym,optionsDF,priceSim)



expectedUtility <- data %>%
  group_by(strike,ask) %>%
  summarize(eU = mean(utility)) %>%
  ungroup() %>%
  arrange(desc(eU))

expectedUtility




