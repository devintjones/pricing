ask         <- 7.3
size        <- 100
currPrice   <- 36.93
strikePrice <- 35



calcProf <- function(futurePrice,strikePrice,size, ask){
  value   <- (futurePrice - strikePrice)
  rev     <- size * (value - ask)
  cost    <- ask*size
  profit  <- max(rev-cost,-1* cost)
  return(profit)
}
futurePrice <- currPrice
calcProf(currPrice,strikePrice,size, ask)

prof <- mapply(calcProf,c(20:60),strikePrice,size,ask)

overPrices <- data.frame(price=c(20:60),prof=prof)

plot(overPrices)


quotes <- c("GOOG","AAPL")

selectedTags <- c("Name","Symbol","Ask","Bid")

getQuotes <- function(quotes,selectedTags){
  require(RCurl)
  
  props <- read.delim("~/r_workspace/props.txt",stringsAsFactors=FALSE)
  
  selectedTags <- data.frame(name=selectedTags)
  tags <- merge(selectedTags,props,by="name")
  
  url <- paste0("http://download.finance.yahoo.com/d/quotes.csv?s="
                ,paste(quotes,collapse=",")
                ,"&f="
                ,paste(tags$tag,collapse="")
                ,"&e=.csv")
  
  response        <- getURL(url)
  #clean data
  responseCln     <- gsub("\\\\|\"|\n","",response)
  rows            <- data.frame(strsplit(responseCln,"\r")[[1]],stringsAsFactors=FALSE)
  rowList         <- sapply(rows[,1],function(x) strsplit(x,","))
  data            <- data.frame(do.call("rbind",rowList),stringsAsFactors=FALSE)
  names(data)     <- tags$name
  row.names(data) <- NULL
  data$Ask        <- as.numeric(data$Ask)
  data$Bid        <- as.numeric(data$Bid)
  return(data)
}

data <- getQuotes(quotes,selectedTags)