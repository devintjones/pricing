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


library(RCurl)




