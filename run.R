library(quantmod)
library(dplyr)
library(stringr)
# TickerComm <- c("JJA","COW","JO","OIL","UNG","GDX")
# getSymbols("OIL",from = as.Date("2000-01-04"), to = as.Date("2017-01-01"))
# data <- new.env()
# getSymbols.extra(c('RWX','VNQ','VGSIX'), src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
# stock = new.env()
# getSymbols.yahoo(Symbols = '^GSPC',env = stock)

#################
# IMPORT DATA
#################
TickerEquity <- c("^GSPC","^FTSE","^STOXX50E","VEIEX","^N225")
TickerBond <- c("VBMFX","VWESX","VWEHX","VFSTX")
TickerComm <- c("JJA","COW","JO","OIL","UNG","GDX")
TickerRealEst <- c("VGSIX")
Tickers <- c(TickerEquity,TickerBond,TickerRealEst)
YahooData <- new.env() # define new environment
getSymbols(Tickers, src = "yahoo",from = as.Date("1980-01-04"), 
          to = as.Date("2018-01-01"),env = YahooData,periodicity = "daily")
a <- eapply(YahooData,function(x){rownames_to_column(as.data.frame(na.omit(x)),"Date")})
Intersect <- a[[1]] # initialization
for(i in a[-1]){# extract common dates
  Intersect <- Intersect %>% inner_join(i,by="Date")
}
# for converting date-string to day of week (number) use: as.POSIXlt(Price[1,1])$wday
Price <- Intersect %>% select(Date,paste(str_replace_all(Tickers,"[^[:alnum:]]",""),".Close",sep="")) # to be changed
# convert daily to weekly, in order to apply the function "to.weekly" we need a xls object
Price <- xts(Price[,-1], order.by=as.Date(Price[,1])) %>% to.weekly(.,OHLC = FALSE) %>%
  as.data.frame()
Return <- Price[-1,] / Price[-dim(Price)[2],]-1
  




