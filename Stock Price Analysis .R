install.packages(c("quantmod", "TTR"))
library(quantmod)
library(TTR)
# Get Apple stock data from Yahoo Finance
getSymbols("AAPL", src = "yahoo", from = "2023-01-01", to = Sys.Date())
head(AAPL)

# Calculate 50-day and 200-day SMA
AAPL$SMA50 <- SMA(Cl(AAPL), n = 50)
AAPL$SMA200 <- SMA(Cl(AAPL), n = 200)
# Plot the closing price and SMAs
chartSeries(AAPL, name = "Apple Stock Price with Moving Averages", 
            TA = c(addSMA(n = 50, col = "blue"), addSMA(n = 200, col = "red")))



