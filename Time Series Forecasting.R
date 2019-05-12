# Time Series Forecasting

install.packages("quantmod")
install.packages("forecast")
install.packages("fpp")
library(quantmod)  #Quantitative Financial Modelling and Trading Framework for R
library(forecast)
library(fpp)


from.dat=as.Date("04/01/14", format="%m/%d/%y")

to.dat=as.Date("10/30/17", format="%m/%d/%y")

#Load and Manage Data from Multiple Sources

# Loading Google stock data using its symbol at NASDAQ (GOOG)

getSymbols("GOOG",src = "yahoo",from=from.dat,to=to.dat) 

head(GOOG)


# Store as monthly time series



mGoog=to.monthly(GOOG)

head(mGoog)

googOpen = Op(mGoog)

googOpen

#===================================================

# read directly from CSV

mGoog=read.csv(file.choose())

head(mGoog)

# Take stock opening information

googOpen = mGoog[,2]

head(googOpen)

str(googOpen)

#save(googOpen,file="googOpen.RData")

#load("googOpen.RData")

#===================================================

# create time series object

ts1=ts(googOpen,frequency = 12) # frequency: No of observation per unit time

ts1

# plot time series data
par(mfrow=c(2,2))
plot(ts1,xlab = "Years+1",ylab="GOOG")

# Decompose time series

plot(decompose(ts1),xlab="Years+1")

ts1Train=window(ts1,start=1,end=3.5)

ts1Test=window(ts1,start=3.5,end=4.5)

ts1Train

ts1Test

par(mfrow=c(1,1))

#==========Moving Average====================

#order=2

goog_ma=ma(ts1Train,order=2)

# Forecast using moving average

goog_ma_forecast=forecast(goog_ma) #ETS(A,A,N)

accuracy(goog_ma_forecast,ts1Test)[,5]

plot(ts1,col="black")

lines(goog_ma_forecast$fitted,col="green")

lines(goog_ma_forecast$mean,col="green")

#order=4

goog_ma=ma(ts1Train,order=4)

# Forecast using moving average

goog_ma_forecast=forecast(goog_ma) #ETS(A,A,N)

accuracy(goog_ma_forecast,ts1Test)[,5]

lines(goog_ma_forecast$fitted,col="red")

lines(goog_ma_forecast$mean,col="red")

# order=8

goog_ma=ma(ts1Train,order=8)

# Forecast using moving average

goog_ma_forecast=forecast(goog_ma) #ETS(A,A,N)

accuracy(goog_ma_forecast,ts1Test)[,5]

lines(goog_ma_forecast$fitted,col="blue")

lines(goog_ma_forecast$mean,col="blue")
