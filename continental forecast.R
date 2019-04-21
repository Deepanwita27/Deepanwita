install.packages("forecast")
install.packages("fpp")
install.packages("TRR")
library(forecast)
library(fpp)
library(TTR)
food<-read.csv("C:/NItesh/Data Analytics IIMA/Forecasting/Apollo Hospitals/Apollo Hospitals Food.txt", sep = '\t',header = T)
View(food)
head(food)
Continental_BF<-food$Continental_BF

#Converting data into ts objects
Continental_BFts <- ts(Continental_BF[4:115], start=c(1), end=c(16), frequency=7)

View(Continental_BFts)

#Plotting of data
plot(Continental_BFts)

library(openxlsx)

library(forecast)

#Moving Average
Continental_BFSMA7 <- SMA(Continental_BF,n=7)
Continental_BFSMA7ts<-ts(Continental_BFSMA7, start=c(1), end=c(17), frequency=7)
View(Continental_BFSMA7)
plot(Continental_BFSMA7ts)
str(Continental_BFSMA7ts)
aa<-forecast(Continental_BFSMA7,7)
fcast_SMA_Continental_BF<-aa$mean
plot(aa)

#Seasonal plots of data 
seasonplot(Continental_BFts)
ggseasonplot(Continental_BFts, col=rainbow(7), year.labels=TRUE)
ggseasonplot(Continental_BFts)
ggseasonplot(Continental_BFts)


#Simple Exponential Smoothing
fitContinental_BFts <- ses(Continental_BFts, h=7)
plot(fitContinental_BFts)
fcast_hwfcast_ses_Continental_BF<-fitContinental_BFts$mean


#Double Exponential Smoothing
fitContinental_BFtsde <- holt(Continental_BFts, h=7)
plot(fitContinental_BFtsde)
fcast_hwfcast_hwdouble_Continental_BF<-fitContinental_BFtsde$mean


#Holt-Winters Triple Exponential Smoothing
fitContinental_BFtshw <- hw(Continental_BFts, h=7)
plot(fitContinental_BFtshw)
fcast_hwfcast_hwtriple_Continental_BF<-fitContinental_BFtshw$mean


#Decomposition method with linear trend
t<-c(1:112)
xq<-c("Th","F","Sa","S","M","T", "W")
dwk<-rep(xq,16)
Continental_BFt<-Continental_BF[4:115]
Continental_BF.df<-data.frame(Continental_BFt,t,dwk)
Continental_BFlint<-lm(Continental_BFt ~ t+factor(dwk), data=Continental_BF.df)
summary(Continental_BFlint)
newdt<- data.frame(t=c(113:119),dwk=xq)
View(newdt)
?predict.lm
predict<-predict.lm(Continental_BFlint,newdata= newdt)
plot(predict.lm(Continental_BFlint,newdata= newdt),type="l")
fcast_hwfcast_hwtriple_Continental_BF<-predict

#Decomposition method using STL
stlfit <- stl(Continental_BFts, t.window=15, s.window="periodic", robust=TRUE)
plot(stlfit)
eeadj <- seasadj(stlfit)
fcast <- forecast(stlfit, method="naive",h=7)
plot(fcast)
fcast_stlm_Continental_BF<-fcast$mean
