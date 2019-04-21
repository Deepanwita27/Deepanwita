install.packages("forecast")
install.packages("fpp")
install.packages("TRR")
library(forecast)
library(fpp)
library(TTR)
food<-read.csv("C:/NItesh/Data Analytics IIMA/Forecasting/Apollo Hospitals/Apollo Hospitals Food.txt", sep = '\t',header = T)
View(food)
head(food)
idly<-food$Idly

#Converting data into ts objects
idlyts <- ts(idly[4:115], start=c(1), end=c(16), frequency=7)

View(idlyts)

#Plotting of data
plot(idlyts)

library(openxlsx)

library(forecast)

#Moving Average
idlySMA7 <- SMA(idly,n=7)
idlySMA7ts<-ts(idlySMA7, start=c(1), end=c(17), frequency=7)
View(idlySMA7)
plot(idlySMA7ts)
str(idlySMA7ts)
aa<-forecast(idlySMA7,5)
plot(aa)

#Seasonal plots of data 
seasonplot(idlyts)
ggseasonplot(idlyts, col=rainbow(7), year.labels=TRUE)
ggseasonplot(idlyts)
ggseasonplot(dosats)


#Simple Exponential Smoothing
fitidlyts <- ses(idlyts, h=7)
plot(fitidlyts)
fcast_hwfcast_ses_idly<-fitidlyts$mean


#Double Exponential Smoothing
fitidlytsde <- holt(idlyts, h=7)
?holt
plot(fitidlytsde)
fcast_hwfcast_hwdouble_idly<-fitidlytsde$mean


#Holt-Winters Triple Exponential Smoothing
fitidlytshw <- hw(idlyts, h=7)
plot(fitidlytshw)
fcast_hwfcast_hwtriple_idly<-fitidlytshw$mean


#Decomposition method with linear trend
t<-c(1:112)
xq<-c("Th","F","Sa","S","M","T", "W")
dwk<-rep(xq,16)
idlyt<-idly[4:115]
idly.df<-data.frame(idlyt,t,dwk)
idlylint<-lm(idlyt ~ t+factor(dwk), data=idly.df)
summary(idlylint)
newdt<- data.frame(t=c(113:119),dwk=xq)
View(newdt)
?predict.lm
predict.lm(idlylint,newdata= newdt)
plot(predict.lm(idlylint,newdata= newdt),type="l")


#Decomposition method using STL
stlfit <- stl(idlyts, t.window=15, s.window="periodic", robust=TRUE)
plot(stlfit)
eeadj <- seasadj(stlfit)
fcast <- forecast(stlfit, method="naive",h=7)
plot(fcast)
fcast_stlm_idly<-fcast$mean
