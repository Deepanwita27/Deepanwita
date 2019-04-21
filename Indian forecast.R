install.packages("forecast")
install.packages("fpp")
install.packages("TRR")
library(forecast)
library(fpp)
library(TTR)
food<-read.csv("C:/NItesh/Data Analytics IIMA/Forecasting/Apollo Hospitals/Apollo Hospitals Food.txt", sep = '\t',header = T)
summary(food)
View(food)
head(food)
North_Indian_BF<-food$North_Indian_BF

#Converting data into ts objects
North_Indian_BFts <- ts(North_Indian_BF[4:115], start=c(1), end=c(16), frequency=7)

View(North_Indian_BFts)

#Plotting of data
plot(North_Indian_BFts)

library(openxlsx)

library(forecast)

#Moving Average
North_Indian_BFSMA7 <- SMA(North_Indian_BF,n=7)
North_Indian_BFSMA7ts<-ts(North_Indian_BFSMA7, start=c(1), end=c(17), frequency=7)
View(North_Indian_BFSMA7)
plot(North_Indian_BFSMA7ts)
str(North_Indian_BFSMA7ts)
aa<-forecast(North_Indian_BFSMA7,7)
fcast_SMA_North_Indian_BF<-aa$mean
plot(aa)

#Seasonal plots of data 
seasonplot(North_Indian_BFts)
ggseasonplot(North_Indian_BFts, col=rainbow(7), year.labels=TRUE)
ggseasonplot(North_Indian_BFts)
ggseasonplot(North_Indian_BFts)


#Simple Exponential Smoothing
fitNorth_Indian_BFts <- ses(North_Indian_BFts, h=7)
plot(fitNorth_Indian_BFts)
fcast_hwfcast_ses_North_Indian_BF<-fitNorth_Indian_BFts$mean


#Double Exponential Smoothing
fitNorth_Indian_BFtsde <- holt(North_Indian_BFts, h=7)
plot(fitNorth_Indian_BFtsde)
fcast_hwfcast_hwdouble_North_Indian_BF<-fitNorth_Indian_BFtsde$mean


#Holt-Winters Triple Exponential Smoothing
fitNorth_Indian_BFtshw <- hw(North_Indian_BFts, h=7)
plot(fitNorth_Indian_BFtshw)
fcast_hwfcast_hwtriple_North_Indian_BF<-fitNorth_Indian_BFtshw$mean


#Decomposition method with linear trend
t<-c(1:112)
xq<-c("Th","F","Sa","S","M","T", "W")
dwk<-rep(xq,16)
North_Indian_BFt<-North_Indian_BF[4:115]
North_Indian_BF.df<-data.frame(North_Indian_BFt,t,dwk)
North_Indian_BFlint<-lm(North_Indian_BFt ~ t+factor(dwk), data=North_Indian_BF.df)
summary(North_Indian_BFlint)
newdt<- data.frame(t=c(113:119),dwk=xq)
View(newdt)
?predict.lm
predict<-predict.lm(North_Indian_BFlint,newdata= newdt)
plot(predict.lm(North_Indian_BFlint,newdata= newdt),type="l")
fcast_hwfcast_hwtriple_North_Indian_BF<-predict

#Decomposition method using STL
stlfit <- stl(North_Indian_BFts, t.window=15, s.window="periodic", robust=TRUE)
plot(stlfit)
eeadj <- seasadj(stlfit)
fcast <- forecast(stlfit, method="naive",h=7)
plot(fcast)
fcast_stlm_North_Indian_BF<-fcast$mean
