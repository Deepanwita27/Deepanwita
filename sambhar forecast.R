install.packages("forecast")
install.packages("fpp")
install.packages("TRR")
library(forecast)
library(fpp)
library(TTR)
food<-read.csv("C:/NItesh/Data Analytics IIMA/Forecasting/Apollo Hospitals/Apollo Hospitals Food.txt", sep = '\t',header = T)
View(food)
head(food)
Sambar<-food$Sambar

#Converting data into ts objects
Sambarts <- ts(Sambar[4:115], start=c(1), end=c(16), frequency=7)

View(Sambarts)

#Plotting of data
plot(Sambarts)

library(openxlsx)

library(forecast)

#Moving Average
SambarSMA7 <- SMA(Sambar,n=7)
SambarSMA7ts<-ts(SambarSMA7, start=c(1), end=c(17), frequency=7)
View(SambarSMA7)
plot(SambarSMA7ts)
str(SambarSMA7ts)
aa<-forecast(SambarSMA7,7)
fcast_SMA_Sambar<-aa$mean
plot(aa)

#Seasonal plots of data 
seasonplot(Sambarts)
ggseasonplot(Sambarts, col=rainbow(7), year.labels=TRUE)
ggseasonplot(Sambarts)
ggseasonplot(Sambarts)


#Simple Exponential Smoothing
fitSambarts <- ses(Sambarts, h=7)
plot(fitSambarts)
fcast_hwfcast_ses_Sambar<-fitSambarts$mean


#Double Exponential Smoothing
fitSambartsde <- holt(Sambarts, h=7)
plot(fitSambartsde)
fcast_hwfcast_hwdouble_Sambar<-fitSambartsde$mean


#Holt-Winters Triple Exponential Smoothing
fitSambartshw <- hw(Sambarts, h=7)
plot(fitSambartshw)
fcast_hwfcast_hwtriple_Sambar<-fitSambartshw$mean


#Decomposition method with linear trend
t<-c(1:112)
xq<-c("Th","F","Sa","S","M","T", "W")
dwk<-rep(xq,16)
Sambart<-Sambar[4:115]
Sambar.df<-data.frame(Sambart,t,dwk)
Sambarlint<-lm(Sambart ~ t+factor(dwk), data=Sambar.df)
summary(Sambarlint)
newdt<- data.frame(t=c(113:119),dwk=xq)
View(newdt)
?predict.lm
predict<-predict.lm(Sambarlint,newdata= newdt)
plot(predict.lm(Sambarlint,newdata= newdt),type="l")
fcast_hwfcast_hwtriple_Sambar<-predict

#Decomposition method using STL
stlfit <- stl(Sambarts, t.window=15, s.window="periodic", robust=TRUE)
plot(stlfit)
eeadj <- seasadj(stlfit)
fcast <- forecast(stlfit, method="naive",h=7)
plot(fcast)
fcast_stlm_Sambar<-fcast$mean
