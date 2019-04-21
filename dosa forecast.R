install.packages("forecast")
install.packages("fpp")
install.packages("TRR")
library(forecast)
library(fpp)
library(TTR)
food<-read.csv("C:/NItesh/Data Analytics IIMA/Forecasting/Apollo Hospitals/Apollo Hospitals Food.txt", sep = '\t',header = T)
View(food)
head(food)
Dosa<-food$Dosa

#Converting data into ts objects
Dosats <- ts(Dosa[4:115], start=c(1), end=c(16), frequency=7)

View(Dosats)

#Plotting of data
plot(Dosats)

library(openxlsx)

library(forecast)

#Moving Average
DosaSMA7 <- SMA(Dosa,n=7)
DosaSMA7ts<-ts(DosaSMA7, start=c(1), end=c(17), frequency=7)
View(DosaSMA7)
plot(DosaSMA7ts)
str(DosaSMA7ts)
aa<-forecast(DosaSMA7,7)
fcast_SMA_Dosa<-aa$mean
plot(aa)

#Seasonal plots of data 
seasonplot(Dosats)
ggseasonplot(Dosats, col=rainbow(7), year.labels=TRUE)
ggseasonplot(Dosats)
ggseasonplot(Dosats)


#Simple Exponential Smoothing
fitDosats <- ses(Dosats, h=7)
plot(fitDosats)
fcast_hwfcast_ses_Dosa<-fitDosats$mean


#Double Exponential Smoothing
fitDosatsde <- holt(Dosats, h=7)
?holt
plot(fitDosatsde)
fcast_hwfcast_hwdouble_Dosa<-fitDosatsde$mean


#Holt-Winters Triple Exponential Smoothing
fitDosatshw <- hw(Dosats, h=7)
plot(fitDosatshw)
fcast_hwfcast_hwtriple_Dosa<-fitDosatshw$mean


#Decomposition method with linear trend
t<-c(1:112)
xq<-c("Th","F","Sa","S","M","T", "W")
dwk<-rep(xq,16)
Dosat<-Dosa[4:115]
Dosa.df<-data.frame(Dosat,t,dwk)
Dosalint<-lm(Dosat ~ t+factor(dwk), data=Dosa.df)
summary(Dosalint)
newdt<- data.frame(t=c(113:119),dwk=xq)
View(newdt)
?predict.lm
predict<-predict.lm(Dosalint,newdata= newdt)
plot(predict.lm(Dosalint,newdata= newdt),type="l")
fcast_hwfcast_hwtriple_Dosa<-predict

#Decomposition method using STL
stlfit <- stl(Dosats, t.window=15, s.window="periodic", robust=TRUE)
plot(stlfit)
eeadj <- seasadj(stlfit)
fcast <- forecast(stlfit, method="naive",h=7)
plot(fcast)
fcast_stlm_Dosa<-fcast$mean
