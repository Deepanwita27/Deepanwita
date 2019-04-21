install.packages("forecast")
install.packages("fpp")
install.packages("TRR")
library(forecast)
library(fpp)
library(TTR)
food<-read.csv("C:/NItesh/Data Analytics IIMA/Forecasting/Apollo Hospitals/Apollo Hospitals Food.txt", sep = '\t',header = T)
View(food)
head(food)
Chutney<-food$Chutney

#Converting data into ts objects
Chutneyts <- ts(Chutney[4:115], start=c(1), end=c(16), frequency=7)

View(Chutneyts)

#Plotting of data
plot(Chutneyts)

library(openxlsx)

library(forecast)

#Moving Average
ChutneySMA7 <- SMA(Chutney,n=7)
ChutneySMA7ts<-ts(ChutneySMA7, start=c(1), end=c(17), frequency=7)
View(ChutneySMA7)
plot(ChutneySMA7ts)
str(ChutneySMA7ts)
aa<-forecast(ChutneySMA7,7)
fcast_SMA_Chutney<-aa$mean
plot(aa)

#Seasonal plots of data 
seasonplot(Chutneyts)
ggseasonplot(Chutneyts, col=rainbow(7), year.labels=TRUE)
ggseasonplot(Chutneyts)
ggseasonplot(Chutneyts)


#Simple Exponential Smoothing
fitChutneyts <- ses(Chutneyts, h=7)
plot(fitChutneyts)
fcast_hwfcast_ses_Chutney<-fitChutneyts$mean


#Double Exponential Smoothing
fitChutneytsde <- holt(Chutneyts, h=7)
plot(fitChutneytsde)
fcast_hwfcast_hwdouble_Chutney<-fitChutneytsde$mean


#Holt-Winters Triple Exponential Smoothing
fitChutneytshw <- hw(Chutneyts, h=7)
plot(fitChutneytshw)
fcast_hwfcast_hwtriple_Chutney<-fitChutneytshw$mean


#Decomposition method with linear trend
t<-c(1:112)
xq<-c("Th","F","Sa","S","M","T", "W")
dwk<-rep(xq,16)
Chutneyt<-Chutney[4:115]
Chutney.df<-data.frame(Chutneyt,t,dwk)
Chutneylint<-lm(Chutneyt ~ t+factor(dwk), data=Chutney.df)
summary(Chutneylint)
newdt<- data.frame(t=c(113:119),dwk=xq)
View(newdt)
?predict.lm
predict<-predict.lm(Chutneylint,newdata= newdt)
plot(predict.lm(Chutneylint,newdata= newdt),type="l")
fcast_hwfcast_hwtriple_Chutney<-predict

#Decomposition method using STL
stlfit <- stl(Chutneyts, t.window=15, s.window="periodic", robust=TRUE)
plot(stlfit)
eeadj <- seasadj(stlfit)
fcast <- forecast(stlfit, method="naive",h=7)
plot(fcast)
fcast_stlm_Chutney<-fcast$mean
