install.packages("forecast")
install.packages("fpp")
install.packages("TRR")
library(forecast)
library(fpp)
library(TTR)
food<-read.csv("C:/NItesh/Data Analytics IIMA/Forecasting/Apollo Hospitals/Apollo Hospitals Food.txt", sep = '\t',header = T)
View(food)
head(food)
Omellette<-food$Omellette

#Converting data into ts objects
Omellettets <- ts(Omellette[4:115], start=c(1), end=c(16), frequency=7)

View(Omellettets)

#Plotting of data
plot(Omellettets)

library(openxlsx)

library(forecast)

#Moving Average
OmelletteSMA7 <- SMA(Omellette,n=7)
OmelletteSMA7ts<-ts(OmelletteSMA7, start=c(1), end=c(17), frequency=7)
View(OmelletteSMA7)
plot(OmelletteSMA7ts)
str(OmelletteSMA7ts)
aa<-forecast(OmelletteSMA7,7)
fcast_SMA_Omellette<-aa$mean
plot(aa)

#Seasonal plots of data 
seasonplot(Omellettets)
ggseasonplot(Omellettets, col=rainbow(7), year.labels=TRUE)
ggseasonplot(Omellettets)
ggseasonplot(Omellettets)


#Simple Exponential Smoothing
fitOmellettets <- ses(Omellettets, h=7)
plot(fitOmellettets)
fcast_hwfcast_ses_Omellette<-fitOmellettets$mean


#Double Exponential Smoothing
fitOmellettetsde <- holt(Omellettets, h=7)
plot(fitOmellettetsde)
fcast_hwfcast_hwdouble_Omellette<-fitOmellettetsde$mean


#Holt-Winters Triple Exponential Smoothing
fitOmellettetshw <- hw(Omellettets, h=7)
plot(fitOmellettetshw)
fcast_hwfcast_hwtriple_Omellette<-fitOmellettetshw$mean


#Decomposition method with linear trend
t<-c(1:112)
xq<-c("Th","F","Sa","S","M","T", "W")
dwk<-rep(xq,16)
Omellettet<-Omellette[4:115]
Omellette.df<-data.frame(Omellettet,t,dwk)
Omellettelint<-lm(Omellettet ~ t+factor(dwk), data=Omellette.df)
summary(Omellettelint)
newdt<- data.frame(t=c(113:119),dwk=xq)
View(newdt)
?predict.lm
predict<-predict.lm(Omellettelint,newdata= newdt)
plot(predict.lm(Omellettelint,newdata= newdt),type="l")
fcast_hwfcast_hwtriple_Omellette<-predict

#Decomposition method using STL
stlfit <- stl(Omellettets, t.window=15, s.window="periodic", robust=TRUE)
plot(stlfit)
eeadj <- seasadj(stlfit)
fcast <- forecast(stlfit, method="naive",h=7)
plot(fcast)
fcast_stlm_Omellette<-fcast$mean
