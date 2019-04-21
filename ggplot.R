#ggplot

library(ggplot2)
p=c(21,19,39,50,60,55,78,69,90,110,111,120,130,141)
q=c(32,46,38,47,40,48,67,50,40,52,65,74,85,79)
f=data.frame(x,y)
ggplot(f,aes(y=q,x=p))+
geom_point(alpha=.5)+
stat_smooth(method="lm",formula = y~I(x^2))

#Overfit:
  
library(ggplot2)
q=c(1,4,9,16,25,36,49,64)
p=c(1,2,3,4,5,6,7,8)
f=data.frame(p,q)
ggplot(f,aes(y=q,x=p))+
geom_point(alpha=.5)+
stat_smooth(method="lm",formula = y~I(x^2))

