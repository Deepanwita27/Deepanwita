#Error analysis 
attach(Alumni)
#Full model fitted. Note that a better model omits "percentage of small classes" (S)
alumni.lm.123=lm(Alumni.Giving.Rate~ Graduation.Rate+Student.Faculty.Ratio +Percentage.Below.20)
summary(alumni.lm.123)

#Create the standarised residuals 
alumni.lm.123.stdres=rstandard(alumni.lm.123)
alumni.lm.123.fit=fitted(alumni.lm.123)

#Plot of the standardised residuals against the fitted values
plot(alumni.lm.123.fit,alumni.lm.123.stdres,xlab="Fitted Giving Percentages",ylab="Standardized Residuals", pch=19, main="Plot of standardized residuals against fitted values", col="red")
abline(c(0,0))

#Plot of the standardised residuals against the variables
plot(Graduation.Rate,alumni.lm.123.stdres,xlab="Graduation Rate",ylab="Standardized Residuals", pch=19, main="Plot of standardized residuals against graduation rate", col="red")
abline(c(0,0))
plot(Student.Faculty.Ratio,alumni.lm.123.stdres,xlab="Student/Faculty Ratio", ylab="Standardized Residuals", pch=19, main="Plot of standardized residuals against student/faculty ratio", col="red")
abline(c(0,0))
plot(Percentage.Below.20,alumni.lm.123.stdres,xlab="Percentage of classes of size less than 20", ylab="Standardized Residuals", pch=19, main="Plot of standardized residuals against percentage of small classes", col="red")
abline(c(0,0))

#Plot of the histogram of the standardised residuals to check for the validity of the normality assumption
hist(alumni.lm.123.stdres, probability=T, xlab = "Standardised Residuals", main = "Histogram of Standardised Residuals")