
# Data Import
library(readxl)
dataset1 = read_excel("/home/yashkhem/Downloads/Problem set 1.xlsx", sheet = "Puriry hydrocarbon")


# Defining Variables
y = dataset1$`Purity (%)`
x = dataset1$`Hydrocarbon (%)`


# Scatter plot
plot(x,y, xlab = "Hydrocarbon (%)", ylab = "Purity of oxygen", main= "Scatter plot, fitted regression line, confidence and prediction bands       Roll No. ----")



# Simple linear regression fitting
reg = lm(y~x)
summary(reg)


#Plotting regression line
abline(reg)


#fitted values
fitted(reg)

#residuals
residuals(reg)



# ANOVA table
anova(reg)

#confidence interval for parameters
confint(reg, level=0.95)

# Confidence interval and prediction interval
new.dat = data.frame(x=1)
predict(reg, newdata = new.dat, interval = 'confidence')
predict(reg, newdata = new.dat, interval = 'prediction')

# Confidence band and prediction band
newx = seq(0.8,1.6,0.05)
pred_interval = predict(reg, newdata = data.frame(x=newx), interval = 'prediction')
conf_interval = predict(reg, newdata = data.frame(x=newx), interval = 'confidence')
lines(newx, conf_interval[,2], col="blue", lty=1)
lines(newx, conf_interval[,3], col="blue", lty=1)
lines(newx, pred_interval[,2], col="orange", lty=1)
lines(newx, pred_interval[,3], col="orange", lty=1)

