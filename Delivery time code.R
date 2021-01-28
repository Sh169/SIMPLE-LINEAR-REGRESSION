setwd("F:/Assignments/Simple Regression/delivery_time.csv")
getwd()
#Choose file delivery_time
delivery<- read.csv(file.choose())
View(delivery)

#Exploratory Data Analysis
summary(delivery)

#scatter plot
plot(delivery$Delivery.Time,delivery$Sorting.Time)

# Other Exploratory data analysis and Plots

boxplot(delivery)

#Histogram
hist(delivery$Delivery.Time)
hist(delivery$Sorting.Time)


attach(delivery)

#Correlation Coefficent r
cor(Delivery.Time,Sorting.Time)
# If |r| is greater than  0.7 then Co-relation is Strong(Correlation Co-efficient = 0.8259973). 
# This has a strong Correlation 

#Simple Linear Regression Model
reg<-lm(Sorting.Time~Delivery.Time)
summary(reg)

## Probability value is 0.513 which is greater than 0.05
## The Probability Value for F-Statistic is 3.983e-06(Overall Probability Model is less than 0.05)
pred<-predict((reg))
 
 reg$residuals
 sum(reg$residuals)
 
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(delivery))  #RMSE
 
sqrt(mean(reg$residuals^2))

#For calculating the confidence interval
confint(reg,level=0.95)
## The above code will get you 2 equations 
# 1 to calculate the lower range and other for upper range

predict(reg,interval="predict")


# ggplot for adding regresion line for data
install.packages("ggplot2")
library(ggplot2)
ggplot(data = delivery, aes(x = Delivery.Time, y = Sorting.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery, aes(x=Delivery.Time, y=pred))

# we may have to do transformation of variables for better R-squared value
# Applying transformations

#########    Logrithamic Model

# x = log(Delivery.time); y = Sorting.time

plot(log(Delivery.Time), Sorting.Time)
cor(log(Delivery.Time), Sorting.Time)

reg_log <- lm(Sorting.Time ~ log(Delivery.Time))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(delivery))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")
##### probability=0.000222
#####R-squared:  0.7109,	Adjusted R-squared:  0.6957 
#####This model is 69.57% efficient


##############  Exponential model
reg_exp<-lm(log(Sorting.Time)~Delivery.Time) 
summary(reg_exp)
confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

#Probality=1.86e-14 whisch is still less than 0.05
#Multiple R-squared: 0.6954,	Adjusted R-squared:  0.6794 
#This model is 71.09% efficient

# Polynomial model with 2 degree (quadratic model)
plot(Delivery.Time, Sorting.Time)
plot(Delivery.Time*Delivery.Time, Sorting.Time)

cor(Delivery.Time*Delivery.Time, Sorting.Time)

plot(Delivery.Time*Delivery.Time, log(Sorting.Time))

cor(Delivery.Time, log(Sorting.Time))
cor(Delivery.Time*Delivery.Time, log(Sorting.Time))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Sorting.Time) ~ Delivery.Time + I(Delivery.Time*Delivery.Time))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = delivery$Sorting.Time - expy

sqrt(sum(err^2)/nrow(delivery))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = delivery, aes(x = Delivery.Time + I(Delivery.Time^2), y = log(Sorting.Time))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery, aes(x=Delivery.Time+I(Delivery.Time^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(AT)~Waist + I(Waist*Waist) + I(Waist*Waist*Waist))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = wc_at, aes(x = Waist + I(Waist^2) + I(Waist^3), y = AT)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = wc_at, aes(x=Waist+I(Waist^2)+I(Waist^3), y=expy3))
#Probability=0.292 which is greater than 0.05
#Multiple R-squared:  0.7976,	Adjusted R-squared:  0.7619 
#This model is 79.76


model_R_Squared_values <- list
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","reg2degree","reg3degree")
model_R_Squared_values[["R_squared"]] <- c(0.6655,0.6957,0.6794,0.7708,0.7619)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
View(Final)

# Polynomial with 2 degree model gives the best Adjusted R-Squared value
predicted_Value <- exp(predict(reg2degree))
predicted_Value

Final <- cbind(Sorting_Time=delivery$Sorting.Time ,Delivery_Time = delivery$Delivery.Time,Predicted_Delivery_time=predicted_Value)

View(Final)
plot(reg2degree)
hist(residuals(reg_exp)) # close to normal distribution
