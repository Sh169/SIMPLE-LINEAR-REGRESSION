setwd("F:/Assignments/Simple Regression/Salary_data.csv")
getwd()
#Choose file delivery_time
emp<- read.csv(file.choose())
View(emp)

#Exploratory Data Analysis
summary(emp)

#scatter plot
#X--> YearsExperience,Y-->Salary
plot(emp$YearsExperience,emp$Salary)

# Other Exploratory data analysis and Plots

boxplot(emp)

#Histogram
hist(emp$YearsExperience)
hist(emp$Salary)


attach(emp)

#Correlation Coefficent r
cor(YearsExperience,Salary)
# If |r| is greater than  0.7 then Co-relation is Strong(Correlation Co-efficient = 0.9782416). 
# This has a strong Correlation 

#Simple Linear Regression Model
reg<-lm(Salary~YearsExperience) # (Y~X)
summary(reg)

## Probability value is 5.51e-12 which is less than 0.05
#Multiple R-squared:  0.957,	Adjusted R-squared:  0.9554 
## The Probability Value for F-Statistic is 2.2e-16 (Overall Probability Model is less than 0.05)
pred<-predict((reg))

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(emp))  #RMSE

sqrt(mean(reg$residuals^2))

#For calculating the confidence interval
confint(reg,level=0.95)
## The above code will get you 2 equations 
# 1 to calculate the lower range and other for upper range

predict(reg,interval="predict")


# ggplot for adding regresion line for data
install.packages("ggplot2")
library(ggplot2)
ggplot(data = emp, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=YearsExperience, y=pred))

# we may have to do transformation of variables for better R-squared value
# Applying transformations

#########    Logrithamic Model

# x = log(YearsExperience); y =Salary

plot(log(YearsExperience), Salary)
cor(log(YearsExperience), Salary)

reg_log <- lm(Salary ~ log(YearsExperience))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(emp))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")
##### probability=0.00727
#####Multiple R-squared:  0.8539,	Adjusted R-squared:  0.8487
#####This model is 85.87% 


##############  Exponential model
reg_exp<-lm(log(Salary)~YearsExperience) 
summary(reg_exp)
confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

#Probality=2e-16 whisch is still less than 0.05
#Multiple R-squared:  0.932,	Adjusted R-squared:  0.9295 
#This model is 93%

# Polynomial model with 2 degree (quadratic model)
plot(YearsExperience, Salary)
plot(YearsExperience*YearsExperience, Salary)

cor(YearsExperience*YearsExperience, Salary)

plot(YearsExperience*YearsExperience, log(Salary))

cor(YearsExperience, log(Salary))
cor(YearsExperience*YearsExperience, log(Salary))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Salary) ~ YearsExperience + I(YearsExperience*YearsExperience))

summary(reg2degree)
#Probality=2e-16 whisch is still less than 0.05
#Multiple R-squared:0.9486,	Adjusted R-squared:  0.9448   
#This model is 94%

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = emp$Salary- expy

sqrt(sum(err^2)/nrow(emp))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = emp, aes(x = YearsExperience + I(YearsExperience^2), y = log(Salary))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=YearsExperience+I(YearsExperience^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(Salary)~YearsExperience+ I(YearsExperience*YearsExperience) + I(YearsExperience*YearsExperience*YearsExperience))
summary(reg3degree)
#Probality=2e-16 whisch is still less than 0.05
#Multiple R-squared:0.9515,	Adjusted R-squared:  0.9459  
#This model is 95%

logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = emp, aes(x = YearsExperience + I(YearsExperience^2) + I(YearsExperience^3), y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=YearsExperience+I(YearsExperience^2)+I(YearsExperience^3), y=expy3))


model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","reg2degree","reg3degree")
model_R_Squared_values[["R_squared"]] <- c(0.9554,0.8487,0.9295 ,0.9448 ,0.9459)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
View(Final)

#Simple Linear Regression model gives the best Adjusted R-Squared value
predicted_Value <- (predict(reg))
predicted_Value



Final <- cbind(Salary=emp$Salary ,YearsExperience = emp$YearsExperience,Predicted_Salary=predicted_Value)

View(Final)
plot(reg)
hist(residuals(reg)) # close to normal distribution





