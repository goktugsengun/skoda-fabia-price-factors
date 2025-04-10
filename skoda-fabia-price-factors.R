getwd()

fabia <- read.csv("/Users/goktug/Desktop/paper/fabia.csv")
head(fabia)

summary(fabia$date)

library(tidyverse)

fabia$age <- 2023 - fabia$date
fabia$engine_cc <- ifelse(fabia$kW <= 81, 1.0, ifelse(fabia$kW == 92, 1.4, 1.5))


fabia[1,]
head(fabia)

summary(fabia$km)

unique(fabia$date)

unique(fabia$engine_cc)

unique(fabia$kW)


date <- fabia$date
km <- fabia$km
kW <- fabia$kW
Price <- fabia$Price
age <- fabia$age
engine_cc <- fabia$engine_cc



# Histogram of all variables.

?hist
main, xlab, ylab

par(mfrow = c(1,1))

hist(age, freq = TRUE,
     main= "Histogram of Car's Age", xlab= "Age", ylab= "Number of Cars")
lines(density(age, kernel="gaussian"), lwd= 2, col = 2)

hist(km, freq = TRUE,
     main= "Histogram of Mileage (km)", xlab= "Mileage (km)", ylab= "Number of Cars", scale_x_discrete(c('2','3','4','5','6','8')))

lines(density(km, kernel="gaussian"), lwd= 2, col = 2)


hist(kW,freq = TRUE,
     main= "Histogram of Engine's Power (kW)", xlab= "Power (kW)", ylab= "Number of Cars")
lines(density(kW, kernel="gaussian"), lwd= 2, col = 2)

hist(engine_cc, freq = TRUE,
     main= "Histogram of Engine's Cubic Capacity (cc)", xlab= "Engine's Cubic Capacity (cc)", ylab= "Number of Cars")

lines(density(engine_cc, kernel="gaussian"), lwd= 2, col = 2)

hist(Price, freq = FALSE,
     main= "Histogram of Price (€)", xlab= "Price (€)", ylab= "Density")
lines(density(Price, kernel="gaussian"), lwd= 2, col = 2)
scale_x_discrete()

length(which(engine_cc == 1.5))

unique(kW)



### age - price

#1-year old car prices
hist(fabia[which(fabia[,5]==1), 4])
#2-year old car prices
hist(fabia[which(fabia[,5]==2), 4])

#4-year old car prices
hist(fabia[which(fabia[,5]==4), 4])
#5-year old car prices
hist(fabia[which(fabia[,5]==5), 4])
#6-year old car prices
hist(fabia[which(fabia[,5]==6), 4])



### mean comparison between different ages
m_age_one <- mean(fabia[which(fabia[,5]==1), 4])
m_age_two <- mean(fabia[which(fabia[,5]==2), 4])
m_age_four <- mean(fabia[which(fabia[,5]==4), 4])
m_age_five <- mean(fabia[which(fabia[,5]==5), 4])
m_age_six <- mean(fabia[which(fabia[,5]==6), 4])

plot(c(m_age_one, m_age_two, m_age_four, m_age_five, m_age_six), type = "b", lwd = 2, col = 2, 
     main = " Mean of Price Between Ages", xlab = "Age", ylab = "Price (€)")


### variance comparison between different ages.
v_age_one <- var(fabia[which(fabia[,5]==1), 4])
v_age_two <- var(fabia[which(fabia[,5]==2), 4])
v_age_four <- var(fabia[which(fabia[,5]==4), 4])
v_age_five <- var(fabia[which(fabia[,5]==5), 4])
v_age_six <- var(fabia[which(fabia[,5]==6), 4])

plot(c(v_age_one, v_age_two, v_age_four, v_age_five, v_age_six), type = "b", lwd = 2, col = 2, 
     main = "Variance of Price Between Ages", xlab = "Age", ylab = "Price (€)")




### The effects of km 

used1 <- fabia[which(fabia[,2]>0 & fabia[,2]<=20000 ), 4]
used2 <- fabia[which(fabia[,2]>20000 & fabia[,2]<=40000 ), 4]
used3 <- fabia[which(fabia[,2]>40000 & fabia[,2]<=60000 ), 4]
used4 <- fabia[which(fabia[,2]>60000 & fabia[,2]<=80000 ), 4]
used5 <- fabia[which(fabia[,2]>80000 & fabia[,2]<=100000 ), 4]
used6 <- fabia[which(fabia[,2]>100000), 4]


##mean visualization

m_used1 <- mean(used1)
m_used2 <- mean(used2)
m_used3 <- mean(used3)
m_used4 <- mean(used4)
m_used5 <- mean(used5)
m_used6 <- mean(used6)



plot(c(m_used1, m_used2, m_used3, m_used4, m_used5, m_used6), type = "b", lwd = 2, col = 2, 
     main = " Mean of Price Between Mileage Intervals", xlab = "Mileage of Car", ylab = "Price (€)")


## variance visualization

v_used1 <- var(used1)
v_used2 <- var(used2)
v_used3 <- var(used3)
v_used4 <- var(used4)
v_used5 <- var(used5)
v_used6 <- var(used6)

plot(c(v_used1, v_used2, v_used3, v_used4, v_used5, v_used6), type = "b", lwd = 2, col = 2, 
     main = " Variance of Price Between Mileage Intervals", xlab = "Mileage of Car", ylab = "Price (€)")

par(mfrow = c(1,2))
plot(c(m_used1, m_used2, m_used3, m_used4, m_used5, m_used6), type = "b", lwd = 2, col = 2, 
     main = " Mean of Price Between Mileage Intervals", xlab = "Mileage", ylab = "Price (€)")

plot(c(v_used1, v_used2, v_used3, v_used4, v_used5, v_used6), type = "b", lwd = 2, col = 2, 
     main = " Variance of Price Between Mileage Intervals", xlab = "Mileage", ylab = "Price (€)")


################################# ESTIMATE       Multiple Regression Model

# When I create to multiple regression model, I included all variables in the data. Which are mileage (km), kW (Power), age and
# the engine's cubic capacity (engine_cc). 
cor(x=age, y=Price)
cor(x=km,y=Price)
cor(x=engine_cc, y=Price)
cor(x=kW, y=Price)

model <- lm(Price ~ km + kW + age + engine_cc , data = fabia)

##################################.   Provide the analysis on parameter significance and the goodness-of-fit

summary(model)
# In order to understand the quality of a regression model, R-squared measure is used. R-squared is a value between 0 and 1. If the value is 
# close to "1", it shows a strong relationship and a better fit. We can reach the value of R-squared simply by using summary statistics.
# The summary statistics shows the R-squared along with other statistics. From the Adjusted R-squared section, we can reach the R-squared value.
# As it is seen, the R-squared for our model is "0.8319" when we include all of the variables in the data along with their squared values. 
# By looking at the summary statistics of our first model we can say that our model is 83.19% good, 
# in other words, there is a strong relationship between our dependent variable and the independent variables.

par(mfrow = c(2,2))
plot(model)



################################# Stepwise Selection

# I have included all the variables of the data in the first model. However, we can use other model and even get a better model 
# by excluding some of them. One of the ways to determine which variables to include to our model is Adjusted R-squared. Although it is useful,
# it has a disadvantage. The disadvantage is that the R-squared score increases as the number of variables increase, even though there is not much 
# relationship between the dependent variable and the independent variable.
# A better approach for deciding on a better model is "Akaike’s Information Criterion (AIC)". It is a better way of measure because it takes 
# the number of variables into consideration and avoids the effects of unrelated variables. In order to understand the better variable to include 
# in our model we look at their "AIC" value.

install.packages("MASS")

library(MASS)
stepAIC(model)

# When we look at the outcome, we see our AIC score for the first model is "2907.42" when all variables are included.
# Then we look at each variable and we notice that the engine's cubic capacity "engine_cc" has the lowest AIC score. Since we know that we need to
# exclude the variable with lower AIC score to get a better model, we create a new model without "engine_cc". 


model1 <- lm(Price ~ km + kW + age +engine_cc, data = fabia)
stepAIC(model1)
AIC(model1)

model2 <- lm(Price ~ km + kW + age, data = fabia)
stepAIC(model2)
AIC(model2)
summary(model2)


model3 <- lm(Price ~ km + age, data = fabia)
stepAIC(model3)
AIC(model3)
summary(model3)

model4 <- lm(Price ~ km + engine_cc, data = fabia)
AIC(model4)


model5 <- lm(Price ~ age + engine_cc, data = fabia)
AIC(model5)

model6 <- lm(Price ~ age + km + engine_cc)
AIC(model6)

model7 <- lm(Price ~ kW + engine_cc)
AIC(model7)





fitstart <- lm(Price ~ 1, data = fabia)

step(fitstart, direction = 'forward', scope = formula(model1))



step(model1, direction = 'backward')



cor(Price, age)


model <- lm(Price ~ age + km, data = fabia)

?stepAIC

##################  Plot estimated regression to check the assumptions (normality, equal variance)

par(mfrow = c(1,1))
plot(model)

summary(model)
round(coef(model),4)

##################   Describe how to use the estimated model for prediction

summary(model3)

round(coef(model3), 2)

fabia$predicted_price2 <- predict.lm(model)

predicted_price2 - predicted_price

?predict

predicted_price <- fabia$predicted_price



?predict


#######################.  Calculate 95% confidence interval for residuals of the resulting model

# The residual for each observation is the difference between predicted values of y (dependent variable) 
# and observed values of y . Which means; "Residual= actual y value − predicted y value". In order to 
# reach the residuals for our estimated model, I created "predicted_price" column where I calculated the predicted price for the model
# and finally created "residuals" field by subtracting fitted values from the actual "Price" field.

fabia$residuals <- Price - predicted_price
residuals <- fabia$residuals

par(mfrow = c(1,1))

hist(residuals, probability = TRUE)
lines(density(residuals))

model
fabia

datad <- c()



ppp <- predict(model, newdata = fabia)

summary(ppp)

ppp$residuals




newf <- fabia[, c('age', 'km')]
newf



newp <- predict(model, newdata = newf)

newp - predicted_price


model5 <- lm(Price ~ age + km)

a <- predict(model5, level = .95, interval = 'confidence')

plot(a)

c <- data.frame(yeni = modelr::rmse(model, newf) , eski = abs(residuals))
c

modelr::qae(model, newf, probs = c(.025, .975))


mod_summary <- summary(model5)
mod_summary$residuals

residuals - mod_summary$residuals

14890 - 14399.12

#
#
#
#
#

(50000 * -0.04) + (70 * 213.89) + (3 * -1268.99) + (1 * 3592.59) + 11072.06

(93108 * -0.04) + (81 * 213.89) + (5 * -1268.99) + (1 * -3592.59) + 11072.06

fabia[51,]
fabia[1,c('km', 'kW', 'Price', 'age', 'engine_cc', 'predicted_price')]




hist(residuals)

plot(residuals)


#confint(model, 'Price', level=0.95)



n <- length(residuals)
alpha <-  1 - .95
dof <- n-1
xbar <- mean(residuals)
Sx <- sqrt(mean((residuals - xbar) ^ 2))


lower_bound <- xbar - qt(1 - alpha / 2 , dof) * Sx / sqrt(n)
upper_bound <- xbar + qt(1 - alpha / 2 , dof) * Sx / sqrt(n)

lower_bound
upper_bound

n <- length(predicted_price)
alpha <-  1 - .95
dof <- n-1
xbar <- mean(predicted_price)
Sx <- sqrt(mean((predicted_price - xbar) ^ 2))




plot(residuals)
abline(h=0, col = 2, lwd = 2)
abline(h=lower_bound)
abline(h=upper_bound)


nintyfive <- fabia[which(residuals < upper_bound) & 
  which(residuals > lower_bound ), ]

fabia %>% filter(residuals < upper_bound &
                   residuals > lower_bound)



cor(Price,engine_cc)
cor(Price, kW)



################################################################################
lower_bound <- xbar - 2 * Sx
upper_bound <- xbar + 2 * Sx

hist(residuals)
abline(h = lower_bound)
abline(h = upper_bound)



plot(predicted_price)

abline(h = upperlimit)
abline(h = lowerlimit)
sqrt(var(predicted_price))

?ifelse
?which




#######
xbar <- mean(predicted_price)

#######






par(mfrow = c(1,1))
hist(residuals, probability = TRUE, breaks = 20)
lines(density(residuals), col = 2, lwd =3)


?hist


par(mfrow = c(1,1))
hist(residuals, probability = FALSE)
lines(density(residuals), col = 2, lwd =3)

abline(v = lower_bound, col = 2, lwd = 2)
abline(v = upper_bound, col = 2, lwd = 2)


upper_bound
lower_bound


plot(x=km + age, y=Price)
abline(Price ~ km + age)
abline()
lines(lower_bound)
lines(upper_bound)



plot(predicted_price, Price)
lines(residuals)
abline(model3)


model3

plot(model3)

coef(model3)

?abline

upperlimit <- mean(predicted_price) + upper_bound
lowerlimit <- mean(predicted_price) + lower_bound
 
within_cifs <- ifelse(upperlimit > predicted_price & predicted_price > lowerlimit, predicted_price, 0)

length(within_cifs)
within_cifs


x <- rnorm( 100 , 3 , 2 )
n <- length(x)
alpha <- 1 - 0.90
dof <- n - 1
xbar <- mean(x)
Sx <- sqrt(mean((x - xbar)^2))

lowerbound <- xbar - qt(1 - alpha/2, dof) * Sx / sqrt(n)
upperbound <- xbar + qt(1 - alpha/2, dof) * Sx / sqrt(n)

lowerbound
upperbound

par(mfrow = c(1,1))
hist(x, probability = TRUE)
lines(density(x))
abline(v=upperbound)






ggplot(fabia,                                     # Draw plot using ggplot2 package
       aes(x = predicted_price,
           y = Price, size )) +
  geom_point() + geom_smooth(method = 'lm', level = .95) +
  labs(title = 'Actual Price ~ Predicted Price', x = 'Predicted Price', y = 'Actual Price')
  





geom_abline(intercept = -1.235e-11,
            slope = 1.000e+00,
            color = "red",
            size = 2) + 

?geom_smooth


?ggplot2
  
?aes
  
  
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
  
  
  
esd <- lm(Price ~ predicted_price)
esd