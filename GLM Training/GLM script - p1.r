library(insuranceData)
library(dplyr)

data(dataCar)
colnames(dataCar)
head(dataCar, 2)
# variables
# veh_value = vehicle value in $10,000s
# exposure  = 0-1 fraction of year the policy was active (eg. 0.5 = half year)
# clm       = claim occured? (0 = no, 1 = yes)
# numclaims = claim count
# claimcst0 = claim cost/amount
# veh_body  = category of vehicle body (BUS, CONVT (Convertible), COUPE, HBACK (Hatchback), HDTOP (Hardtop), MCARA, MIBUS (Minibus), PANVN (Panel Van), RDSTR (Roadster), SEDAN, STNWG (Station Wagon), TRUCK, and UTE (Utility))
# veh_age   = category for age 1,2,3,4
# gender    = M/F
# area      = geographical area category A,B,C,D,E,F
# agecat    = driver age category 1,2,3,4,5,6

# model 1 : claim cost ~ vehicle value
linear_fit = lm(claimcst0 ~ veh_value, data= dataCar)
summary(linear_fit)

plot(dataCar$veh_value,dataCar$claimcst0, xlab='vehicle value', ylab='claim cost')
abline(linear_fit, col='red',lwd=2)
# equation: claim_cost = 121.9 + 8.8*vehicle_value
# r-square = 0.009% (very very very poor)

# lets look at the output variable's distribution
range(dataCar$claimcst0) # $0 to $55922
length(dataCar$claimcst0[dataCar$claimcst0==0]) # 0 claim amt occurs 63,232 times
length(dataCar$claimcst0)

y = dataCar$claimcst0[dataCar$claimcst0!=0 & dataCar$claimcst0 <= 10000]
x = dataCar$veh_value[dataCar$claimcst0!=0 & dataCar$claimcst0 <= 10000]
plot(density(y))
# right skewed - normal distribution will not work

fit1 = glm(y ~ x, family = Gamma(link = "inverse"))
summary(fit1)
coef(fit1)
# equation: 1/y = 0.00055 + 0.000061*x
# => y = 1/(0.00055 + 0.000061*x)
x[1] # vehicle value = 1.66
0.00055 + 0.000061*x[1] # 0.00065 
predict(fit1, data.frame(x=x[1])) # 0.00065 

1/(0.00055 + 0.000061*x[1]) # y = 1535.485
predict(fit1, data.frame(x=x[1]), type = 'response') # y = 1528 
y[1] # actual claim = 669.51

fit2 = glm(y ~ x, family = Gamma(link = 'log'))
summary(fit2)
coef(fit2)
# equation: log(y) = 7.463 - 0.077*x
# => y = exp(7.463 - 0.077*x)
exp(7.46293253 -0.07722777*x[1]) # prediction = 1532.6
predict(fit2, data.frame(x=x[1]), type = 'response') # y = 1532.6
y[1] # actual claim = 669.51

# which is model better? how to compare?
summary(fit1)
summary(fit2)
# AIC = -2*log_likelihood + 2*k
# log_likelihood measures how well the model fits the data
# k = parameter count
# lower the AIC, better the model (clearly)
AIC(fit1) # 74486 (lower)
AIC(fit2) # 74490

# fit1 is better

## lets add a categorical variable - gender
gender = dataCar$gender[dataCar$claimcst0!=0 & dataCar$claimcst0 <= 10000]

fit3 = glm(y ~ x + gender, family = Gamma)
# equation: 1/y = 0.00057 + 0.00006*x - 0.00004*gender_male
1/(0.00057 + 0.00006*x[1] - 0.00004*1) # gender[1] is Male
predict(fit3, data.frame(x=x[1], gender='M'), type = 'response') # y = 1583.94
y[1]
1/(0.00057 + 0.00006*x[2] - 0.00004*0) # gender[2] is female
predict(fit3, data.frame(x=x[2], gender='F'), type = 'response') # y = 1511.09
y[2]

#-------------------------------------------------------------------------------
## R-squared in GLM
#-------------------------------------------------------------------------------
# R-squared measure is strictly defined for linear regression
# where we use (1) OLS (ordinary least square) method for parameter estimation
# and (2) have errors ~ Normal distribution
# we cannot calculate R2-squared in case of GLM
# we thus rely on AIC and other metrics

#-------------------------------------------------------------------------------
## interaction effects
#-------------------------------------------------------------------------------
# current model believes that veh_value (x) and gender are independent of each other
# but, in real life, say that vehicle_value can vary based on gender
# thus, there is need to include a term that can capture this interaction between gender and vehicle_value
fit4 = glm(y ~ x + gender + x:gender, family = Gamma(link = 'inverse'))
coef = round(coef(fit4),5)
# equation: 1/y = 0.00055 + 0.00007*x -0.00001*gender_male -0.00002*(x:gender_male)

1/(coef[1] + coef[2]*x[1] + coef[3]*1 + coef[4]*x[1]*1) # 1605.136
1/(coef[1] + coef[2]*x[2] + coef[3]*0 + coef[4]*x[2]*0) # 1525.088

AIC(fit4) # 74485 (slightly lower than fit3 - thus better) 


