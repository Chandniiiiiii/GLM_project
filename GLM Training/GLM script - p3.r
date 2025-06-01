library(insuranceData)
library(dplyr)
library(statmod)
library(tweedie)
library(MASS)

# Load and filter data
data(dataCar)
df <- dataCar %>% filter(claimcst0 > 0, claimcst0 <= 10000) %>% mutate(across(c(gender, veh_body, veh_age, area, agecat), as.factor))

fit <- glm(claimcst0 ~ veh_value + veh_age + gender + area, data = df,
           family = tweedie(var.power = 1.5, link.power = 0))
# var.power = 1.5 (Tweedie power parameter), link.power = 0 is log-link function
summary(fit)

head(df,1)
# veh_value = 1.66
# veh_Age = 3
# gender = M
# area = B
predict(fit, newdata = df[1, ], type = "response")  # 1587.97
