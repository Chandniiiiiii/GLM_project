library(insuranceData)
library(dplyr)
library(MuMIn)

# Load and filter data
data(dataCar)
df <- dataCar %>% filter(claimcst0 > 0, claimcst0 <= 10000) %>% mutate(across(c(gender, veh_body, veh_age, area, agecat), as.factor))

# full model - gamma
full_model <- glm(claimcst0 ~ veh_value * (gender + veh_body + veh_age + area + agecat), data = df, family = Gamma(link = "inverse"))
# Ensure na.action is set to "na.fail" (required by dredge)
options(na.action = "na.fail")
# Run dredge
model_dredge <- dredge(full_model, rank = "AIC",trace = T) # +2000 models were fitted
# best model - gamma
best_model <- get.models(model_dredge, subset = 1)[[1]]
summary(best_model) # aic = 74,434 (better)

# full model - normal
full_model_normal <- glm(claimcst0 ~ veh_value * (gender + veh_body + veh_age + area + agecat), data = df, family = gaussian)
options(na.action = "na.fail")
model_dredge_normal <- dredge(full_model_normal, rank = "AIC")
best_model_normal <- get.models(model_dredge_normal, subset = 1)[[1]]
summary(best_model_normal) # aic = 80,115 (bad)

#-------------------------------------------------------------------------------
## Residual analysis
#-------------------------------------------------------------------------------
par(mfrow = c(2, 2))  # 2x2 grid of plots
plot(best_model)
par(mfrow = c(1, 1))
