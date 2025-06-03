# devtools::install_github("dutangc/CASdatasets")
library(CASdatasets)
library(dplyr)
library(tidyverse)

#-------------------------------------------------------------------------------
# 1. Data preparation
#-------------------------------------------------------------------------------
# load("C:/Users/Kunal/Downloads/GLM_project-main/GLM_project-main/freMTPL2sev.rda")
# load("C:/Users/Kunal/Downloads/GLM_project-main/GLM_project-main/freMTPL2freq.rda")

# open R Data file from your folder 

# ?freMTPL
freq <- freMTPL2freq
dim(freq)
colnames(freq)
glimpse(freq)
# IDpol    - policyholder id
# ClaimNb  - number of claims during the exposure period
# exposure - period of exposure for a policy (in years)
# vehPower - power of car
# vehAge   - vehicle age (in years)
# DrivAge  - driver age (in years)
# BonusMalus - bonus/malus, between 50 and 350: <100 means bonus, >100 means malus in france
# VehBrand - car brand
# VehGas - diesel or regular
# area - from 'A' for rural area to 'F' to urban center
# density - population density where car driver lives
# region - policy region in france, based on 1970-2015 classification
sev <- freMTPL2sev
dim(sev)
colnames(sev)
glimpse(sev)
# IDpol - policyholder id
# ClaimAmount - cost of claims,as at recent date

# calculate claims count based on severity data
sev$ClaimNb <- 1
sev_agg <- aggregate(sev$ClaimNb, by = list(IDpol = sev$IDpol), FUN = sum)
head(sev_agg)

# merge frequency and severity data
data <- merge(x = freq[,-2],y = sev_agg, by = 'IDpol',all.x = T)
data[is.na(data)] <- 0
colnames(data)[12] <- 'ClaimNb'
glimpse(data)

# data preparation assumptions -
# (1) claim count should be less than 4
table(data$ClaimNb)
data$ClaimNb <- pmin(data$ClaimNb, 4)
# (2) exposure is 1
data$Exposure <- pmin(data$Exposure, 1)
# (3) max driver age is 90
data$DrivAge <- pmin(data$DrivAge, 90)
# (4) max vehicle age is 40
data$VehAge <- pmin(data$VehAge, 90)

glimpse(data)

## write excel file
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "data")
writeData(wb, sheet = "data", x = data)
saveWorkbook(wb, "data.xlsx",overwrite = TRUE)
