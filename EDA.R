# devtools::install_github("dutangc/CASdatasets")
library(CASdatasets)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
# install.packages('corrplot')
library(corrplot)
library(DataExplorer)

# re-run data prep
source('Data prep.r')

glimpse(data)

#-------------------------------------------------------------------------------
# 2. Exploratory Data Analysis (EDA)
#-------------------------------------------------------------------------------
# numerical variables
num_vars <- c("ClaimNb", "Exposure", "VehPower", "VehAge", "DrivAge", "BonusMalus", "Density")
data %>% select(all_of(num_vars)) %>%
  gather(key = "Variable", value = "Value") %>% ggplot(aes(x = Value)) +
  facet_wrap(~ Variable, scales = "free") +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") + theme_minimal() +
  labs(title = "Histogram of Numeric Variables", x = NULL, y = "Count")
# categorical variables
cat_vars <- c("VehBrand", "VehGas", "Area", "Region")
cat_plots <- lapply(cat_vars, function(var) {
  data %>% count(!!sym(var)) %>%
    ggplot(aes(x = reorder(!!sym(var), -n), y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = var, y = "Count") + theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = 10, face = "bold")) + ggtitle(var)
})
(combined_cat_plots <- wrap_plots(cat_plots, ncol = 2))

# exposure distribution
hist(data$Exposure, prob=T)
lines(density(data$Exposure), col='red', lwd=2)













