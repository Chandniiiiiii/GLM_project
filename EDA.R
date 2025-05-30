# Load required packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(skimr)
library(DataExplorer)

# Load the dataset
data <- read_csv("data.csv")

# View initial structure
glimpse(data)

# ---- DATA FORMATTING ----

# Convert dates
data <- data %>%
  mutate(
    Date_start_contract = dmy(Date_start_contract),
    Date_last_renewal = dmy(Date_last_renewal),
    Date_next_renewal = dmy(Date_next_renewal),
    Date_birth = dmy(Date_birth),
    Date_driving_licence = dmy(Date_driving_licence),
    Date_lapse = dmy(Date_lapse)
  )


# Convert to appropriate types
data <- data %>%
  mutate(
    Distribution_channel = factor(Distribution_channel, labels = c("Agent", "Broker")),
    Payment = factor(Payment, labels = c("Annual", "Half-yearly")),
    Type_risk = factor(Type_risk, labels = c("Motorbike", "Van", "Car", "Agricultural")),
    Area = factor(Area, labels = c("Rural", "Urban")),
    Second_driver = factor(Second_driver, labels = c("No", "Yes")),
    Type_fuel = factor(Type_fuel),
    N_doors = as.integer(N_doors),
    Year_matriculation = as.integer(Year_matriculation)
  )

# ---- FEATURE ENGINEERING ----

# Calculate age of insured and license age
data <- data %>%
  mutate(
    age = interval(Date_birth, Date_start_contract) / years(1),
    license_years = interval(Date_driving_licence, Date_start_contract) / years(1),
    policy_duration_years = interval(Date_start_contract, Date_next_renewal) / years(1)
  )

# ---- EDA ----

# Summary stats
skim(data)

# Plot distribution of premiums
ggplot(data, aes(x = Premium)) +
  geom_histogram(bins = 50, fill = "#69b3a2", color = "white") +
  labs(title = "Distribution of Premiums", x = "Premium", y = "Count")

# Claims frequency
ggplot(data, aes(x = N_claims_year)) +
  geom_bar(fill = "#ff7f0e") +
  labs(title = "Number of Claims per Policy", x = "Claims This Year", y = "Count")

# Claims cost distribution (log scale)
ggplot(data, aes(x = Cost_claims_year)) +
  geom_histogram(bins = 50, fill = "#1f77b4", color = "white") +
  scale_x_log10() +
  labs(title = "Distribution of Claim Costs (log scale)", x = "Cost of Claims", y = "Count")

# Policy type vs claims
ggplot(data, aes(x = Type_risk, fill = as.factor(N_claims_year > 0))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Claims by Vehicle Type", fill = "Has Claims")

# Automatic report (optional)
# create_report(data)  # From DataExplorer

