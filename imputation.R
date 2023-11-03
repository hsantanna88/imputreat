#-------------------------------------------------------------------------------
# Name: Generating the imputation variable
#-------------------------------------------------------------------------------
#
# Description: Script for the Difference-in-Differences with "Bad Covariates"
#  Paper Application (Callaway & Pedro Sant'Anna multiple Time Periods)
#
#-------------------------------------------------------------------------------
#
# Authors: Stroud Payne (1) and Hugo Sant'Anna (2)
#   (1) Vanderbilt University
#   (2) University of Georgia
#
#-------------------------------------------------------------------------------

# Loading the Libraries
library(tidyverse) # Data Wrangling
library(tidylog) # Tidyverse logs
library(feather) # Fast data loader

# Loading the dataset
df <- read_feather("data/wagescars_wrangled.feather")

# making the lagged experience variable
df <- df %>%
  arrange(id, year) %>% 
  group_by(id) %>% 
  mutate(lag_exper = lag(exper)) %>% 
  ungroup()

# Estimating the beta coefficients
beta <- coef(fixest::feols(exper ~ lag_exper, data=filter(df, year_sep == 0)))
alpha<- beta[2]
beta <- beta[1]

# Create a function for the accumulation logic
df <- df %>%
  group_by(id) %>%
  mutate(
    imput_obs = ifelse(year_sep != 0, lag_exper[year == year_sep], NA)
  ) %>% 
  rowwise() %>% 
  mutate(
    T = year - year_sep + 1,
    imput_exper = ifelse(
      year_sep != 0 & year >= year_sep,
      beta * sum(alpha^(0:(T-1))) + imput_obs * alpha^T,
      exper
    )
  ) %>%
  ungroup() %>% 
  select(-imput_obs, -T)

# END OF SCRIPT