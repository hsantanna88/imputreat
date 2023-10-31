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

# Testing with lag_exper
df <- df %>%
  mutate(lag_exper = lag(exper))


beta <- coef(fixest::feols(exper ~ lag_exper, data=filter(df, year_sep == 0)))


# Create a function for the accumulation logic
update_exper <- function(previous_exper, year, year_sep) {
  if (year >= year_sep & !is.na(previous_exper)) {
    return(beta[1] + previous_exper * beta[2])
  } else {
    return(previous_exper)
  }
}


# Updating the treatment group variables
df <- df %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(
    exper = ifelse(year_sep != 0, pmap_dbl(
      list(previous_exper = c(first(exper), exper[-n()]), 
           year, year_sep), 
      update_exper
    ), exper),
  ) %>%
  ungroup()

# END OF SCRIPT