#-------------------------------------------------------------------------------
# Name: Fast Data Wrangling
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


# Loading the dataset
library(tidyverse) # Data Wrangling
library(tidylog) # Tidyverse logs
library(haven) # Loading DTA
library(feather) # Loading feather


# Please, change the data name accordingly (and working dir)
df <- read_dta("data/wagescars.dta") %>%
  # Only 1993 and below are important (I will keep 94 to check occ changes)
  filter(year <= 1993) 
total_years <- n_distinct(df$year)

# Creating occ score from occ1990 variable
# This variation allows changes per year AND
#          occupation score becomes a 1-4 scale
occscore_dt <- df %>%
  group_by(occ1990, year) %>%
  mutate(
    occ_score = median(wagesalary / 100, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(occ_score != 0 & !is.na(occ_score)) %>%
  group_by(year) %>%
  mutate(
    occ_score = case_when(
      occ_score < quantile(occ_score, 0.25) ~ 1,
      occ_score < quantile(occ_score, 0.5) &
        occ_score >= quantile(occ_score, 0.25) ~ 2,
      occ_score < quantile(occ_score, 0.75) &
        occ_score >= quantile(occ_score, 0.5) ~ 3,
      occ_score >= quantile(occ_score, 0.75) ~ 4
    )
  ) %>%
  select(occ1990, occ_score, year) %>%
  distinct()

# merging the datasets
df <- df %>%
  left_join(occscore_dt, by = c("occ1990", "year")) %>%
  distinct()


# Back to the main dataset. First we filter individuals reporting all years
df <- df %>%
  group_by(id) %>%
  mutate(
    year_sep = ifelse(sep1 == 1, year, 0),
    year_sep = max(year_sep)
  ) %>%
  # We arrange by year for the occupation change check
  arrange(year) %>%
  # Checking if they ever changed occupation
  mutate(
    treated     = ifelse(year >= year_sep & year_sep > 0, 1, 0),
    changed_occ = ifelse(occ_score != lead(occ_score), 1, 0)
  ) %>%
  filter(occ_score != 0 & !is.na(occ_score)) %>%
  filter(n_distinct(year) == total_years) %>%
  ungroup() %>% 
  mutate(lag_occscore = lag(occ_score))


df <- df %>% 
  select(occ_score, year_sep, sep1, year, id, age_base, female_base, white_base,
         mothed_base, fathed_base, r0000100, numjobs, exper, occ1990, wage)

df <- df[!is.na(df$year_sep),]

write_feather(df, "data/wagescars_wrangled.feather")
# END OF SCRIPT
