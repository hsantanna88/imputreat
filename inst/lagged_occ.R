#-------------------------------------------------------------------------------
# Name: Job Displacement Code
#-------------------------------------------------------------------------------
#
# Description: Script for the Difference-in-Differences with "Bad Covariates"
#  Paper Application (Callaway & Pedro Sant'Anna multiple Time Periods)
#  Trying the lagged occupation
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
library(haven) # Loading DTA
library(fixest) # TWFE regs
library(did) # Callaway's DID package

# devtools::install_github("bcallaway11/pte")
library(pte) # Callaway's PTE package   



#-------------------------------------------------------------------------------
# Wrangling Part
#-------------------------------------------------------------------------------

# Loading the dataset

# Please, change the data name accordingly (and working dir)
df_raw <- read_dta("data/wagescars.dta")


# Preparing the dataset for the regressions (multiple periods case)
df <- df_raw %>%
  # Only 1993 and below are important (I will keep 94 to check occ changes)
  filter(year <= 1993) %>%
  # Removing unnecessary NAs
  filter(!(is.na(sep1))) %>%
  filter(!(is.na(occ1990))) %>%
  filter(!(is.na(wage)) & wage != 0) %>% 
  filter(!is.na(age_base)) %>% 
  filter(!is.na(female_base)) %>% 
  filter(!is.na(white_base)) %>% 
  filter(!is.na(black_base)) %>% 
  filter(!is.na(raceothr_base)) %>% 
  filter(!is.na(hispanic_base)) %>% 
  filter(!is.na(mothed_base)) %>% 
  filter(!is.na(fathed_base)) %>% 
  filter(!is.na(r0000100)) %>% 
  filter(!is.na(numjobs)) %>% 
  filter(!is.na(exper))

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
  select(occ_score, year_sep, year, id, age_base, female_base, white_base,
         mothed_base, fathed_base, r0000100, numjobs, exper)




#-------------------------------------------------------------------------------
# Application Part
#-------------------------------------------------------------------------------

##### Try using PTE package #####

xformla <- ~ age_base + female_base + white_base +
  mothed_base + fathed_base + r0000100 + numjobs + exper + I(exper^2)

lagged_reg <- pte_default(
  yname  = "occ_score",
  gname  = "year_sep",
  tname  = "year",
  idname = "id",
  data = df,
  xformla = xformla,
  base_period = "varying",
  lagged_outcome_cov=TRUE
)

summary(lagged_reg)


#-------------------------------------------------------------------------------
# Preliminary Results -  Lagged outcome cov = TRUE
#-------------------------------------------------------------------------------
# Overall ATT:  
#   ATT    Std. Error     [ 95%  Conf. Int.]  
# -0.4297        0.0506    -0.5289     -0.3305 *
#   
#   
#   Dynamic Effects:
#   Event Time Estimate Std. Error   [95%  Conf. Band]  
# -8  -0.1236     0.2530 -0.9102      0.6629  
# -7  -0.2095     0.2282 -0.9188      0.4999  
# -6  -0.3395     0.1294 -0.7419      0.0629  
# -5  -0.2596     0.1729 -0.7972      0.2780  
# -4  -0.4128     0.1786 -0.9681      0.1425  
# -3  -0.3595     0.1385 -0.7901      0.0710  
# -2  -0.1905     0.1538 -0.6688      0.2878  
# -1  -0.4170     0.1119 -0.7650     -0.0689 *
#  0  -0.4265     0.1092 -0.7659     -0.0871 *
#  1  -0.3981     0.0881 -0.6722     -0.1241 *
#  2  -0.2927     0.1119 -0.6407      0.0553  
#  3  -0.3353     0.1514 -0.8059      0.1354  
#  4  -0.4733     0.1624 -0.9783      0.0318  
#  5  -0.3938     0.1341 -0.8108      0.0233  
#  6  -0.3543     0.1329 -0.7676      0.0590  
#  7  -0.8224     0.1050 -1.1488     -0.4960 *
#  8  -0.3400     0.2210 -1.0269      0.3470  
# ---
#   Signif. codes: `*' confidence band does not cover 0


lagged_reg <- pte_default(
  yname  = "occ_score",
  gname  = "year_sep",
  tname  = "year",
  idname = "id",
  data = df,
  xformla = xformla,
  base_period = "varying",
  d_outcome=TRUE
)

summary(lagged_reg)

event_study_boot <- rep(0, 1000)

for(i in 1:1000) {
  boot_df <- df[sample(1:nrow(df), nrow(df), replace=TRUE),]
  lagged_reg_boot <- pte_default(
    yname  = "occ_score",
    gname  = "year_sep",
    tname  = "year",
    idname = "id",
    data = df,
    xformla = xformla,
    base_period = "varying",
    d_outcome=TRUE
  )
  event_study_boot <- summary(lagged_reg_boot)$overall_att[1]
}

# Overall ATT:  
#   ATT    Std. Error     [ 95%  Conf. Int.] 
# -0.0742        0.0959    -0.2622      0.1138 
# 
# 
# Dynamic Effects:
#   Event Time Estimate Std. Error   [95%  Conf. Band] 
# -8   0.0808     0.3570 -0.9761      1.1376 
# -7  -0.0641     0.1597 -0.5369      0.4087 
# -6  -0.1720     0.1740 -0.6873      0.3432 
# -5   0.0899     0.1515 -0.3588      0.5385 
# -4  -0.0318     0.1861 -0.5828      0.5193 
# -3   0.0020     0.1214 -0.3574      0.3613 
# -2   0.1941     0.1630 -0.2885      0.6768 
# -1  -0.2191     0.1284 -0.5993      0.1612 
#  0  -0.0775     0.1168 -0.4232      0.2682 
#  1  -0.0147     0.0865 -0.2709      0.2415 
#  2   0.0698     0.0951 -0.2118      0.3513 
#  3   0.0243     0.1598 -0.4487      0.4974 
#  4  -0.0091     0.2335 -0.7003      0.6820 
#  5  -0.0180     0.1538 -0.4735      0.4375 
#  6  -0.0196     0.1499 -0.4635      0.4243 
#  7  -0.5706     0.1935 -1.1435      0.0024 
#  8  -0.0892     0.2678 -0.8821      0.7036 
# ---
#   Signif. codes: `*' confidence band does not cover 0



#TWFE Regression

#df_treated <- subset(df, year_sep > 0)

summary(feols(occ_score ~ as.numeric(year_sep <= year) + age_base + female_base + white_base +
        mothed_base + fathed_base + r0000100 + numjobs + exper + I(exper^2) | year + id,
      data = df))$coefficients[1]

#ATT = -0.03037478

boot_coef <- rep(0, 1000)

for(i in 1:1000) {
  boot_df <- df[sample(1:nrow(df), nrow(df), replace=TRUE),]
  boot_coef[i] <- summary(feols(occ_score ~ as.numeric(year_sep - year <= 0) + age_base + female_base + white_base +
                                  mothed_base + fathed_base + r0000100 + numjobs + exper + I(exper^2) | year + id,
                                data = boot_df))$coefficients[1]
}

#SE for TWFE
sd(boot_coef)

#0.06744974

lagged_reg <- pte_default(
  yname = "occ_score",
  gname = "year_sep",
  tname = "year",
  idname = "id",
  data = df,
  xformla = xformla,
  base_period = "varying",
  d_outcome = TRUE
)

glimpse(df)

df <- df %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(lag_exper = lag(exper), lag_numjobs = lag(numjobs)) %>%
  ungroup()

unique(df$year_sep)

treated_groups <- unique(df$year_sep)[unique(df$year_sep) != 0]

control_df <- subset(df, df$year_sep == 0)

control_df$lag_exper <- lag(control_df$exper)

model_exper <- fixest::feols(exper ~ lag_exper, data=control_df)
model_numjobs <- fixest::feols(numjobs ~ lag_numjobs, data=control_df)

df$imput_exper <- 0

df$imput_exper <- ifelse(df$year_sep > df$year | df$year_sep == 0, df$exper, 0)

# df$imput_exper <- ifelse(df$year_sep == df$year,
#                          df$lag_exper * coef(model_exper)[2] + coef(model_exper)[1],
#                          df$imput_exper)

df$imput_exper <- ifelse(df$year_sep >= df$year,
                         df$lag_exper[df$year_sep == df$year, ] * coef(model_exper)[2] *(df$year_sep - df$year + 1) + 
                         coef(model_exper)[1],
                         df$imput_exper)

# df$imput_exper <- ifelse(df$year_sep > df$year,
#                          df$imput_exper * )



df <- df %>%
  group_by(year_sep) %>%
  mutate(
    imput_exper = case_when(
      year_sep == 0 ~ exper,
      TRUE ~ ifelse(year >= year_sep, predict(model_exper, newda))
    )
  )
















