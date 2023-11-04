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
library(data.table) # Fast data manipulation
library(feather) # Fast data loader

# Loading the dataset
dt <- read_feather("data/wagescars_wrangled.feather")

group_var = "year_sep"
time_var = "year"
imput_var = "exper"
id_var = "id"

imputreat = function(dt, imput_var, group_var, id_var, time_var) {

  # Ensure the data is a data.table
  setDT(dt)

  # Create the lagged variable by reference
  dt[,
    lag_var := shift(get(imput_var)),
      by = get(id_var)][order(get(id_var), get(time_var))]

  # Estimating the beta coefficients
  frml = formula(paste0(imput_var, " ~ lag_var"))
  beta = coef(fixest::feols(frml, data=dt))
  beta1 = beta[1]
  beta2 = beta[2]

  print(beta1)
  print(beta2)

  # Get the lagged variable for right before treatment
  dt[, imput_obs := fifelse(get(group_var) == get(time_var), lag_var, NA_real_), by = get(id_var)]

  dt[, imput_obs := nafill(imput_obs, type = "locf"), by = get(id_var)]

  # Step 3: Ensure that the control group (group_var == 0) has NA for imput_obs
  dt[get(group_var) == 0, imput_obs := NA_real_]

  dt[, imput := {
    T = get(time_var) - get(group_var) + 1
    fifelse(
      get(group_var) != 0 & get(time_var) >= get(group_var),
      sapply(T, function(t) {
        beta1 * sum(beta2^(0:(t - 1)))}) + imput_obs * beta2^T,
      get(imput_var)
    )
  }, by = .(get(id_var))]
}

# END OF SCRIPT