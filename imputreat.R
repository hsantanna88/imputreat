#' Auxiliary function for the Difference-in-Differences with bad covariates. 
#'
#' @param dt A dataset object, in "long" format.
#' @param imput_var String name of the imputation variable.
#' @param group_var String name of the group variable.
#' @param id_var String name of the individual identifier variable.
#' @param time_var String name of the time identifier variable.
#' @return A modified data.table object with new column for the imputation variable.
#' @examples
#' # Assuming `data` is a data.table with appropriate columns:
#' imputed_data <- imputreat(data, "imput_var_name", "group_var_name", 
#'                           "id_var_name", "time_var_name")
#' 
#' @export
#' 
#' @author Stroud Payne (1) and Hugo Sant'Anna (2)
#'   (1) Vanderbilt University
#'   (2) University of Georgia
#'
#' @references
#' Caetano, C., & Callaway, B., & Payne, S., & Sant'Anna, H.(2024). Difference-in-differences With Bad Covariates.
#'
imputreat <- function(dt, imput_var, group_var, id_var, time_var) {

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