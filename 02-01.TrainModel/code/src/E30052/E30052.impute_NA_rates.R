impute_NA_rates <- function (dt, varName, lag_initial, lag_final, na_suffix, imputing_suffix){
 
  na_varFullName <- ifelse(nchar(na_suffix) == 0,
                     paste('tasa', varName, lag_final, lag_initial, sep = "_"),
                     paste('tasa', varName, na_suffix, lag_final, lag_initial, sep = "_"))
  
  imp_varFullName <- ifelse(nchar(imputing_suffix) == 0,
                           paste('tasa', varName, lag_final, lag_initial, sep = "_"),
                           paste('tasa', varName, imputing_suffix, lag_final, lag_initial, sep = "_"))
  
  dt[is.na(get(na_varFullName)), (na_varFullName) := get(imp_varFullName)]

  return(dt)
  
}