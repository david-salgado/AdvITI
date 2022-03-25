

build_FD_current_regressors <- function(dt){
  
  #####                          :::::::::::::::                              #### 
  #####                      CREACIÓN DE NUEVAS VARIABLES                     ####
  
  dt[
    , mean_cnae2_est      := mean(cn01, na.rm = TRUE), by = c("CNAE2_est")][
    , mean_cnae2_est_ccaa := mean(cn01, na.rm = TRUE), by = c("CNAE2_est","ccaa")][
    , mean_cnae3_est      := mean(cn01, na.rm = TRUE), by = c("CNAE3_est")][
    , mean_cnae3_est_ccaa := mean(cn01, na.rm = TRUE), by = c("CNAE3_est", "ccaa")][
    , mean_cnae1_est      := mean(cn01, na.rm = TRUE), by = c("CNAE1_est")][
    , mean_cnae1_est_ccaa := mean(cn01, na.rm = TRUE), by = c("CNAE1_est", "ccaa")][
    , mean_actiFF_1       := mean(cn01, na.rm = TRUE), by = c("actiFF_1")][
    , mean_actiFF_1_ccaa  := mean(cn01, na.rm = TRUE), by = c("actiFF_1", "ccaa")]
  
  dt[
    , std_cnae3_est      := sd(cn01, na.rm = TRUE), by = c("CNAE3_est")][
    , std_cnae3_est_ccaa := sd(cn01, na.rm = TRUE), by = c("CNAE3_est", "ccaa")][
    , std_cnae2_est      := sd(cn01, na.rm = TRUE), by = c("CNAE2_est")][
    , std_cnae2_est_ccaa := sd(cn01, na.rm = TRUE), by = c("CNAE2_est", "ccaa")][
    , std_cnae1_est      := sd(cn01, na.rm = TRUE), by = c("CNAE1_est")][
    , std_cnae1_est_ccaa := sd(cn01, na.rm = TRUE), by = c("CNAE1_est", "ccaa")][
    , std_actiFF_1       := sd(cn01, na.rm = TRUE), by = c("actiFF_1")][
    , std_actiFF_1_ccaa  := sd(cn01, na.rm = TRUE), by = c("actiFF_1", "ccaa")]

  
  #####  CALCULO DE TASAS DE VARIACION     ####
  
  # Tasas de variacion por estrato
 
  for (i in seq(1, 12)){
    
    dt <- compute_rates(dt = dt, varName = 'cn01', idName = 'norden', lag_initial = 0, lag_final = i, by = NULL)
    dt <- compute_rates(dt = dt, varName = 'cn01', idName = 'norden', lag_initial = 0, lag_final = i, by = c('estrato'))
    dt <- compute_rates(dt = dt, varName = 'cn01', idName = 'norden', lag_initial = 0, lag_final = i, by = c('CNAE2_est'))
    dt <- compute_rates(dt = dt, varName = 'cn01', idName = 'norden', lag_initial = 0, lag_final = i, by = c('CNAE3_est'))
    dt <- compute_rates(dt = dt, varName = 'cn01', idName = 'norden', lag_initial = 0, lag_final = i, by = c('actiFF_1'))
    dt <- compute_rates(dt = dt, varName = 'cn01', idName = 'norden', lag_initial = 0, lag_final = i, by = c('CNAE2div_emp', 'ccaa'))
    dt <- compute_rates(dt = dt, varName = 'cn01', idName = 'norden', lag_initial = 0, lag_final = i, by = c('CNAE3_est', 'ccaa'))
    dt <- compute_rates(dt = dt, varName = 'cn01', idName = 'norden', lag_initial = 0, lag_final = i, by = c('actiFF_1', 'ccaa'))
    
  }  
  
  #### Metemos variable de si mando dato 
  
  dt[, imputada := ifelse(is.na(cn01), 1, 0)]
  
  return(dt)

  
}

