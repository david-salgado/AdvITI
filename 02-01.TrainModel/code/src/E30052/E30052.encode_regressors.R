

encode_regressors <- function(data.dt){

  

  #####                          :::::::::::::::                              ####
  #####                             ELIMINACION DE CIERTAS VARIABLES NO NECESARIAS                                  ####

  # columns_to_delete <- c(grep('_3_1',  names(data.dt), value = TRUE),
  #                        grep('_5_1',  names(data.dt), value = TRUE),
  #                        grep('_6_1',  names(data.dt), value = TRUE),
  #                        grep('_8_1',  names(data.dt), value = TRUE),
  #                        grep('_9_1',  names(data.dt), value = TRUE),
  #                        grep('_10_1', names(data.dt), value = TRUE),
  #                        grep('_11_1', names(data.dt), value = TRUE),
  #                        grep('_12_1', names(data.dt), value = TRUE),
  #                        grep('_2_0',  names(data.dt), value = TRUE),
  #                        grep('_4_0',  names(data.dt), value = TRUE),
  #                        grep('_5_0',  names(data.dt), value = TRUE),
  #                        grep('_7_0',  names(data.dt), value = TRUE),
  #                        grep('_8_0',  names(data.dt), value = TRUE),
  #                        grep('_9_0',  names(data.dt), value = TRUE),
  #                        grep('_10_0', names(data.dt), value = TRUE),
  #                        grep('_11_0', names(data.dt), value = TRUE))
  # 
  # data.dt[, (columns_to_delete) := NULL]
  # gc()


  #####                          :::::::::::::::                              ####
  ### mean encoding de variables categoricas de + de 30 categorias ####

  ### USAMOS LA MEDIA A 12 PERIODOS en lugar de la variable target para evitar el sesgo de no respuesta

  categVars <- names(data.dt)[sapply(data.dt, is.character)]
  categVars <- setdiff(categVars, c('norden', 'numidest'))
  
  categVars_up30 <- melt(data.dt[, lapply(.SD, function(x){length(unique(x))}), .SDcols = categVars],
                       measure.vars = categVars,
                       variable.factor = FALSE)[
                  value > 30, variable]
  
  categVars_up30 <- setdiff(categVars_up30, c('norden', 'numidest'))

  for (var in categVars_up30){
    
    data.dt[, (var) := mean(MA_est_12, na.rm = TRUE), by = var][
      , (var) := as.numeric(get(var))]

  }


  #####                          :::::::::::::::                              ####
  #####                         ONE-HOT ENCODING                              ####

  categVars_low30      <- setdiff(categVars, categVars_up30)
  data.dt[, (categVars_low30) := lapply(.SD, as.factor), .SDcols = categVars_low30]
  
  id_vars <- c('norden', 'month', 'year', 'batch')
  categVars_low30_id   <- intersect(categVars_low30, id_vars)
  categVars_low30_noId <- setdiff(categVars_low30, id_vars)
  
  data.dt <- one_hot(data.dt, cols = categVars_low30_noId)
  data.dt <- one_hot(data.dt, cols = categVars_low30_id, dropCols = FALSE)
  
  return(data.dt)

}

