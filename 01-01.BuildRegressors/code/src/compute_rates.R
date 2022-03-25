compute_rates <- function (dt, varName, idName, lag_initial, lag_final, by = NULL){
  
  ### Esta función calcula las tasas agrupadas o simples para un lag_initial y un lag_final
  
  varFullName <- ifelse(is.null(by),
                        paste('tasa', varName, lag_final, lag_initial, sep = "_"),
                        paste('tasa', varName, paste(by, collapse = '_'), lag_final, lag_initial, sep = "_"))
  varFinal    <- paste(varName, lag_final, sep = "_")
  varInicial  <- paste(varName, lag_initial, sep ="_")
  
  if (lag_initial != 0){
    
    if (!is.null(by)){
      
      dt[, (varFullName) := fcase(
        abs(mean(get(varFinal), na.rm = TRUE)) < .Machine$double.xmin & abs(mean(get(varInicial), na.rm = TRUE)) < .Machine$double.xmin, 0.0,
        abs(mean(get(varFinal), na.rm = TRUE)) < .Machine$double.xmin & abs(mean(get(varInicial), na.rm = TRUE)) >= .Machine$double.xmin, .Machine$double.xmax,
        default = (mean(get(varInicial), na.rm = TRUE) - mean(get(varFinal), na.rm = TRUE)) / mean(get(varFinal), na.rm = TRUE)), by = by]
    }
    if(is.null(by)){

      dt[, (varFullName) := fcase(
        abs(get(varFinal)) < .Machine$double.xmin & abs(get(varInicial)) < .Machine$double.xmin, 0.0,
        abs(get(varFinal)) < .Machine$double.xmin & abs(get(varInicial)) >= .Machine$double.xmin, .Machine$double.xmax,
        default = (get(varInicial) - get(varFinal)) / get(varFinal)), by = idName]  
    }
  }
  if(lag_initial == 0){
    
    if (!is.null(by)){
      
      dt[, (varFullName) := fcase(
        abs(mean(get(varFinal), na.rm = TRUE)) < .Machine$double.xmin & abs(mean(get(varName), na.rm = TRUE)) < .Machine$double.xmin, 0.0,
        abs(mean(get(varFinal), na.rm = TRUE)) < .Machine$double.xmin & abs(mean(get(varName), na.rm = TRUE)) >= .Machine$double.xmin, .Machine$double.xmax,
        default = (mean(get(varName), na.rm = TRUE) - mean(get(varFinal), na.rm = TRUE)) / mean(get(varFinal), na.rm = TRUE)), by = by]
    
    }
    if (is.null(by)){
      
      dt[, (varFullName) := fcase(
        abs(get(varFinal)) < .Machine$double.xmin & abs(get(varName)) < .Machine$double.xmin, 0.0,
        abs(get(varFinal)) < .Machine$double.xmin & abs(get(varName)) >= .Machine$double.xmin, .Machine$double.xmax,
        default = (get(varName) - get(varFinal)) / get(varFinal)), by = idName]  
    }
  
  }
  
  
  return(dt)
  
}
