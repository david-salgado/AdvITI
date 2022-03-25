setup_predictions <- function(data.dt, prediction_name){
  
  data.dt[get(prediction_name) < 4, (prediction_name) := 4]
  
  data.dt[meses_facturando12 == 12, (prediction_name) := 4]
  
  data.dt[is.na(cn01_1), cn01_1 := 4]
  
  data.dt[, imputada := ifelse(is.na(cn01), 1, 0)]
  
  data.dt[, final_prediction := cn01]
  data.dt[imputada == 1, final_prediction := get(prediction_name)]
  
  return(data.dt)
}