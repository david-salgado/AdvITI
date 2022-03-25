setup_predictions_t0 <- function(data.dt, prediction_name){

  data.dt[get(prediction_name) < 4, (prediction_name) := 4]
  
  data.dt[meses_facturando12 == 12, (prediction_name) := 4]
  
  return(data.dt)
}