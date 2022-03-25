calculate_rate_of_change_batch <- function(data_refPeriod.dt, 
                                           data_precPeriod1.dt, 
                                           data_precPeriod12.dt = NULL,
                                           annual, batch){

  
  if(batch != "00"){

    setnames(data_precPeriod1.dt, c("index", "index_rmse"), c("index_1", "index_rmse_1"))

    if(annual){
      setnames(data_precPeriod12.dt, c("index", "index_rmse"), c("index_12", "index_rmse_12")) 
    }
  }
  if(batch == "00"){
    
    setnames(data_precPeriod1.dt, c("index"), c("index_1"))
    if(annual){
      setnames(data_precPeriod12.dt, c("index"), c("index_12")) 
    }
  }
  
  
  data_precPeriod1.dt <- data_precPeriod1.dt[, c("batch", "refPeriod") := NULL]
  if(annual){
    data_precPeriod12.dt <- data_precPeriod12.dt[, c("batch", "refPeriod") := NULL]
  }
  fulldata.dt <- merge(data_refPeriod.dt, data_precPeriod1.dt,
                       by = intersect(names(data_refPeriod.dt), names(data_precPeriod1.dt)))
  if(annual){
    fulldata.dt <- merge(fulldata.dt, data_precPeriod12.dt,
                         by = intersect(names(fulldata.dt), names(data_precPeriod12.dt)))
  }
  
  fulldata.dt[, index_monthly_rate := (index - index_1) / index_1]
  
  if(annual){
    fulldata.dt[, index_annual_rate := (index - index_12) / index_12]
  }
  
 
  
  if (batch != '00'){
    
    fulldata.dt[, index_monthly_rate_rmse := index_rmse  / index_1]
    if(annual){
      fulldata.dt[, index_annual_rate_rmse := index_rmse  / index_12]
      #fulldata.dt <- fulldata.dt[, index_rmse_12 := NULL]
    }
    
    #fulldata.dt <- fulldata.dt[, index_rmse_1 := NULL]
    
  }
  # fulldata.dt <- fulldata.dt[, index_1 := NULL]
  # if(annual){
  #  fulldata.dt <- fulldata.dt[, index_12 := NULL]
  # }
  
  return(fulldata.dt)
  
} 