build_target_variable <- function(FD.dt, FF.dt, prev.date, target_name, id_var){
  
  FD.dt[, date := as.Date(paste0(year, "-", month, "-01"), format = "%Y-%m-%d")]
  FF.dt[, date := as.Date(paste0(year, "-", month, "-01"), format = "%Y-%m-%d")] 
  
  FF.dt           <- FF.dt[date <= prev.date]
  FD_refPeriod.dt <- FD.dt[date == prev.date + months(1)]
  
  target_nameFF <- paste0(target_name, "FF")

  setnames(FF.dt, target_name, target_nameFF)
  
  id_varFF <- setdiff(id_var, "batch")
  
  targetFD.dt <- merge(FD.dt, FF.dt[, c(target_nameFF, id_varFF), with = FALSE],
                 by = id_varFF)
  
  targetFD.dt <- rbindlist(list(targetFD.dt, FD_refPeriod.dt), fill = TRUE)
  
  targetFD.dt[, date := NULL]
  
  return(targetFD.dt)
  
}