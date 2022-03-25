identifySample <- function(data_refPeriod.dt, data_precPeriod.dt, data_precPeriod2.dt = NULL,
                           idVar, type_refPeriod){
  
  # A partir de los conjuntos de datos data_running (mes en curso) y data_ref 
  # (mes anterior) se determina la muestra para el cómputo de los índices 
  # elementales utilizando los códigos de validez, de actualización, de 
  # actividad y comunidad autónoma
  # data_refPeriod.dt <- dcast_StQ(data_refPeriod, UnitNames = TRUE)
  # data_precPeriod.dt <- dcast_StQ(data_precPeriod, UnitNames = TRUE)
  
  for (idv in idVar){
    
    data_refPeriod.dt <- data_refPeriod.dt[get(idv) != '']
    data_precPeriod.dt <- data_precPeriod.dt[get(idv) != '']
  }

  if(type_refPeriod == "FF"){
    
    # Condición 1: mismo numidest, ccaa, cnae a 3 dígitos
    temp_refPeriod.dt <- data_refPeriod.dt[
      , .(numidest, ccaa, acti)][
        , acti3 := substr(acti, 1, 3)][
          , acti := NULL]
    temp_precPeriod.dt <- data_precPeriod.dt[
      , .(numidest, ccaa, acti)][
        , acti3 := substr(acti, 1, 3)][
          , acti := NULL]
    sample_cond1.dt <- merge(temp_refPeriod.dt, temp_precPeriod.dt, by = c('numidest', 'ccaa', 'acti3'))
    data_refPeriod.dt <- data_refPeriod.dt[sample_cond1.dt[, .(numidest)], on = 'numidest']
    data_precPeriod.dt <- data_precPeriod.dt[sample_cond1.dt[, .(numidest)], on = 'numidest']
    
    # Condicion2: running y ref: cn01v == 1 
    data_refPeriod_cond2.dt <- data_refPeriod.dt[cn01v == 1]
    data_prec_cond2.dt <- data_precPeriod.dt[cn01v == 1]
    numidest_cond2.dt <- merge(
      data_refPeriod_cond2.dt[, ..idVar], data_prec_cond2.dt[, ..idVar], by = idVar)
    
    # Condicion3: running: cn01v == 1 y ref: cn01v == 2
    data_refPeriod_cond3.dt <- data_refPeriod.dt[cn01v == 1]
    data_prec_cond3.dt <- data_precPeriod.dt[cn01v == 2]
    numidest_cond3.dt <- merge(
      data_refPeriod_cond3.dt[, ..idVar], data_prec_cond3.dt[, ..idVar], by = idVar)
    
    # Condicion4: running: cn01v = 1 y ref: cn01a = 3
    data_refPeriod_cond4.dt <- data_refPeriod.dt[cn01v == 1]
    data_prec_cond4.dt <- data_precPeriod.dt[cn01a == 3]
    numidest_cond4.dt <- merge(
      data_refPeriod_cond4.dt[, ..idVar], data_prec_cond4.dt[, ..idVar], by = idVar)
    
    numidest_cond.dt <- rbindlist(list(numidest_cond2.dt, numidest_cond3.dt, numidest_cond4.dt))
    numidest_cond.dt <- numidest_cond.dt[!duplicated(numidest_cond.dt, by = idVar)]
    
  }
  if(type_refPeriod == "FD"){
    
    # Condición 1: mismo numidest, ccaa, cnae a 3 dígitos
    temp_precPeriod.dt <- data_precPeriod.dt[
      , .(numidest, ccaa, acti)][
        , acti3 := substr(acti, 1, 3)][
          , acti := NULL]
    temp_precPeriod2.dt <- data_precPeriod2.dt[
      , .(numidest, ccaa, acti)][
        , acti3 := substr(acti, 1, 3)][
          , acti := NULL]
    sample_cond1.dt <- merge(temp_precPeriod2.dt, temp_precPeriod.dt, by = c('numidest', 'ccaa', 'acti3'))
    
    data_refPeriod.dt <- data_refPeriod.dt[sample_cond1.dt[, .(numidest)], on = 'numidest']
    data_precPeriod.dt <- data_precPeriod.dt[sample_cond1.dt[, .(numidest)], on = 'numidest']
    
    # Condicion2 y 4: prec: cn01a %in% c(1, 2, 3, 6, 7, 8) 
    data_prec_cond.dt <- data_precPeriod.dt[cn01a %in% c(1, 2, 3, 6, 7, 8)]
    numidest_cond.dt <- merge(data_refPeriod.dt[, ..idVar], 
                              data_prec_cond.dt[, ..idVar], 
                              by = idVar)
    
  }
  
  data_refPeriod.dt <- data_refPeriod.dt[numidest_cond.dt, on = idVar]
  sample.dt <- data_refPeriod.dt[, idVar, with = FALSE]
  
  return(sample.dt)
}