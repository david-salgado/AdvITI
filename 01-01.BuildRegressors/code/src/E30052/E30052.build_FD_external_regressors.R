

build_FD_external_regressors <- function(dt, external.list){
  
  ################### CRUCE CON IPI ################
  
  ### ipi con CNAE a 4 dígitos cruzado con CCAA
  ipi4ca <- external.list[["ipi4ca"]]
  
  dt <- merge(dt, ipi4ca, by = c('actiFF_1', 'ccaa', 'year', 'month'), all.x = TRUE)
  
  ### ipi con CNAE a 4 dígitos 
  ipi4 <- external.list[["ipi4"]]
  
  dt <- merge(dt, ipi4, by = c('actiFF_1', 'year', 'month'), all.x = TRUE)
  
  ### ipi con CNAE a 3 dígitos cruzado con CCAA
  ipi3ca <- external.list[["ipi3ca"]]
  
  dt<- merge(dt, ipi3ca, by = c('CNAE3_est', 'ccaa', 'year', 'month'), all.x = TRUE)
  
  ### ipi con CNAE a 3 dígitos 
  ipi3 <- external.list[["ipi3"]]
  
  dt <- merge(dt, ipi3, by = c('CNAE3_est', 'year', 'month'), all.x = TRUE)
  
  ################### CRUCE CON IPRI ################
  
  ### ipi con CNAE a 4 dígitos cruzado con CCAA
  ipri_4_ca <- external.list[["ipri_4_ca"]]
  
  dt <- merge(dt, ipri_4_ca, by = c('actiFF_1', 'ccaa', 'year', 'month'), all.x = TRUE)
  
  ### ipi con CNAE a 4 dígitos 
  ipri_4 <- external.list[["ipri_4"]]
  
  dt <- merge(dt, ipri_4, by = c('actiFF_1', 'year', 'month'), all.x = TRUE)
  
  ### ipi con CNAE a 3 dígitos cruzado con CCAA
  ipri_3_ca <- external.list[["ipri_3_ca"]]
  
  dt <- merge(dt, ipri_3_ca, by = c('CNAE3_est', 'ccaa', 'year', 'month'), all.x = TRUE)
  
  ### ipi con CNAE a 3 dígitos 
  ipri_3 <- external.list[["ipri_3"]]
  
  dt <- merge(dt, ipri_3, by = c('CNAE3_est', 'year', 'month'), all.x = TRUE)
  
  #### remove NA columns not present in IPI and IPRI data
  dt[, c('tt_3','tt_3ca','tt_4ca','tt_4') := NULL]
  
  return(dt)
  
}

