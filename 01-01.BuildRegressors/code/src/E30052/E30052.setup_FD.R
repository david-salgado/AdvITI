setup_FD <- function(FD_current.dt, FF.dt, refPeriod){
  
  # Preparación del FD para construir los regresores en los siguientes pasos
  # Se incorpora información de la historical_FF
  
  
  mmaaaa <- substr(refPeriod, 3, 8) # get current monthyear: MMYYYY
  setnames(FD_current.dt, 'acti', 'actiFD')
  
  for(i in seq(lagsFF)){
    
    mmaaaa_im  <- substr(getRepo(newRepoTime(refPeriod) - months(i)), 3, 8)
    
    FF_im.dt <- FF.dt[month == substr(mmaaaa_im, 1, 2) & year == substr(mmaaaa_im, 3, 6)]
    
    if(i == 1){
      
      new_names <- c(paste0(c('actiFF', 'cn01','cn01v','cn01a'), '_', i)) 
      setnames(FF_im.dt, c('acti', 'cn01','cn01v','cn01a'), new_names)
      
      new_names <- c('norden', 'ccaa', 'GRUPO', new_names)
      
    }
    if(i > 1){
      
      new_names <- c(paste0(c('cn01','cn01v','cn01a'), '_', i)) 
      setnames(FF_im.dt, c('cn01','cn01v','cn01a'), new_names)
      
      new_names <- c('norden', new_names)
      
    }
    
    
    FD_current.dt <- merge(FD_current.dt, 
                           FF_im.dt[, ..new_names],
                           by = "norden",
                           all.x = TRUE) 
    
  }
  

  FD_current.dt[
    , month := substr(mmaaaa, 1, 2)][
    , year := substr(mmaaaa, 3, 6)]
  
  if(dim(FD_current.dt[duplicated(FD_current.dt$norden)])[1] > 0){
    
    stop(paste0("There are duplicated units in ", refPeriod))
    
  }
  
  ### We drop out the new units because they are not used in the index computation
  FD_current.dt <- FD_current.dt[!is.na(cn01_1)]
  # We drop not used variables 
  vars_remove <- intersect(names(FD_current.dt), 
                           c('Direccion', 'nif', 'razonSocial', 'telefono1', 
                             'Usuario', 'NombreEdit', 'localidad',
                             'cn02', 'cn03', 'cn04', 'cn05', 
                             'cn01a', 'cn02a' , 'cn03a', 'cn04a', 'cn05a'))
  FD_current.dt[, (vars_remove) := NULL]
  
  
  
  # Se le asigna CODCNAEEst(del FD) a los actiFF_1 (acti del mes anterior) que son NA
  FD_current.dt[is.na(actiFF_1), actiFF_1 := CodCNAEEst]
  

  cat(paste("Reference period = ", mmaaaa, "\n"))
  
  return(FD_current.dt)
  
}