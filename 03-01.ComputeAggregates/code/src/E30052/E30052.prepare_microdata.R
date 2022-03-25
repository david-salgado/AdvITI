
prepare_microdata <- function(predictions, errors, 
                              error_varname, collected_var,
                              refPeriod, batch, repoInfo){

  final_var <- ifelse(batch == "00", "predictions_model0_t0", "final_prediction")
  name_batch <- ifelse(batch == "00", "01", batch)
  
  elementary_cells_vars_micro <- c('ccaa', 'GRUPO')
  elementary_cells_vars_pond <- c('pondccaa', 'codigo')
  elementary_cells_vars_aggr <- c('ccaa', 'codigo') 
  
  corresp_micro_pond.dt <- data.table(elementary_cells_vars_micro, elementary_cells_vars_pond, elementary_cells_vars_aggr)
  setnames(corresp_micro_pond.dt, c('micro', 'pond', 'aggr'))
  
  
  # Read previous FF
  DDversion <- repoInfo$DDversion
  path_repo <- repoInfo$path_repo
  
  DD <- RepoXLSToDD(file.path(path_repo, paste0('E30052.NombresVariables_V', DDversion, '.xlsx')))
  
  refPeriod_1  <- getRepo(newRepoTime(refPeriod) - months(1))
  refPeriod_2  <- getRepo(newRepoTime(refPeriod) - months(2))
  
  data_files     <- list.files(path_repo)
  FF_files_names <- data_files[grep(paste0('.FF_B15_V', DDversion), data_files)]
  
  FFfileSpec_1.dt  <- spec_repoFiles(FF_files_names, last = TRUE)[
    period %in% refPeriod_1]
  FF_previous_full.StQ <- ReadRepoFile(file.path(path_repo, FFfileSpec_1.dt$fileName), DD, perl = TRUE)
  
  FFfileSpec_2.dt  <- spec_repoFiles(FF_files_names, last = TRUE)[
    period %in% refPeriod_2]
  FF_previous2_full.StQ <- ReadRepoFile(file.path(path_repo, FFfileSpec_2.dt$fileName), DD, perl = TRUE)
  
  
  GRUPO <- new(Class = 'Variable', 
               UnitName = 'GRUPO', 
               IDDD = 'ActivEcono', 
               QualsValues = list(NOrden = '', TipoMicrodato = '13.', TareaProceso = '6.', RolProceso = '6.', Clasificacion = '2.6.4.1.', EsModif ='0'), 
               Length = '5', 
               ClassVar = 'character', 
               ValueRegExp = '[.]+', 
               Formula = as.call(list('actiToSubdivi(ActivEcono_35._6._2.1.2._2.1.4._)')), 
               SlotName = 'MicroData', 
               Literal = '', 
               DDversion = DDversion)
  
  FF_previous_full.StQ <- setVariable(FF_previous_full.StQ, GRUPO)
  FF_previous2_full.StQ <- setVariable(FF_previous2_full.StQ, GRUPO)
  
  FF_previous.StQ  <- FF_previous_full.StQ[NOrden  !=  '']
  FF_previous2.StQ <- FF_previous2_full.StQ[NOrden  !=  '']
  
  total_data     <- FF_previous.StQ$Data[FF_previous.StQ$Data$Total == '680:1.-2.']
  FF_previous.dt <- dcast_StQ(FF_previous.StQ, UnitNames = TRUE)
  FF_previous2.dt <- dcast_StQ(FF_previous2.StQ, UnitNames = TRUE)
  
  FD_current.StQ <- ReadRepoFile(file.path(path_repo, paste0('E30052.FD_V1.', refPeriod, '.D_', as.numeric(name_batch))), DD, perl = TRUE)
  FD_current.dt <- dcast_StQ(FD_current.StQ, UnitNames = TRUE)
  FD_current.dt <- FD_current.dt[, ccaa := provinciaToCCAA(Codprovincia)]
  
  sample.dt <- identifySample(data_refPeriod.dt = FD_current.dt,
                              data_precPeriod.dt = FF_previous.dt,
                              data_precPeriod2.dt = FF_previous2.dt,
                              idVar = "norden", type_refPeriod = "FD")
  
  FD_current.dt <- FD_current.dt[sample.dt, on = "norden"]

  predictions <- predictions[batch == name_batch, 
                                   c(final_var, 'norden', 'batch'), with = FALSE][
                                     , batch := NULL]
 
  setnames(predictions, final_var, collected_var)
  
  FD_current.dt[is.na(cn01), cn01 := 4] # los que sean NA los dejamos a cero por si no aparecen en la muestra
  FD_current.dt <- FD_current.dt[!norden %in%  predictions$norden, c('norden', 'cn01'), with = FALSE]
  FD_current.dt <- rbind(FD_current.dt, predictions)
  
  FD_current.dt[, TipoMicrodato := '07.']
  FD_current.dt[, TareaProceso := '4.']
  FD_current.dt[, TipoMercado := '']
  FD_current.dt[, Total := '680:1.-2.']
  FD_current.dt[, ElementoProceso := '']
  FD_current.dt[, RolProceso := '']
  FD_current.dt[, IDEdit := '']
  FD_current.dt[, Clasificacion := '']
  FD_current.dt[, EsModif := '']
  FD_current.dt[, IDDD := 'CifraNeg']
  setnames(FD_current.dt, 'cn01', 'Value')
  setnames(FD_current.dt, 'norden', 'NOrden')
  setcolorder(FD_current.dt, colnames(total_data))
  
  total_data <- total_data[!NOrden %in%  FD_current.dt$NOrden]
  total_data$Value <- 0
  total_data <- rbind(total_data, FD_current.dt)
  
  FF_previous_updated.StQ      <-  FF_previous.StQ
  FF_previous_updated.StQ$Data <- FF_previous.StQ$Data[FF_previous.StQ$Data$Total != '680:1.-2.']
  FF_previous_updated.StQ$Data <- rbind(FF_previous_updated.StQ$Data, total_data)
  
  outputVar <- list(data.StQ = FF_previous_updated.StQ, 
                 sample.dt = sample.dt, 
                 corresp_micro_pond.dt = corresp_micro_pond.dt)
  

  
  if (batch != '00'){
    #### RMSE  #####
    total_data <- FF_previous.StQ$Data[FF_previous.StQ$Data$Total == '680:1.-2.']
    FD_current.dt <- dcast_StQ(FD_current.StQ, UnitNames = TRUE)
    
    errors <- errors[batch == name_batch, 
                                     c(error_varname, 'norden', 'batch'), with = FALSE][
                                       , batch := NULL]
    
    setnames(errors, error_varname, collected_var)
    
  
    FD_current.dt[is.na(cn01), cn01 := 4] # los que sean NA los dejamos a cero por si no aparecen en la muestra
    FD_current.dt <- FD_current.dt[!norden %in%  errors$norden, c('norden', 'cn01')]
    FD_current.dt <- rbind(FD_current.dt, errors)
    FD_current.dt[is.na(cn01),cn01 := 4]
    FD_current.dt[, TipoMicrodato := '07.']
    FD_current.dt[, TareaProceso := '4.']
    FD_current.dt[, TipoMercado := '']
    FD_current.dt[, Total := '680:1.-2.']
    FD_current.dt[, ElementoProceso := '']
    FD_current.dt[, RolProceso := '']
    FD_current.dt[, IDEdit := '']
    FD_current.dt[, Clasificacion := '']
    FD_current.dt[, EsModif := '']
    FD_current.dt[, IDDD := 'CifraNeg']
    setnames(FD_current.dt, 'cn01', 'Value')
    setnames(FD_current.dt, 'norden', 'NOrden')
    setcolorder(FD_current.dt, colnames(total_data))
    
    total_data <- total_data[!NOrden %in%  FD_current.dt$NOrden]
    total_data$Value <- 0
    total_data <- rbind(total_data, FD_current.dt)
    
    FF_previous_updated.StQ      <-  FF_previous.StQ
    FF_previous_updated.StQ$Data <- FF_previous.StQ$Data[FF_previous.StQ$Data$Total != '680:1.-2.']
    FF_previous_updated.StQ$Data <- rbind(FF_previous_updated.StQ$Data, total_data)
    
    outputError <- list(data.StQ = FF_previous_updated.StQ, 
                   sample.dt = sample.dt, 
                   corresp_micro_pond.dt = corresp_micro_pond.dt)
    
    total_running_rmse.dt <- computeTotal(data.StQ = FF_previous_updated.StQ, 
                                              sample.dt = sample.dt, 
                                              varNames = collected_var, 
                                              cellNames = corresp_micro_pond.dt[['micro']])
    
    setnames(total_running_rmse.dt, corresp_micro_pond.dt[['micro']], corresp_micro_pond.dt[['pond']])

  }else{ outputError <- NULL }
  
  output <- list(outputVar = outputVar, 
                 outputError = outputError, 
                 FF_previous.StQ = FF_previous_full.StQ)
  
  return(output)
 
}

