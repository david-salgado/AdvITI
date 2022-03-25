
prepare_validatedData <- function(collected_var,
                                  refPeriod, 
                                  repoInfo){


  elementary_cells_vars_micro <- c('ccaa', 'GRUPO')
  elementary_cells_vars_pond <- c('pondccaa', 'codigo')
  elementary_cells_vars_aggr <- c('ccaa', 'codigo') 
  
  corresp_micro_pond.dt <- data.table(elementary_cells_vars_micro, elementary_cells_vars_pond, elementary_cells_vars_aggr)
  setnames(corresp_micro_pond.dt, c('micro', 'pond', 'aggr'))
  
  
  # Read previous and current FF
  DDversion <- repoInfo$DDversion
  path_repo <- repoInfo$path_repo
  
  DD <- RepoXLSToDD(file.path(path_repo, paste0('E30052.NombresVariables_V', DDversion, '.xlsx')))
  
  previous_ref  <- getRepo(newRepoTime(refPeriod) - months(1))
  
  data_files     <- list.files(path_repo)
  FF_files_names <- data_files[grep(paste0('.FF_B15_V', DDversion), data_files)]
  
  FFfileSpec.dt  <- spec_repoFiles(FF_files_names, last = TRUE)
  
  FFfileSpec_prec.dt <- FFfileSpec.dt[period %in% previous_ref]
  FF_previous_full.StQ <- ReadRepoFile(file.path(path_repo, FFfileSpec_prec.dt$fileName), DD, perl = TRUE)
  
  FFfileSpec_cur.dt <- FFfileSpec.dt[period %in% refPeriod]
  FF_current_full.StQ <- ReadRepoFile(file.path(path_repo, FFfileSpec_cur.dt$fileName), DD, perl = TRUE)

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
  FF_current_full.StQ  <- setVariable(FF_current_full.StQ, GRUPO)
  
  FF_previous.StQ <- FF_previous_full.StQ[NOrden  !=  '']
  FF_current.StQ  <- FF_current_full.StQ[NOrden  !=  '']
  
  FF_previous.dt <- dcast_StQ(FF_previous.StQ, UnitNames = TRUE)
  FF_current.dt  <- dcast_StQ(FF_current.StQ, UnitNames = TRUE)
  
  sample.dt <- identifySample(data_refPeriod.dt = FF_current.dt, 
                              data_precPeriod.dt = FF_previous.dt,
                              idVar = "norden", type_refPeriod = "FF")
  
  FF_current.dt <- FF_current.dt[sample.dt, on = "norden"]

  FF_current_updated.StQ <- melt_StQ(FF_current.dt, DD)
  FF_current_updated.StQ  <- setVariable(FF_current_updated.StQ, GRUPO)
  
  outputVar <- list(data.StQ = FF_current_updated.StQ, 
                 sample.dt = sample.dt, 
                 corresp_micro_pond.dt = corresp_micro_pond.dt)
  
  output <- list(outputVar = outputVar, 
                 FF_previous.StQ = FF_previous_full.StQ)
  
  return(output)
 
}

