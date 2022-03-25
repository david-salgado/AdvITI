
calculate_elementary_index_batch <- function(microdata.list, 
                                                       collected_var, refPeriod, batch){
  
  vars_names <- collected_var
  
  previous_ref  <- getRepo(newRepoTime(refPeriod) - months(1))
  corresp_micro_pond.dt <- microdata.list$outputVar$corresp_micro_pond.dt
  
  total_running.dt <- computeTotal(data.StQ = microdata.list$outputVar$data.StQ, 
                                   sample.dt = microdata.list$outputVar$sample.dt, 
                                   varNames = collected_var, 
                                   cellNames = corresp_micro_pond.dt[['micro']])
  
  setnames(total_running.dt, 
           corresp_micro_pond.dt[['micro']], 
           corresp_micro_pond.dt[['pond']])
  
  if(batch != "00"){
    
    total_running_rmse.dt <- computeTotal(data.StQ = microdata.list$outputError$data.StQ, 
                                          sample.dt = microdata.list$outputError$sample.dt, 
                                          varNames = collected_var, 
                                          cellNames = corresp_micro_pond.dt[['micro']])
    
    setnames(total_running_rmse.dt, 
             corresp_micro_pond.dt[['micro']], 
             corresp_micro_pond.dt[['pond']])
    
  }
  
  
  FF_previous.StQ <- microdata.list$FF_previous.StQ

  total_prec.dt    <- computeTotal(data.StQ = FF_previous.StQ, 
                                  sample.dt = microdata.list$outputVar$sample.dt, 
                                  varNames = collected_var, 
                                  cellNames = corresp_micro_pond.dt[['micro']])
  
  setnames(total_prec.dt, 
           corresp_micro_pond.dt[['micro']], 
           corresp_micro_pond.dt[['pond']])
  
  
  
  indices_prec.dt <- as.data.table(read_sas(file.path(path_data_intermediate_survey, "indices_icn.sas7bdat")))[
    mes ==  substr(previous_ref, 3, 4) & ano ==  substr(previous_ref, 5, 8)][
      , c('mes', 'ano') := NULL]
  
  oldNames <- intersect(corresp_micro_pond.dt[['micro']], names(indices_prec.dt))
  newNames <- corresp_micro_pond.dt[micro %in% oldNames][['pond']]
  
  setnames(indices_prec.dt, oldNames, newNames)
  
  index_names <- c( "INDICE0", "IMI0", "IZE0", "IZNE0", "IRM0")
  
  indices_prec_melted.dt <- melt(
    indices_prec.dt, 
    id.vars = corresp_micro_pond.dt[['pond']], 
    measure.vars = index_names, 
    value.name = 'index')[
      , variable := vars_names[variable]]
  
   elemIndices.dt <- computeElementaryIndices(tot_ref = total_running.dt, 
                                             tot_prec = total_prec.dt, 
                                             indices_prec = indices_prec_melted.dt)

   output.list <- list(elemIndices.dt = elemIndices.dt,
                  total_running.dt = total_running.dt)
   
   if (batch != '00'){
     
   elemIndices_rmse.dt <- computeElementaryIndices(tot_ref = total_running_rmse.dt, 
                                              tot_prec = total_prec.dt, 
                                              indices_prec = indices_prec_melted.dt)
   
   setnames(elemIndices_rmse.dt, 'index', 'index_rmse')
   setnames(total_running_rmse.dt, 'total', 'total_rmse')
   
   output.list$elemIndices_rmse.dt <- elemIndices_rmse.dt
   output.list$total_running_rmse.dt <- total_running_rmse.dt
   
   }
  
   
  lapply(output.list, function(i) if(is.data.table(i)) setkeyv(i, c('pondccaa', 'codigo', 'variable')))
  #return(output.list)
  output.dt <- Reduce(function(...) merge(..., all = TRUE), output.list)

  output.dt[, monthyear := refPeriod][, batch := batch]
  
  return(output.dt)
  
}

