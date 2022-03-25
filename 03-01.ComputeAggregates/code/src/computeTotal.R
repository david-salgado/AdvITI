computeTotal <- function(data.StQ, sample.dt, varNames, cellNames, samplingWNames = NULL){
  
  # Calcular el total de cada variable en varNames para cada celda
  # en cellNames utilizando solo las unidades estadísticas de sample
  # tomando data como base de datos
  
  idVar <- names(sample.dt)
  idVar_repo <- unname(UnitToIDDDNames(idVar, getDD(data.StQ)))
  sample <- copy(sample.dt)
  setnames(sample, idVar_repo)
  data.StQ <- data.StQ[sample, on = idVar_repo]
  data.dt <- dcast_StQ(data.StQ, UnitNames = TRUE)[
    , c(idVar, varNames, cellNames, samplingWNames), with = FALSE]
  
  aggreg.dt <- melt(
    data.dt, id.vars = c(idVar, cellNames), measure.vars = varNames)[
      , list(total = sum(value)), by = c(cellNames, 'variable')]
  
  return(aggreg.dt)
  
}