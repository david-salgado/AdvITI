computeComposedIndices <- function(elemIndices, weights, cells){
  
  # Aplicar el sistema de ponderaciones para calcular los índices compuestos
  # a partir de los índices elementales
  
  compIndices.dt <- merge(elemIndices, weights, by = intersect(names(elemIndices), names(weights)))
  
  compIndices.dt.lst <- vector('list', length(cells))
  names(compIndices.dt.lst) <- names(cells)
  for (cell in names(cells)) {
    
    cell_by <- cells[[cell]]
    newNames <- setdiff(names(cell_by), names(compIndices.dt))
    
    if (length(newNames) == 0) newNames <- names(cell_by)
    if (all(names(cell_by) %in% names(compIndices.dt))) {
      
      tempDT <- compIndices.dt[
        , list(index = sum(pond * index) / sum(pond)), by = newNames][
          , index := round(index, 4)]
      setkeyv(tempDT, newNames)  
      compIndices.dt.lst[[cell]] <- tempDT
      
    } else {
      
      commonNames <- intersect(names(compIndices.dt), names(cell_by))
      if (length(commonNames) == 0) return(list(error = "ERROR: no se puede calcular el índice por falta de variable común para la correspondencia.", 
                                                compIndices.dt = compIndices.dt, 
                                                cell_by = cell_by))

      compIndices.dt <- merge(compIndices.dt, cell_by, by = commonNames)
      tempDT <- compIndices.dt[
        , list(index = sum(pond * index) / sum(pond)), by = newNames][
          , index := round(index, 4)]
      setkeyv(tempDT, newNames) 
      compIndices.dt.lst[[cell]] <- tempDT
      
    }
  
  }
  return(compIndices.dt.lst)
}