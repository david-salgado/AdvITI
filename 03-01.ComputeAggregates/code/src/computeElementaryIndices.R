computeElementaryIndices <- function(tot_ref, tot_prec, indices_prec){
  

  tot_ref <- copy(tot_ref)
  tot_prec <- copy(tot_prec)
  
  setnames(tot_ref, 'total', 'total_running')
  setnames(tot_prec, 'total', 'total_prec')
  temp_data.dt <- merge(tot_ref, tot_prec, 
                        by = intersect(names(tot_ref), names(tot_prec)))[
    , ratio := ifelse(abs(total_prec) > 1e-3, total_running / total_prec, 1e-8)][
    , c('total_running', 'total_prec') := NULL]

  index.dt <- merge(temp_data.dt, indices_prec, 
                        by = intersect(names(temp_data.dt), names(indices_prec)))[
    , index := index * ratio][
    , ratio := NULL]
  
  return(index.dt)
  
}