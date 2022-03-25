
setup_FF <- function(FF.StQ.list){
  
  GRUPO <- new(Class = 'Variable',
               UnitName = 'GRUPO',
               IDDD = 'ActivEcono',
               QualsValues = list(NOrden = '', TipoMicrodato = '13.', TareaProceso = '6.', RolProceso = '6.', Clasificacion = '2.6.4.1.', EsModif ='0'),
               Length = '3',
               ClassVar = 'character',
               ValueRegExp = '[.]+',
               Formula = as.call(list('actiToSubdivi(ActivEcono_35._6._2.1.2._2.1.4._)')),
               SlotName = 'MicroData',
               Literal = '',
               DDversion = '1')
  
  FF.dt.list <- vector("list", length = length(FF.StQ.list))
  
  for(i in seq_along(FF.StQ.list)){
    
    FF.StQ <- FF.StQ.list[[i]]  
    
    FF.StQ <- setVariable(FF.StQ, GRUPO)
    FF.StQ <- FF.StQ[NOrden != '']
    FF.dt <- dcast_StQ(FF.StQ, UnitNames = TRUE)
    
    FFName <- names(FF.StQ.list)[i]
    mmaaaa <- stringr::str_match(FFName, ".MM\\s*(.*?)\\s*.D_")[2]
    
    FF.dt[
      , period := paste0("MM", mmaaaa)][
      , month := substr(mmaaaa, 1, 2)][
      , year := substr(mmaaaa, 3, 6)]
    
    FF.dt.list[[i]] <- FF.dt
    
  }
  
  
  FF.dt <- rbindlist(FF.dt.list)[
    !is.na(numidest)][
    cn01a == 1 & cn01 == 4, cn01 := 0][
    cn02a == 1 & cn02 == 1, cn02 := 0][
    cn03a == 1 & cn03 == 1, cn03 := 0][
    cn04a == 1 & cn04 == 1, cn04 := 0][
    cn05a == 1 & cn05 == 1, cn05 := 0]

  varsToRemove <- intersect(names(FF.dt), c("pondccaa", "codigo", "NombreEdit", "PONMI", "PONZE", "PONZNE", "PONRM"))
  if (length(varsToRemove) > 0) FF.dt[, (varsToRemove) := NULL]
  
  return(FF.dt)
  
}