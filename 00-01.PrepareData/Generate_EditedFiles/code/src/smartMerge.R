smartMerge <- function(x, y, by = NULL){
  
  # for (col in intersect(by, names(x))){
  #   
  #   x[is.na(get(col)), (col) := '']
  # }
  # 
  # for (col in intersect(by, names(y))){
  #   
  #   y[is.na(get(col)), (col) := '']
  # }
  # 
  units_x <- x[, intersect(by, names(x)), with = F]
  units_y <- y[, intersect(by, names(y)), with = F]
#return(list(units_x, units_y))
  units_xy <- merge(units_x, units_y, by = intersect(names(units_x), names(units_y)), all = TRUE)
#return(list(units_x, units_y, units_xy))
  x_Common <- merge(x, units_xy, by = names(units_xy), all.x = TRUE)
  y_Common <- merge(y, units_xy, by = names(units_xy), all.x = TRUE)
#return(list(x_Common, y_Common))  

  DT_Common <- merge(x_Common, y_Common, all = TRUE, by = names(units_xy))
#return(DT_Common)
  commVars <- setdiff(intersect(names(x_Common), names(y_Common)), names(units_xy))
  for (commVar in commVars) {
    
    DT_Common[, (commVar) := IfElse(is.na(get(paste0(commVar, '.x'))) & !is.na(get(paste0(commVar, '.y'))), get(paste0(commVar, '.y')), NA)]
    DT_Common[, (commVar) := IfElse(!is.na(get(paste0(commVar, '.x'))) & is.na(get(paste0(commVar, '.y'))), get(paste0(commVar, '.x')), get(commVar))]
    DT_Common[, (commVar) := IfElse(!is.na(get(paste0(commVar, '.x'))) & !is.na(get(paste0(commVar, '.y'))), get(paste0(commVar, '.y')), get(commVar))]
    
  }

  allVars <- union(names(x_Common), names(y_Common))
  DT_Common <- DT_Common[, allVars, with = F]

  x_NotCommon <- x[!x_Common, on = names(x_Common)]

  y_NotCommon <- y[!y_Common, on = names(y_Common)]
  DT_NotCommon <- rbindlist(list(x_NotCommon, y_NotCommon), fill = TRUE)

  DT <- rbindlist(list(DT_Common, DT_NotCommon), fill = TRUE)
  return(DT)

}
