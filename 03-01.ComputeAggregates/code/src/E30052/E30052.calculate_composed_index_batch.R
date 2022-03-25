
calculate_composed_index_batch <- function(all_elemental_index, 
                                                FF_previous.StQ,
                                                collected_var, refPeriod, batch){
  
  vars_names <- collected_var
  
  #####                          CALCULATE COMPOSITE INDEX                           #### 
  ##### :::::::::::::   #####
  #### indice compuesto  #####
  
  elemIndices.dt <- copy(all_elemental_index)[, c('pondccaa', 'codigo', 'variable', 'index')]
  
  FF_ref.dt <- dcast_StQ(FF_previous.StQ, UnitNames = TRUE)
  FF_ref.dt <- FF_ref.dt[pondccaa != '']
  FF_ref.dt <- FF_ref.dt[, list(codigo, pondccaa, PONMI, PONZE, PONZNE, PONRM)]
  
  pond_fund.dt <- melt(FF_ref.dt, 
                       id.vars = c('pondccaa', 'codigo'), 
                       variable.name = c('variable'), 
                       value.name = 'pond')
  
  pond_fund_names <- c('PONMI', 'PONZE', 'PONZNE', 'PONRM')
  names(pond_fund_names) <- c('cn02', 'cn03', 'cn04', 'cn05')
  
  #como calcular cn01 con el resto de cn02..
  pond_fund.dt[, variable := names(pond_fund_names)[variable]]
  
  pond_fund.dt <- pond_fund.dt[, lapply(.SD, sum), by = .(pondccaa, codigo), .SDcols = c('pond')]
  pond_fund.dt <- pond_fund.dt[, variable := 'cn01']
  
  
  codigo_pond_fund_comp <- c('10', '13', '20', '25', '26', '27', '30', '32')
  
  pond_elem_ccaa_codigo.dt <- pond_fund.dt[
    !codigo %chin% codigo_pond_fund_comp][
      , variable := 'cn01'][
        , variable := NULL]
  
  setcolorder(pond_elem_ccaa_codigo.dt, c('pondccaa', 'codigo', 'pond'))
  
  pond_positiva.dt <- pond_elem_ccaa_codigo.dt[pond > 0, .(pondccaa, codigo)]
  
  elemIndices.dt <- merge(elemIndices.dt, 
                          pond_positiva.dt, 
                          by = c('pondccaa', 'codigo'), 
                          all.y = TRUE)
  
  temp.list <- vector('list', length(vars_names))
  names(temp.list) <- vars_names
  
  for (var in vars_names){
    
    temp.list[[var]] <- elemIndices.dt[
      is.na(index)][
        , c('variable', 'index') := NULL][
          , variable := var]
    
  }
  
  pond_positiva.dt <- rbindlist(temp.list)[
    , index := 1e-10]
  elemIndices.dt <- elemIndices.dt[!is.na(variable)]
  elemIndices.dt <- rbindlist(list(elemIndices.dt, pond_positiva.dt))[
    order(pondccaa, codigo)]
  
  
  
  
  ICN_dissemPlan <- list(
    ccaa       = data.table(ccaa = str_pad(1:17, 2, 'left', '0')),
    div_subdiv = data.table(codigo = c("05",  "06",  "07",  "08",  "10A", "10B", "11",  "12", "13A", "13B", "14",  "15",  "16",
                                       "17",  "18",  "19" , "20A", "20B", "21",  "22",  "23", "24",  "25A", "25B", "26A", "26B",
                                       "26C", "27A", "27B", "28",  "29",  "30A", "30B", "31", "32A", "32B", "32C", "33")),
    division   = data.table(codigo = c("05",  "06",  "07",  "08",  "10A", "10B", "11",  "12", "13A", "13B", "14",  "15",  "16",
                                       "17",  "18",  "19" , "20A", "20B", "21",  "22",  "23", "24",  "25A", "25B", "26A", "26B",
                                       "26C", "27A", "27B", "28",  "29",  "30A", "30B", "31", "32A", "32B", "32C", "33"),
                            division = c("05", "06", "07", "08", "10", "10", "11", "12", "13", "13", "14", "15", "16",
                                         "17", "18", "19" , "20", "20", "21", "22", "23", "24", "25", "25", "26", "26",
                                         "26", "27", "27", "28", "29", "30", "30", "31", "32", "32", "32", "33")),
    seccion     = data.table(division = c("05", "06", "07", "08", "10", "11", "12", "13", "14", "15", "16",
                                          "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27",
                                          "28", "29", "30", "31", "32", "33"),
                             seccion  = c(rep('B', 4), rep('C', 24))),
    general    = data.table(seccion = c('B', 'C'), general = c('general', 'general')),
    MIG        = data.table(codigo = c("05",  "06",  "07",  "08",  "10A", "10B", "11",  "12", "13A", "13B", "14",  "15",  "16",
                                       "17",  "18",  "19" , "20A", "20B", "21",  "22",  "23", "24",  "25A", "25B", "26A", "26B",
                                       "26C", "27A", "27B", "28",  "29",  "30A", "30B", "31", "32A", "32B", "32C", "33"),
                            MIG    = c('EN', 'EN', 'BI', 'BI', 'CN',  'BI', 'CN', 'CN',  'BI',  'CN', 'CN', 'CN', 'BI',
                                       'BI',  'CN',  'EN', 'CN', 'BI',  'CN',  'BI', 'BI', 'BI', 'BC', 'BI', 'BI', 'BC',
                                       'CD', 'CD', 'BI', 'BC', 'BC', 'BC', 'CD', 'CD', 'CD', 'CN', 'BC', 'BC')),
    MIG2       = data.table(MIG = c('EN', 'BI', 'CD', 'CN', 'BC'), MIG2 = c('EN', 'BI', 'XC', 'XC', 'BC')),
    Total      = data.table(MIG2 = c('EN', 'BI', 'XC', 'BC'), total = c('total', 'total', 'total', 'total')))
  
  
  setnames(elemIndices.dt, 'pondccaa', 'ccaa')

  setnames(pond_elem_ccaa_codigo.dt, 'pondccaa', 'ccaa')
  
  
  merge_elem_pond.list <- computeComposedIndices(elemIndices.dt, 
                                                 weights = pond_elem_ccaa_codigo.dt, 
                                                 cells = ICN_dissemPlan)

  merge_elem_pond.list <- lapply(merge_elem_pond.list, function(x, batch, refPeriod){
            
    x <- x[, batch := batch][, refPeriod := refPeriod]
    
    return(x)
    }, batch, refPeriod)
  
  if (batch != '00'){
  
    elemIndices_rmse.dt <- copy(all_elemental_index)[, c('pondccaa', 'codigo', 'variable', 'index_rmse')]
    setnames(elemIndices_rmse.dt, 'index_rmse', 'index')
    
    pond_elem_ccaa_codigo.dt <- pond_fund.dt[
      !codigo %chin% codigo_pond_fund_comp][
        , variable := 'cn01'][
          , variable := NULL]
    setcolorder(pond_elem_ccaa_codigo.dt, c('pondccaa', 'codigo', 'pond'))
    
    pond_positiva.dt <- pond_elem_ccaa_codigo.dt[pond > 0, .(pondccaa, codigo)]
    
    elemIndices_rmse.dt <- merge(elemIndices_rmse.dt, pond_positiva.dt, by = c('pondccaa', 'codigo'), all.y = TRUE)
    
    temp.list <- vector('list', length(vars_names))
    names(temp.list) <- vars_names
    for (var in vars_names){
      
      temp.list[[var]] <- elemIndices_rmse.dt[
        is.na(index)][
          , c('variable', 'index') := NULL][
            , variable := var]
      
    }
    pond_positiva.dt <- rbindlist(temp.list)[
      , index := 1e-10]
    
    elemIndices_rmse.dt <- elemIndices_rmse.dt[!is.na(variable)]
    elemIndices_rmse.dt <- rbindlist(list(elemIndices_rmse.dt, pond_positiva.dt))[
      order(pondccaa, codigo)]
    
    
    setnames(elemIndices_rmse.dt, 'pondccaa', 'ccaa')
    setnames(pond_elem_ccaa_codigo.dt, 'pondccaa', 'ccaa')
    
    merge_elem_rmse_pond.list <- computeComposedIndices(elemIndices_rmse.dt, 
                                                      weights = pond_elem_ccaa_codigo.dt, 
                                                      cells = ICN_dissemPlan)
    
    merge_elem_rmse_pond.list <- lapply(merge_elem_rmse_pond.list, function(x, batch){
      
      x <- x[, batch := batch][, refPeriod := refPeriod]
      setnames(x = x, old = 'index', new = 'index_rmse')
      
      return(x)
    }, batch)



  
  
  div_subdiv.dt <- merge(merge_elem_pond.list$div_subdiv,
                        merge_elem_rmse_pond.list$div_subdiv, 
                        by = intersect(names(merge_elem_pond.list$div_subdiv),
                                       names(merge_elem_rmse_pond.list$div_subdiv)))[
                                         , agrupacion := "div_subdiv"]

  setnames(div_subdiv.dt, "codigo", "div_subdiv")
  
  ccaa.dt <- merge(merge_elem_pond.list$ccaa,
                   merge_elem_rmse_pond.list$ccaa, 
                   by = intersect(names(merge_elem_pond.list$ccaa),
                                  names(merge_elem_rmse_pond.list$ccaa)))[
                                    , agrupacion := "ccaa"]

  division.dt <- merge(merge_elem_pond.list$division,
                   merge_elem_rmse_pond.list$division, 
                   by = intersect(names(merge_elem_pond.list$division),
                                  names(merge_elem_rmse_pond.list$division)))[
                                    , agrupacion := "division"]
  
  seccion.dt <- merge(merge_elem_pond.list$seccion,
                       merge_elem_rmse_pond.list$seccion, 
                       by = intersect(names(merge_elem_pond.list$seccion),
                                      names(merge_elem_rmse_pond.list$seccion)))[
                                        , agrupacion := "seccion"]
  
  general.dt <- merge(merge_elem_pond.list$general,
                      merge_elem_rmse_pond.list$general, 
                      by = intersect(names(merge_elem_pond.list$general),
                                     names(merge_elem_rmse_pond.list$general)))[
                                       , agrupacion := "general"]
  
  MIG.dt <- merge(merge_elem_pond.list$MIG,
                      merge_elem_rmse_pond.list$MIG, 
                      by = intersect(names(merge_elem_pond.list$MIG),
                                     names(merge_elem_rmse_pond.list$MIG)))[
                                       , agrupacion := "MIG"]
  
  MIG2.dt <- merge(merge_elem_pond.list$MIG2,
                  merge_elem_rmse_pond.list$MIG2, 
                  by = intersect(names(merge_elem_pond.list$MIG2),
                                 names(merge_elem_rmse_pond.list$MIG2)))[
                                   , agrupacion := "MIG2"]
  
  output <- list(ccaa = ccaa.dt, 
                 div_subdiv = div_subdiv.dt,
                 division = division.dt,
                 seccion = seccion.dt,
                 general = general.dt, 
                 MIG = MIG.dt, 
                 MIG2 = MIG2.dt)
  
  
  }
  if (batch == '00'){
    
  div_subdiv.dt <- data.table(merge_elem_pond.list$div_subdiv)[, agrupacion := "div_subdiv"]
  setnames(div_subdiv.dt, "codigo", "div_subdiv")
  
  output <- list(ccaa = merge_elem_pond.list$ccaa[, agrupacion := "ccaa"],
                 div_subdiv = div_subdiv.dt,
                 division = merge_elem_pond.list$division[, agrupacion := "division"],
                 seccion = merge_elem_pond.list$seccion[, agrupacion := "seccion"],
                 general = merge_elem_pond.list$general[, agrupacion := "general"],
                 MIG = merge_elem_pond.list$MIG[, agrupacion := "MIG"], 
                 MIG2 = merge_elem_pond.list$MIG2[, agrupacion := "MIG2"]) 
  
  }
  
  output <- lapply(output, function(x){
    
    oldName <- setdiff(names(x), c("batch", "refPeriod", "index", "index_rmse", "agrupacion"))
    setnames(x = x, old = oldName, new = 'sub')
    return(x)
  })
  
  output.dt <- rbindlist(output)
  
  return(output.dt)
}
  