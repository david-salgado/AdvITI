build_FD_longitudinal_regressors <- function(FD_current.dt, thresholds){

  #### Nuevas Variables CNAE y MIG                                          ####
  ##### CNAE3 (grupo) establecimiento y  CNAE3 (grupo) empresa
  FD_current.dt[, c("CNAE3_est","CNAE3_emp") := list(substr(actiFF_1, 1, 3), substr(CodCNAE, 1, 3))]
  
  ##### CNAE2 (division/subdivi) establecimiento
  FD_current.dt[, CNAE2_est := CNAE3toCNAESub(CNAE3_est)]  
  
  ##### CNAE1 (seccion) establecimiento
  FD_current.dt[, CNAE1_est := CNAE2toCNAE1(Code3toCode2(CNAE3_est))]  
  
  ##### CNAE2 (division/subdiv) empresa 
  FD_current.dt[, CNAE2_emp := CNAE3toCNAESub(CNAE3_emp)]  
    
  ##### CNAE2 (division) empresa 
  FD_current.dt[, CNAE2div_emp := Code3toCode2(CNAE3_emp)]  
  
  #####  CNAE1 (seccion) empresa
  FD_current.dt[, CNAE1_emp := CNAE2toCNAE1(Code3toCode2(CNAE3_emp))]  
  
  ##### MIGs establecimiento
  FD_current.dt[, MIG_est := CNAE3toCNAEMIG(CNAE3_est)]  
  
  ##### MIGs empresa
  FD_current.dt[, MIG_emp := CNAE3toCNAEMIG(CNAE3_emp)] 
  
  ##### Match CNAE4,CNAE3,CNAE2,CNAE1 (se comprueba si el del establecimiento coincide con el de la empresa)
  FD_current.dt[, c('Match_CNAE4','Match_CNAE3','Match_CNAE2','Match_CNAE1') := 
                  list((CodCNAE   == actiFF_1), 
                       (CNAE3_est == CNAE3_emp), 
                       (CNAE2_est == CNAE2_emp),
                       (CNAE1_est == CNAE1_emp))] 
  
  ##### CCAA empresa
  FD_current.dt[, ccaa_emp := provinciaToCCAA(Codprovincia)]
  
  ##### estrato
  FD_current.dt[, estrato := paste0(ccaa, CNAE2_est)] 

  
  #### Medias móviles por establecimiento                                   ####
  
  ##### MA(12), MA(6), MA(3)
  FD_current.dt[
    , MA_est_3  := rowMeans(.SD, na.rm = TRUE), .SDcols = paste0('cn01_', 1:3)][
    , MA_est_6  := rowMeans(.SD, na.rm = TRUE), .SDcols = paste0('cn01_', 1:6)][
    , MA_est_12 := rowMeans(.SD, na.rm = TRUE), .SDcols = paste0('cn01_', 1:12)]
  
  
  #### Quantiles y thresholds                                               ####
  ####### Threshold del establecimiento en la division/subdivi
  FD_current.dt[
    , quantile_MA12subdiv_1  := ecdf(MA_est_12)(cn01_1),  by = .(CNAE2_est)][
    , quantile_MA12subdiv_3  := ecdf(MA_est_12)(cn01_3),  by = .(CNAE2_est)][
    , quantile_MA12subdiv_6  := ecdf(MA_est_12)(cn01_6),  by = .(CNAE2_est)][
    , quantile_MA12subdiv_12 := ecdf(MA_est_12)(cn01_12), by = .(CNAE2_est)]
  
  ##### Threshold95 de la media movil del establecimiento en la division/subdivi
  FD_current.dt[
    , q95_ma3           := .(quantile(MA_est_3, probs = c(0.95), na.rm = T)), by = .(CNAE2_est)][
    , thresholds95_MA3  := (MA_est_3 >= q95_ma3)][
    , q95_ma6           := .(quantile(MA_est_6, probs = c(0.95), na.rm = T)), by = .(CNAE2_est)][
    , thresholds95_MA6  := (MA_est_6 >= q95_ma6)][
    , q95_ma12          := .(quantile(MA_est_12, probs = c(0.95), na.rm = T)), by = .(CNAE2_est)][
    , thresholds95_MA12 := (MA_est_12 >= q95_ma12)]
  
  
  ####### Threshold del establecimiento en la division/subdivi y CCAA
  
  # Se crea la variable pNA que es la proporción de valores NA en el subgrupo by
  FD_current.dt[, pNA := (sum(is.na(MA_est_12))/length(MA_est_12)), by = .(CNAE2div_emp, ccaa)]
  
  FD_current.dt[
    pNA  < 1, quantile_MA12div_ccaa_1 := ecdf(MA_est_12)(cn01_1), by = .(CNAE2div_emp, ccaa)][
    pNA == 1, quantile_MA12div_ccaa_1 := quantile_MA12subdiv_1][
    pNA  < 1, quantile_MA12div_ccaa_3 := ecdf(MA_est_12)(cn01_3), by = .(CNAE2div_emp, ccaa)][
    pNA == 1, quantile_MA12div_ccaa_3 := quantile_MA12subdiv_3][
    pNA  < 1, quantile_MA12div_ccaa_6 := ecdf(MA_est_12)(cn01_6), by = .(CNAE2div_emp, ccaa)][
    pNA == 1, quantile_MA12div_ccaa_6 := quantile_MA12subdiv_6][
    pNA  < 1, quantile_MA12div_ccaa_12 := ecdf(MA_est_12)(cn01_12), by = .(CNAE2div_emp, ccaa)][
    pNA == 1, quantile_MA12div_ccaa_12 := quantile_MA12subdiv_12]
  

  ##### Threshold95 de la media movil del establecimiento en la divisi?n/subdivi y CCAA????
  FD_current.dt[
    , q95_ma3_ccaa := .(quantile(MA_est_3, probs = c(0.95), na.rm = T)), by = .(CNAE2div_emp, ccaa)][
    , thresholds95_MA3_ccaa := (MA_est_3 >= q95_ma3_ccaa)][
    , q95_ma6_ccaa := .(quantile(MA_est_6, probs = c(0.95), na.rm = T)), by = .(CNAE2div_emp, ccaa)][
    , thresholds95_MA6_ccaa := (MA_est_6 >= q95_ma6_ccaa)][
    , q95_ma12_ccaa := .(quantile(MA_est_12, probs = c(0.95), na.rm = T)), by = .(CNAE2div_emp,ccaa)][
    , thresholds95_MA12_ccaa := (MA_est_12 >= q95_ma12_ccaa)]
  
  ####### Threshold del establecimiento en el estrato
  
  # Se crea la variable pNA que es la proporción de valores NA en el subgrupo by
  FD_current.dt[, pNA := (sum(is.na(MA_est_12))/length(MA_est_12)), by = .(estrato)]
  
  FD_current.dt[
    pNA  < 1, quantile_MA12estra_1 := ecdf(MA_est_12)(cn01_1), by = .(estrato)][
    pNA  < 1, quantile_MA12estra_3 := ecdf(MA_est_12)(cn01_3), by = .(estrato)][
    pNA  < 1, quantile_MA12estra_6 := ecdf(MA_est_12)(cn01_6), by = .(estrato)][
    pNA  < 1, quantile_MA12estra_12 := ecdf(MA_est_12)(cn01_12), by = .(estrato)][
    pNA == 1, quantile_MA12estra_1 := quantile_MA12subdiv_1, by = .(estrato)][
    pNA == 1, quantile_MA12estra_3 := quantile_MA12subdiv_3, by = .(estrato)][
    pNA == 1, quantile_MA12estra_6 := quantile_MA12subdiv_6, by = .(estrato)][
    pNA == 1, quantile_MA12estra_12 := quantile_MA12subdiv_12, by = .(estrato)]
  
  
  ##### Threshold95 de la media movil del estrato
  FD_current.dt[
    , q95_ma3_estrato := .(quantile(MA_est_3, probs = c(0.95), na.rm = T)), by = .(estrato)][
    , thresholds95_MA3_estrato := (MA_est_3 >= q95_ma3_estrato)][
    , q95_ma6_estrato := .(quantile(MA_est_6, probs = c(0.95), na.rm = T)), by = .(estrato)][
    , thresholds95_MA6_estrato := (MA_est_6 >= q95_ma6_estrato)][
    , q95_ma12_estrato := .(quantile(MA_est_12, probs = c(0.95), na.rm = T)), by = .(estrato)][
    , thresholds95_MA12_estrato := (MA_est_12 >= q95_ma12_estrato)]
  
  
  ##### Tasa anual, Tasa mensual, trimestral y semestral
  
  #### Tasas para cada establecimiento y agrupadas                                   ####
  
  for (i in seq(2, 13)){
    
    FD_current.dt <- compute_rates(dt = FD_current.dt, varName = 'cn01', idName = 'norden', lag_initial = 1, lag_final = i, by = NULL)
    FD_current.dt <- compute_rates(dt = FD_current.dt, varName = 'cn01', idName = 'norden', lag_initial = 1, lag_final = i, by = c('estrato'))
    FD_current.dt <- compute_rates(dt = FD_current.dt, varName = 'cn01', idName = 'norden', lag_initial = 1, lag_final = i, by = c('CNAE2_est'))
    FD_current.dt <- compute_rates(dt = FD_current.dt, varName = 'cn01', idName = 'norden', lag_initial = 1, lag_final = i, by = c('CNAE3_est'))
    FD_current.dt <- compute_rates(dt = FD_current.dt, varName = 'cn01', idName = 'norden', lag_initial = 1, lag_final = i, by = c('actiFF_1'))
    FD_current.dt <- compute_rates(dt = FD_current.dt, varName = 'cn01', idName = 'norden', lag_initial = 1, lag_final = i, by = c('CNAE2div_emp', 'ccaa'))
    FD_current.dt <- compute_rates(dt = FD_current.dt, varName = 'cn01', idName = 'norden', lag_initial = 1, lag_final = i, by = c('CNAE3_est', 'ccaa'))
    FD_current.dt <- compute_rates(dt = FD_current.dt, varName = 'cn01', idName = 'norden', lag_initial = 1, lag_final = i, by = c('actiFF_1', 'ccaa'))
    
  }  
  

  ##### Variables para detectar empresas muy volatiles en facturacion

  FD_current.dt[
    , cv_est_3 := ifelse(MA_est_3==0, 0.0, sd(.SD, na.rm = TRUE)/MA_est_3), .SDcols = paste0('cn01_', 1:3), by = 'norden'][
    , cv_est_6 := ifelse(MA_est_6==0, 0.0, sd(.SD, na.rm = TRUE)/MA_est_6), .SDcols = paste0('cn01_', 1:6), by = 'norden'][
    , cv_est_12 := ifelse(MA_est_12==0,0.0, sd(.SD, na.rm = TRUE)/MA_est_12), .SDcols = paste0('cn01_', 1:12), by = 'norden']
  
  FD_current.dt[
    , min_est_3 := min(.SD, na.rm = TRUE), .SDcols = paste0('cn01_', 1:3)][
    , min_est_6 := min(.SD, na.rm = TRUE), .SDcols = paste0('cn01_', 1:6)][
    , min_est_12 := min(.SD, na.rm = TRUE), .SDcols = paste0('cn01_', 1:12)]
  
  FD_current.dt[
    , max_est_3 := max(.SD, na.rm = TRUE), .SDcols = paste0('cn01_', 1:3)][
    , max_est_6 := max(.SD, na.rm = TRUE), .SDcols = paste0('cn01_', 1:6)][
    , max_est_12 := max(.SD, na.rm = TRUE), .SDcols = paste0('cn01_', 1:12)]
  
  #### Contadores meses previos                                             ####
  ##### Buscamos casos raros en los que cn01v != 1
  ## Se marca cuantas veces ha sido imputada manualmente en los últimos 3, 6, 12 meses
  # cn01a == 7 imputación manual
  # cn01a == 8 imputación automática
  FD_current.dt[
    , is_imp_v1 := (cn01a_1 %in% c('7'))][
    , is_imp_v2 := (cn01a_2 %in% c('7'))][
    , is_imp_v3 := (cn01a_3 %in% c('7'))][
    , is_imp_v4 := (cn01a_4 %in% c('7'))][
    , is_imp_v5 := (cn01a_5 %in% c('7'))][
    , is_imp_v6 := (cn01a_6 %in% c('7'))][
    , is_imp_v7 := (cn01a_7 %in% c('7'))][
    , is_imp_v8 := (cn01a_8 %in% c('7'))][
    , is_imp_v9 := (cn01a_9 %in% c('7'))][
    , is_imp_v10 := (cn01a_10 %in% c('7'))][
    , is_imp_v11 := (cn01a_11 %in% c('7'))][
    , is_imp_v12 := (cn01a_12 %in% c('7'))]
  
  FD_current.dt[
    , meses_imputada_experto_3  := rowSums(.SD, na.rm = TRUE), .SDcols = c('is_imp_v1', 'is_imp_v2', 'is_imp_v3')][
    , meses_imputada_experto_6  := rowSums(.SD, na.rm = TRUE), .SDcols = paste0('is_imp_v', 1:6)][
    , meses_imputada_experto_12 := rowSums(.SD, na.rm = TRUE), .SDcols = paste0('is_imp_v', 1:12)]
  
  remove_is_imp <- c(paste0('is_imp_v', 1:12))
  FD_current.dt <- FD_current.dt[, (remove_is_imp) := NULL]
  
  ## Se marca cuantas veces ha sido imputada automaticamente en los últimos 3, 6, 12 meses
  FD_current.dt[
    , is_imp_v1 := (cn01a_1 %in% c('8'))][
    , is_imp_v2 := (cn01a_2 %in% c('8'))][
    , is_imp_v3 := (cn01a_3 %in% c('8'))][
    , is_imp_v4 := (cn01a_4 %in% c('8'))][
    , is_imp_v5 := (cn01a_5 %in% c('8'))][
    , is_imp_v6 := (cn01a_6 %in% c('8'))][
    , is_imp_v7 := (cn01a_7 %in% c('8'))][
    , is_imp_v8 := (cn01a_8 %in% c('8'))][
    , is_imp_v9 := (cn01a_9 %in% c('8'))][
    , is_imp_v10 := (cn01a_10 %in% c('8'))][
    , is_imp_v11 := (cn01a_11 %in% c('8'))][
    , is_imp_v12 := (cn01a_12 %in% c('8'))]
  
  FD_current.dt[
    , meses_imputada_auto_3  := rowSums(.SD, na.rm = TRUE), .SDcols = c('is_imp_v1', 'is_imp_v2', 'is_imp_v3')][
    , meses_imputada_auto_6  := rowSums(.SD, na.rm = TRUE), .SDcols = paste0('is_imp_v', 1:6)][
    , meses_imputada_auto_12 := rowSums(.SD, na.rm = TRUE), .SDcols = paste0('is_imp_v', 1:12)]
  
  
  remove_is_imp <- c(paste0('is_imp_v', 1:12))
  FD_current.dt <- FD_current.dt[, (remove_is_imp) := NULL]

  
  ## Se marca cuantas veces han facturado menos de thre en los últimos 3, 6, 12 meses
  thre <- thresholds[1]
  
  FD_current.dt[
    , is_correct_v1 := (cn01_1 <= thre)][
    , is_correct_v2 := (cn01_2 <= thre)][
    , is_correct_v3 := (cn01_3 <= thre)][
    , is_correct_v4 := (cn01_4 <= thre)][
    , is_correct_v5 := (cn01_5 <= thre)][
    , is_correct_v6 := (cn01_6 <= thre)][
    , is_correct_v7 := (cn01_7 <= thre)][
    , is_correct_v8 := (cn01_8 <= thre)][
    , is_correct_v9 := (cn01_9 <= thre)][
    , is_correct_v10 := (cn01_10 <= thre)][
    , is_correct_v11 := (cn01_11 <= thre)][
    , is_correct_v12 := (cn01_12 <= thre)]
  
  FD_current.dt[
    , meses_facturando3 := rowSums(.SD, na.rm = TRUE), .SDcols = paste0('is_correct_v', 1:3)][
    , meses_facturando6 := rowSums(.SD, na.rm = TRUE), .SDcols = paste0('is_correct_v', 1:6)][
    , meses_facturando12 := rowSums(.SD, na.rm = TRUE), .SDcols = paste0('is_correct_v', 1:12)]
  
  remove_is_correct<- c(paste0('is_correct_v', 1:12))
  FD_current.dt <- FD_current.dt[, (remove_is_correct) := NULL]
  
  ### Numero de veces que la fila ha sido NA en los ultimos 12 meses
  FD_current.dt[
    , is_na_v2 := is.na(cn01_2)][
    , is_na_v3 := is.na(cn01_3)][
    , is_na_v4 := is.na(cn01_4)][
    , is_na_v5 := is.na(cn01_5)][
    , is_na_v6 := is.na(cn01_6)][
    , is_na_v7 := is.na(cn01_7)][
    , is_na_v8 := is.na(cn01_8)][
    , is_na_v9 := is.na(cn01_9)][
    , is_na_v10 := is.na(cn01_10)][
    , is_na_v11 := is.na(cn01_11)][
    , is_na_v12 := is.na(cn01_12)]
  
  FD_current.dt[, meses_NAs := rowSums(.SD, na.rm = TRUE), .SDcols = paste0('is_na_v', 2:12)]
  
  remove_is_na<- c(paste0('is_na_v', 2:12))
  FD_current.dt <- FD_current.dt[, (remove_is_na) := NULL]
  
  #### True CN01FF   (se crea cn01FF con la info del FF de refPeriod)###
  
  # ##### CAMBIAMOS LOS TIPOS DE CHAR A FACTOR
  # cols <- names(Filter(is.character, FD_current.dt))
  # FD_current.dt[, (cols) := lapply(.SD, as.factor), .SDcols = cols] 

  
  ## eliminamos cn01a_1,....cn01a_13 y cn01v_1,....,cn01v_13
  cols_remove   <- c(paste0("cn01a_", 1:13), paste0("cn01v_", 1:13))
  FD_current.dt <- FD_current.dt[, (cols_remove) := NULL]
  
  return(FD_current.dt)
  
}

