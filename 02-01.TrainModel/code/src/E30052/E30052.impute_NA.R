impute_NA <- function(data){
  

  data[
    is.na(mean_cnae2_est),      mean_cnae2_est      := mean_cnae1_est][
    is.na(mean_cnae2_est_ccaa), mean_cnae2_est_ccaa := mean_cnae2_est][
    is.na(mean_cnae3_est),      mean_cnae3_est      := mean_cnae2_est][
    is.na(mean_cnae3_est_ccaa), mean_cnae3_est_ccaa := mean_cnae3_est]

  data[
    is.na(std_cnae2_est),      std_cnae2_est      := std_cnae1_est][
    is.na(std_cnae2_est_ccaa), std_cnae2_est_ccaa := std_cnae2_est][
    is.na(std_cnae3_est),      std_cnae3_est      := std_cnae2_est][
    is.na(std_cnae3_est_ccaa), std_cnae3_est_ccaa := std_cnae3_est]

  # Tasas previas
  
  na_categories       <- c('', 'CNAE3_est', 'actiFF_1', 'CNAE2div_emp_ccaa', 'estrato', 'CNAE3_est_ccaa', 'actiFF_1_ccaa')
  imputing_categories <- c('actiFF_1', 'CNAE2_est', 'CNAE3_est', 'CNAE2_est', 'CNAE2_est', 'CNAE3_est', 'actiFF_1')
  
  
  for (i in seq(2, 13)){
    
    impute_NA_rates(data, 'cn01', 1, i, na_suffix = na_categories[1], imputing_suffix = imputing_categories[1])
    impute_NA_rates(data, 'cn01', 1, i, na_suffix = na_categories[2], imputing_suffix = imputing_categories[2])
    impute_NA_rates(data, 'cn01', 1, i, na_suffix = na_categories[3], imputing_suffix = imputing_categories[3])
    impute_NA_rates(data, 'cn01', 1, i, na_suffix = na_categories[4], imputing_suffix = imputing_categories[4])
    impute_NA_rates(data, 'cn01', 1, i, na_suffix = na_categories[5], imputing_suffix = imputing_categories[5])
    impute_NA_rates(data, 'cn01', 1, i, na_suffix = na_categories[6], imputing_suffix = imputing_categories[6])
    impute_NA_rates(data, 'cn01', 1, i, na_suffix = na_categories[7], imputing_suffix = imputing_categories[7])
    
    j <- i-1
    
    impute_NA_rates(data, 'cn01', 0, j, na_suffix = na_categories[1], imputing_suffix = imputing_categories[1])
    impute_NA_rates(data, 'cn01', 0, j, na_suffix = na_categories[2], imputing_suffix = imputing_categories[2])
    impute_NA_rates(data, 'cn01', 0, j, na_suffix = na_categories[3], imputing_suffix = imputing_categories[3])
    impute_NA_rates(data, 'cn01', 0, j, na_suffix = na_categories[4], imputing_suffix = imputing_categories[4])
    impute_NA_rates(data, 'cn01', 0, j, na_suffix = na_categories[5], imputing_suffix = imputing_categories[5])
    impute_NA_rates(data, 'cn01', 0, j, na_suffix = na_categories[6], imputing_suffix = imputing_categories[6])
    impute_NA_rates(data, 'cn01', 0, j, na_suffix = na_categories[7], imputing_suffix = imputing_categories[7])
    
  }

  ### Coeficientes de variación
  ## Puede ser 0 porque MA y std sean 0 al mismo tiempo o porque ambas sean NAs
  
  data[
    is.na(cv_est_3),  cv_est_3  := 0.0][
    is.na(cv_est_6),  cv_est_6  := 0.0][
    is.na(cv_est_12), cv_est_12 := 0.0] 

  ##### Corregimos NAs de monthyearos anteriores (Los que tienen NA en cn01_1 se eliminan de la muestra)
  
  
  data[
    is.na(cn01_2),  cn01_2  := cn01_1 * (1 - tasa_cn01_actiFF_1_2_1)][
    is.na(cn01_3),  cn01_3  := cn01_1 * (1 - tasa_cn01_actiFF_1_3_1)][
    is.na(cn01_4),  cn01_4  := cn01_1 * (1 - tasa_cn01_actiFF_1_4_1)][
    is.na(cn01_5),  cn01_5  := cn01_1 * (1 - tasa_cn01_actiFF_1_5_1)][
    is.na(cn01_6),  cn01_6  := cn01_1 * (1 - tasa_cn01_actiFF_1_6_1)][
    is.na(cn01_7),  cn01_7  := cn01_1 * (1 - tasa_cn01_actiFF_1_7_1)][
    is.na(cn01_8),  cn01_8  := cn01_1 * (1 - tasa_cn01_actiFF_1_8_1)][
    is.na(cn01_9),  cn01_9  := cn01_1 * (1 - tasa_cn01_actiFF_1_9_1)][
    is.na(cn01_10), cn01_10 := cn01_1 * (1 - tasa_cn01_actiFF_1_10_1)][
    is.na(cn01_11), cn01_11 := cn01_1 * (1 - tasa_cn01_actiFF_1_11_1)][
    is.na(cn01_12), cn01_12 := cn01_1 * (1 - tasa_cn01_actiFF_1_12_1)][
    is.na(cn01_13), cn01_13 := cn01_1 * (1 - tasa_cn01_actiFF_1_13_1)]
  
  ## NAs IPI
  
  data[
    is.na(it_4ca),   it_4ca   := it_4][
    is.na(tm_4ca),   tm_4ca   := tm_4][
    is.na(ta_4ca),   ta_4ca   := ta_4][
    is.na(tl_4ca),   tl_4ca   := tl_4][
    is.na(tm12_4ca), tm12_4ca := tm12_4][
    is.na(it_3ca),   it_3ca   := it_3][
    is.na(tm_3ca),   tm_3ca   := tm_3][
    is.na(ta_3ca),   ta_3ca   := ta_3][
    is.na(tl_3ca),   tl_3ca   := tl_3][
    is.na(tm12_3ca), tm12_3ca := tm12_3]
  
  ## NAs IPRI
  
  data[
    is.na(ipri4_ca), ipri4_ca := ipri4][
    is.na(ipri3_ca), ipri4_ca := ipri3]


  data[
    is.na(quantile_MA12subdiv_3),    quantile_MA12subdiv_3    := quantile_MA12subdiv_1][
    is.na(quantile_MA12div_ccaa_3),  quantile_MA12div_ccaa_3  := quantile_MA12div_ccaa_1][
    is.na(quantile_MA12estra_3),     quantile_MA12estra_3     := quantile_MA12div_ccaa_3][
    is.na(quantile_MA12subdiv_6),    quantile_MA12subdiv_6    := quantile_MA12subdiv_3][
    is.na(quantile_MA12div_ccaa_6),  quantile_MA12div_ccaa_6  := quantile_MA12div_ccaa_3][
    is.na(quantile_MA12estra_6),     quantile_MA12estra_6     := quantile_MA12div_ccaa_6][
    is.na(quantile_MA12subdiv_12),   quantile_MA12subdiv_12   := quantile_MA12subdiv_6][
    is.na(quantile_MA12div_ccaa_12), quantile_MA12div_ccaa_12 := quantile_MA12div_ccaa_6][
    is.na(quantile_MA12estra_12),    quantile_MA12estra_12    := quantile_MA12div_ccaa_12]
  
  ## Limpiamos los NAs de actiFD
  data[is.na(actiFD), actiFD := actiFF_1]
  

  #### Las numericas que quedan les hacemos fill na 0

  for (col in setdiff(names(which(sapply(data, is.numeric))), 'cn01')){
     
    data[is.na(get(col)), (col) := 0]
  
  }
  
  # En las categoricas se les crea una categoría correspondiente a NA
  for (col in names(which(sapply(data, is.character)))){
    
    data[is.na(get(col)), (col) := '*']
    
  }

  return(data)
  
}

