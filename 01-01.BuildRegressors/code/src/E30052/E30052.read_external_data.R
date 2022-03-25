

read_external_data <- function(paths){
  
  path_ipri <- paths["IPRI"]
  path_ipi  <- paths["IPI"]
  
  ################### LEER IPI ################
  
  ### ipi con CNAE a 4 dígitos cruzado con CCAA
  ipi4ca <- data.table(read_sas(file.path(path_ipi, 'in4ca.sas7bdat')))
  
  setnames(ipi4ca, c('codigo', 'ano', 'mes', 'it', 'tm', 'ta', 'tl', 'tt', 'tm12'),
           c('actiFF_1', 'year', 'month', 'it_4ca', 'tm_4ca', 'ta_4ca', 'tl_4ca', 'tt_4ca', 'tm12_4ca'))
  
  ### ipi con CNAE a 4 dígitos 
  ipi4 <- data.table(read_sas(file.path(path_ipi, 'in4.sas7bdat')))
  
  setnames(ipi4, c('codigo', 'ano', 'mes', 'it', 'tm', 'ta', 'tl', 'tt', 'tm12'),
           c('actiFF_1', 'year', 'month','it_4', 'tm_4', 'ta_4', 'tl_4', 'tt_4', 'tm12_4'))
  
   ### ipi con CNAE a 3 dígitos cruzado con CCAA
  ipi3ca <- data.table(read_sas(file.path(path_ipi, 'in3ca.sas7bdat')))
  
  setnames(ipi3ca, c('codigo', 'ano', 'mes', 'it', 'tm', 'ta', 'tl', 'tt', 'tm12'),
           c('CNAE3_est', 'year', 'month', 'it_3ca', 'tm_3ca', 'ta_3ca', 'tl_3ca', 'tt_3ca', 'tm12_3ca'))
  
  ### ipi con CNAE a 3 dígitos 
  ipi3 <- data.table(read_sas(file.path(path_ipi, 'in3.sas7bdat')))
  
  setnames(ipi3, c('codigo', 'ano', 'mes', 'it', 'tm', 'ta', 'tl', 'tt', 'tm12'),
           c('CNAE3_est', 'year', 'month', 'it_3', 'tm_3', 'ta_3', 'tl_3', 'tt_3', 'tm12_3'))
  
  ## CNAE 2 no hay suficientes niveles en el ipi
  
  ################### LEER IPRI ################
  
  ### ipi con CNAE a 4 dígitos cruzado con CCAA
  ipri <- data.table(read_sas(file.path(path_ipri, 'indicesipri.sas7bdat')))
  
  ipri[, month := str_pad(month(fecha), 2, pad = "0")]
  ipri[, year := as.character(year(fecha))]
  ipri <- ipri[, c('fecha') := NULL]
  
  ipri[, length_rama := nchar(Rama)]
  
  
  ### ipi con CNAE a 4 dígitos cruzado con CCAA
  ipri_4_ca <- ipri[(length_rama == 4) & (ccaa != '00')]
  ipri_4_ca[, length_rama := NULL]
  
  setnames(ipri_4_ca, c('Rama', 'indice_publicable'), c('actiFF_1', 'ipri4_ca'))
  
  ### ipi con CNAE a 4 dígitos 
  ipri_4 <- ipri[(length_rama == 4) & (ccaa == '00')]
  ipri_4[, c('length_rama', 'ccaa') := NULL]
  
  setnames(ipri_4, c('Rama', 'indice_publicable'), c('actiFF_1', 'ipri4'))
  
  ### ipi con CNAE a 3 dígitos cruzado con CCAA
  ipri_3_ca <- ipri[(length_rama == 3) & (ccaa != '00')]
  ipri_3_ca[, length_rama := NULL]
  
  setnames(ipri_3_ca, c('Rama', 'indice_publicable'), c('CNAE3_est', 'ipri3_ca'))
  
  ### ipi con CNAE a 3 dígitos 
  ipri_3 <- ipri[(length_rama == 3) & (ccaa == '00')]
  ipri_3[, c('length_rama', 'ccaa') := NULL]
  
  setnames(ipri_3, c('Rama', 'indice_publicable'), c('CNAE3_est', 'ipri3'))
  
  data.list <- list(ipi4ca = ipi4ca, ipi4 = ipi4, ipi3ca = ipi3ca, ipi3 = ipi3, 
                    ipri_4_ca = ipri_4_ca, ipri_4 = ipri_4, ipri_3_ca = ipri_3_ca, ipri_3 = ipri_3)
  return(data.list)
  
}

