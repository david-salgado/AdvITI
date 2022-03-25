CNAE3toCNAEMIG <- function(CNAE3){
  
  BC = c('251', '252', '253','254','262','263','264','265','266','281','282','283'
         ,'284','289','291','292','293','301','302','303','304','325','331','332')
  BI = c("071","072","081","089","106",'109','131',"132",'133','161','162','171','172',
         '201','202','203','205','206','221','222','231','232','233','234','235','236',
         '237','239','241','242','243','244','245','255','256','257','259','261','268',
         '271','272','273','274','279')
  CD = c('264','267','275','309','310','321','322')
  CN = c('101','102','103','104','105','107','108','110','120','139','141','142','143',
         '151','152','181','182','204','211','212','323','324','329')
  
  EN = c('051','052','061','191','192'
  )
  XC = c()
 
   auxDT <- data.table(CNAE3 =CNAE3)[
    CNAE3 %in% BC, CNAEMIG := 'BC'][
      CNAE3 %in% BI, CNAEMIG := 'BI'][
        CNAE3 %in% CD, CNAEMIG := 'CD'][
          CNAE3 %in% CN, CNAEMIG := 'CN'][
            CNAE3 %in% EN, CNAEMIG := 'EN'][
              CNAE3 %in% XC, CNAEMIG := 'XC']
  auxDT[is.na(CNAEMIG) | CNAE3 == '', CNAEMIG := CNAE3]
  CNAEMIG <- auxDT$CNAEMIG
  return(CNAEMIG)
  
}

CNAE3toCNAESub <- function(CNAE3){
  
  A25 = c('251', '252', '253','254')
  B26 = c('262','263','264','265','266')
  S28 = c('281','282','283','284','289')
  S29 = c('291','292','293')
  A30 = c('301','302','303','304')
  C32='325'
  S33 = c('331','332')
  S07 = c("071","072")
  S08 = c("081","089")
  B10 = c("106",'109')
  A13 = c('131',"132",'133')
  S16 = c('161','162')
  S17 = c('171','172')
  B20 = c('201','202','203','205','206')
  S22 = c('221','222')
  S23 = c('231','232','233','234','235','236','237','239')
  S24 = c('241','242','243','244','245')
  B25 = c('255','256','257','259')
  A26 = c('261','268')
  B27 = c('271','272','273','274','279')
  C26 = c('264','267')
  A27 = c('275')
  B30 = c('309')
  S31 = c('310')
  A32 = c('321','322')
  A10 = c('101','102','103','104','105','107','108')
  S11 = c('110')
  S12 = c('120')
  B13 = c('139')
  S14= c('141','142','143')
  S15 = c('151','152')
  S16 = c('181','182')
  A20 = c('204')
  S21 = c('211','212')
  B32 = c('323','324','329')
  S05 = c('051','052')
  S06 = c('061')
  S19 = c('191','192')
  XC = c()
  
  auxDT <- data.table(CNAE3 =CNAE3)[
    CNAE3 %in% A25, CNAESub := '25A'][
      CNAE3 %in% B26, CNAESub := '26B'][
        CNAE3 %in% S28, CNAESub := '28'][
          CNAE3 %in% S29, CNAESub := '29'][
            CNAE3 %in% A30, CNAESub := '30A'][
              CNAE3 %in% C32, CNAESub := '32C'][
                CNAE3 %in% S33, CNAESub := '33'][
                  CNAE3 %in% S07, CNAESub := '07'][
                    CNAE3 %in% S08, CNAESub := '08'][
                      CNAE3 %in% B10, CNAESub := '10B'][
                        CNAE3 %in% A13, CNAESub := '13A'][
                          CNAE3 %in% S16, CNAESub := '16'][
                            CNAE3 %in% S17, CNAESub := '17'][
                              CNAE3 %in% B20, CNAESub := '20B'][
                                CNAE3 %in% S22, CNAESub := '22'][
                                  CNAE3 %in% S23, CNAESub := '23'][
                                    CNAE3 %in% S24, CNAESub := '24'][
                                      CNAE3 %in% B25, CNAESub := '25B'][
                                        CNAE3 %in% A26, CNAESub := '26A'][
                                          CNAE3 %in% B27, CNAESub := '27B'][
                                            CNAE3 %in% C26, CNAESub := '26C'][
                                              CNAE3 %in% A27, CNAESub := '27A'][
                                                CNAE3 %in% B30, CNAESub := '30B'][
                                                  CNAE3 %in% S31, CNAESub := '31'][
                                                    CNAE3 %in% A32, CNAESub := '32A'][
                                                      CNAE3 %in% A10, CNAESub := '10A'][
                                                        CNAE3 %in% S11, CNAESub := '11'][
                                                          CNAE3 %in% S12, CNAESub := '12'][
                                                            CNAE3 %in% B13, CNAESub := '13B'][
                                                              CNAE3 %in% S14, CNAESub := '14'][
                                                                CNAE3 %in% S15, CNAESub := '15'][
                                                                  CNAE3 %in% S16, CNAESub := '16'][
                                                                    CNAE3 %in% A20, CNAESub := '20A'][
                                                                      CNAE3 %in% S21, CNAESub := '21'][
                                                                        CNAE3 %in% B32, CNAESub := '32B'][
                                                                          CNAE3 %in% S05, CNAESub := '05'][
                                                                            CNAE3 %in% S06, CNAESub := '06'][
                                                                              CNAE3 %in% S19, CNAESub := '19']
  
  auxDT[is.na(CNAESub) | CNAE3 == '', CNAESub := CNAE3]
  CNAESub <- auxDT$CNAESub
  return(CNAESub)
  
}

resultacode <- function(resulta){
  
  imputa = c('DC','EX','RX','SI','NC','EN','ND','SE','NE','RE','GR','GI','GM','BL','CE','CI','SC','IN',"SF")
  noimputa = c('GV','EI','FA','IL', 'LI','SA','CL',"WE")
  imputa0 = c('CS','CV','CO','CT')
  baja = c('DU','CD')
  cawi = c()
  auxDT <- data.table(resulta = resulta)[
    resulta %in% imputa, imputar := 'Si'][
      resulta %in% noimputa,imputar  := 'No'][
        resulta %in% imputa0, imputar := 'Cero'][
          resulta %in% baja, imputar := 'Baja'][
            resulta %in% cawi, imputar := 'Cawi'
          ]
  imputa <- auxDT$imputar
  return(imputa)
  
}



codigovalidez <- function(datatable)
{
  validos = c('1')
  novalidos = c('0','3','4','9')
  data = datatable
  data[cn01a %in% validos, validez := 'SI'][
    cn01a %in% novalidos, validez := "NO"
  ]
  data[is.na(validez), validez := 'SI']
 return(data) 
}

envioranger <- function(regresores, envio, datacomplete = 1)
{
  regresores <- regresoresRF[,(dropvariables):=NULL]
  regresores <- regresoresRF[,(drop2):=NULL]
  regresores <- regresores[!is.na(intermensual.cn01)]
  regresores <- regresores[!is.na(imputar)]
  regresores <- regresores[!is.na(cn01.anterior)]
  regresores <- regresores[!is.na(interanual.cn01)]
  regresores <- regresores[!is.na(cn01a.anterior)]
  regresores <- regresores[!is.na(cn01v.anterior)]
  regresores <- regresores[!is.na(cn01e.anterior)]
  regresores <- regresores[!is.na(cn01.fde)]
  if (envio != 1)
  {
    regresores <- datacomplete[regresores, on=.(numidest), CN01 := .(i.CN01)][]
  }
  data.train <- regresores[!is.na(regresores$CN01)]
  if (envio !=1)
  {
  data.train$imputado <- "No"
  }
  data.control <- regresores[is.na(regresores$CN01)]
  data.control$imputado <- envio
  ranger <- ranger(CN01 ~ . -cn01.fde  , data = na.omit(data.train), mtry = sqrt(ncol(data.train)) ,  importance = "impurity")
  preds = predict(ranger, data = data.control)
  data.control$CN01 = preds$predictions
  
  datacomplete <- rbind(data.train,data.control)
  return(datacomplete)
}
