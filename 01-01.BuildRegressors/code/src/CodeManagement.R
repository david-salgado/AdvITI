# Correspondencia de un código de tres dígitos con el correspondiente de dos
Code3toCode2 <- function(Code3){

  auxDT <- data.table(Code3 = Code3)[
    , Code2 := substr(Code3, 1, 2)]
  auxDT[is.na(Code2), Code2 := Code3]
  auxDT[Code3 == '000', Code2 := '000']
  Code2 <- auxDT$Code2
  return(Code2)

}

# Correspondencia de cógidos CNAE a 2 dígitos con su grupo
CNAE2toCNAE1 <- function(CNAE2){

  auxDT <- data.table(CNAE2 = CNAE2)[
    CNAE2 %in% c('01', '02', '03'), CNAE1 := 'A'][
      CNAE2 %in% c('05', '06' , '07', '08', '09'), CNAE1 := 'B'][
        CNAE2 %in% as.character(10:33), CNAE1 := 'C'][
          CNAE2 == '35', CNAE1 := 'D'][
            CNAE2 %in% as.character(36:39), CNAE1 := 'E'][
              CNAE2 %in% as.character(41:43), CNAE1 := 'F'][
                CNAE2 %in% as.character(45:47), CNAE1 := 'G'][
                  CNAE2 %in% as.character(49:53), CNAE1 := 'H'][
                    CNAE2 %in% as.character(55:56), CNAE1 := 'I'][
                      CNAE2 %in% as.character(58:63), CNAE1 := 'J'][
                        CNAE2 %in% as.character(64:66), CNAE1 := 'K'][
                          CNAE2 == '68', CNAE1 := 'L'][
                            CNAE2 %in% as.character(69:75), CNAE1 := 'M'][
                              CNAE2 %in% as.character(77:82), CNAE1 := 'N'][
                                CNAE2 == '84', CNAE1 := 'O'][
                                  CNAE2 == '85', CNAE1 := 'P'][
                                    CNAE2 %in% as.character(86:88), CNAE1 := 'Q'][
                                      CNAE2 %in% as.character(90:93), CNAE1 := 'R'][
                                        CNAE2 %in% as.character(94:96), CNAE1 := 'S'][
                                          CNAE2 %in% as.character(97:98), CNAE1 := 'T'][
                                            CNAE2 == '99', CNAE1 := 'U']
  auxDT[is.na(CNAE1) | CNAE2 == '', CNAE1 := CNAE2]
  CNAE1 <- auxDT$CNAE1
  return(CNAE1)

}

# Grupos de Edad utilizados en ENSE
ageGroup <- function(Age){

  ageN<-as.numeric(Age)
  auxDT <- data.table(Age = Age)[
    (ageN>= 0) & (ageN<=4), grAge:='01'][
    (ageN >=  5) & (ageN <= 14), grAge := '02'][
    (ageN >= 15) & (ageN <= 24), grAge := '03'][
    (ageN >= 25) & (ageN <= 34), grAge := '04'][
    (ageN >= 35) & (ageN <= 44), grAge := '05'][
    (ageN >= 45) & (ageN <= 54), grAge := '06'][
    (ageN >= 55) & (ageN <= 64), grAge := '07'][
    (ageN >= 65) & (ageN <= 74), grAge := '08'][
    (ageN >= 75) & (ageN <= 84), grAge := '09'][
    (ageN >= 85) , grAge := '10']

  grAge <- auxDT$grAge
  return(grAge)

}

# Correspondencia de códigos CNAE a 3 dígitos con su grupo
CNAE3toCNAE1 <- function(CNAE3){

  CNAE2 <- Code3toCode2(CNAE3)
  CNAE1 <- CNAE2toCNAE1(CNAE2)
  return(CNAE1)

}

# Correspondencia de códigos CNO a 2 dígitos con su grupo
CNO2toCNO1 <- function(CNO2){

  auxDT <- data.table(CNO2 = CNO2)[
    CNO2 %in% as.character(11:15), CNO1 := 'A'][
      CNO2 %in% as.character(21:23), CNO1 := 'B'][
        CNO2 %in% as.character(24:29), CNO1 := 'C'][
          CNO2 %in% as.character(31:38), CNO1 := 'D'][
            CNO2 %in% as.character(41:43), CNO1 := 'E'][
              CNO2 %in% as.character(44:45), CNO1 := 'F'][
                CNO2 %in% as.character(50:55), CNO1 := 'G'][
                  CNO2 %in% as.character(56:58), CNO1 := 'H'][
                    CNO2 == '59', CNO1 := 'I'][
                      CNO2 %in% as.character(61:64), CNO1 := 'J'][
                        CNO2 %in% as.character(71:72), CNO1 := 'K'][
                          CNO2 %in% as.character(73:78), CNO1 := 'L'][
                            CNO2 %in% as.character(81:82), CNO1 := 'M'][
                              CNO2 %in% as.character(83:84), CNO1 := 'N'][
                                CNO2 %in% as.character(91:94), CNO1 := 'O'][
                                  CNO2 %in% as.character(95:98), CNO1 := 'P']
  auxDT[is.na(CNO1) | CNO2 == '', CNO1 := CNO2]
  CNO1 <- auxDT$CNO1
  return(CNO1)
}

# Correspondencia de códigos CNO a 3 dígitos con su grupo
CNO3toCNO1 <- function(CNO3){

  CNO2 <- Code3toCode2(CNO3)
  CNO1 <- CNO2toCNO1(CNO2)
  return(CNO1)

}

# Correspondencia de códigos CNO con su clase social
CNOtoCS <- function(CNO){
  
  auxDT <- data.table(CNO = CNO)[
    CNO %in% c('111','112','121','122','131','132','211','213','214','215','221','223',
              '241','242','243','244','245','251','259','261','262','265','271','281',
              '282','291','292','283'), CS := '1'][
                
      CNO %in% c('141','142','143','150','212','222','224','225','231','232','246','247',
                '263','264','248','272','293','311','315','316','333','362','372','373',
                '001'), CS := '2'][
                  
        CNO %in% c('331','332','340','351','352','353','361','363','371','381','382','383',
        '411','412','421','422','430','441','442','443','444','450','582','591',
        '592','593','002','500','530','584'), CS := '3'][  
          
          CNO %in% c('312','313','314','320','521','581','713','719','721','722','723',
                    '725', '731','732','740','751','752','753','761','782','783','789',
                    '831'), CS := '4'][  
                      
            CNO %in% c('511','512','522','541','543','549','550','561','562','571','572',
                      '589','594','599','611','612','620','630','641','642','643','711',
                      '712','724', '729','762','770','781','811','812','813','814','815',
                      '816','817','819', '820','832','833','841','842','843'), CS := '5'][  
                        
              CNO %in% c('542','583','834','844','910','921','922','931','932','941',
                        '942', '943', '944','949','951','952','953','954','960','970',
                        '981','982'), CS := '6'][ 
                          
                CNO %in% c('000'), CS := 'NC'][ 
                  
                  CNO %in% c(''), CS := 'NP']    
  
    CS <- auxDT$CS
    return(CS)
}
