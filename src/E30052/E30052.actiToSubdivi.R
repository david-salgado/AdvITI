actiToSubdivi <- function(acti){
  
  acti.dt <- data.table(acti = acti)
  
  acti.dt[
    , GRUPO := substr(acti, 1, 3)][
    , GRUPO := ifelse( GRUPO=='101' | GRUPO=='102' | GRUPO=='103' | 
                       GRUPO=='104' | GRUPO=='105' | GRUPO=='107' | GRUPO=='108',
                       '10A', GRUPO)][
    , GRUPO := ifelse( GRUPO=='106' | GRUPO=='109', '10B', GRUPO )][
    , GRUPO := ifelse( GRUPO=='131' | GRUPO=='132' | GRUPO=='133', '13A', GRUPO)][
    , GRUPO := ifelse( GRUPO=='139', '13B', GRUPO)][
    , GRUPO := ifelse( GRUPO=='204', '20A', GRUPO)][
    , GRUPO := ifelse( GRUPO=='201' | GRUPO=='202' | GRUPO=='203' | 
                       GRUPO=='205' | GRUPO=='206', '20B', GRUPO)][
    , GRUPO := ifelse( GRUPO=='251' | GRUPO=='252' | GRUPO=='253' | 
                       GRUPO=='254', '25A', GRUPO)][
    , GRUPO := ifelse( GRUPO=='255' | GRUPO=='256' | GRUPO=='257' | 
                      GRUPO=='259', '25B', GRUPO)][
    , GRUPO := ifelse( GRUPO=='261' | GRUPO=='268', '26A', GRUPO)][
    , GRUPO := ifelse( GRUPO=='262' | GRUPO=='263' | 
                       GRUPO=='265' | GRUPO=='266', '26B', GRUPO)][
    , GRUPO := ifelse( GRUPO=='264' | GRUPO=='267', '26C', GRUPO)][
    , GRUPO := ifelse( GRUPO=='275', '27A', GRUPO)][
    , GRUPO := ifelse( GRUPO=='271' | GRUPO=='272' | GRUPO=='273' | 
                       GRUPO=='274' |GRUPO=='279', '27B', GRUPO)][
    , GRUPO := ifelse( GRUPO=='301' | GRUPO=='302' | 
                       GRUPO=='303' | GRUPO=='304', '30A', GRUPO)][
    , GRUPO := ifelse( GRUPO=='309', '30B', GRUPO)][
    , GRUPO := ifelse( GRUPO=='321' | GRUPO=='322', '32A', GRUPO)][
    , GRUPO := ifelse( GRUPO=='323' | GRUPO=='324' | GRUPO=='329', '32B', GRUPO)][
    , GRUPO := ifelse( GRUPO=='325', '32C', GRUPO)][
    , GRUPO := ifelse( substr(GRUPO,3,3) %in% c("A", "B", "C"), GRUPO, substr(acti, 1, 2) )]
  
  return(acti.dt$GRUPO)
  
}