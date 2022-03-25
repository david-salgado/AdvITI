actiToDivSubdiv <- function(acti){
  

  
  DIVI <- substr(acti, 1, 2)
  GRUPO <- substr(acti, 1, 3)  
  
  if( GRUPO=='101' | GRUPO=='102' | GRUPO=='103' | GRUPO=='104' |                  
      GRUPO=='105' | GRUPO=='107' | GRUPO=='108') {
    
    GRUPO=='10A'
    
  }
  
  if( GRUPO=='106' | GRUPO=='109') {
    
    GRUPO=='10B'
    
  }                       
  
  if( GRUPO=='131' | GRUPO=='132' | GRUPO=='133') {
    
    GRUPO=='13A'
    
  }                                                     
  
  if( GRUPO=='139') {
    
    GRUPO=='13B'
    
  }                                      
  
  if( GRUPO=='204') {
    
    GRUPO=='20A'
    
  }                                      
  
  if( GRUPO=='201' | GRUPO=='202' | GRUPO=='203' | 
      GRUPO=='205' | GRUPO=='206') {
    
    GRUPO=='20B'
    
  }                                      
  
  if( GRUPO=='251' | GRUPO=='252' | GRUPO=='253' | 
      GRUPO=='254' ) {
    
    GRUPO=='25A'
    
  }                                                     
  
  if( GRUPO=='255' | GRUPO=='256' | GRUPO=='257' | GRUPO=='259') {            
  
    GRUPO=='25B'
    
  }
  
  if( GRUPO=='261' | GRUPO=='268') {
    
    GRUPO=='26A'
    
  }
  
  if( GRUPO=='262' | GRUPO=='263' | GRUPO=='265' | GRUPO=='266' ) {
    
    GRUPO=='26B'
    
  }                                                     
  
  if( GRUPO=='264' | GRUPO=='267') {
    
    GRUPO=='26C'
    
  }                       
  
  if( GRUPO=='275') {
    
    GRUPO=='27A'
    
  }                                      
  
  if( GRUPO=='271' | GRUPO=='272' | GRUPO=='273' | 
      GRUPO=='274' |GRUPO=='279') {
    
    GRUPO=='27B'
    
  }
  
  if( GRUPO=='301' | GRUPO=='302' | GRUPO=='303' | GRUPO=='304' ) {
    
    GRUPO=='30A'
    
  }                                                     
  if( GRUPO=='309') {
    
    GRUPO=='30B'
    
  }                                      
  if( GRUPO=='321' | GRUPO=='322') {
    
    GRUPO=='32A'
    
  }                       
  if( GRUPO=='323' | GRUPO=='324' | GRUPO=='329') {
    
    GRUPO=='32B'
    
  }        
  if( GRUPO=='325') {
    
    GRUPO=='32C'
    
  }
  
  if( !substr(GRUPO,3,1) %in% c("A", "B", "C") ) {
    
    GRUPO=DIVI
    
  }
  
  return(list(DIVI = DIVI, GRUPO = GRUPO))
  
}