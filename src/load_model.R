load_model <- function(fileName, type){
  
  if (type == 'xgb') {
    
    output <- lapply(fileName, function(name){
      
      xgb.load(name)
    })
    
  }
  if (type =='lgb'){
    
    output <- lapply(fileName, function(name){
      
      lgb.load(name)
      
    })
    
  }
  else{
    
    output <- lapply(fileName, function(name){
      
      readRDS(name)
      
    })
    
  }
  
  return(output)
}