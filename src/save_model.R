save_model <- function(object, fileName, type){
  
  if (type == 'xgb') {
    
    mapply(function(model, name){
      
      xgb.save(model,  name)
      
      }, object, fileName)
    
  }
  if (type =='lgb'){
    
    mapply(function(model, name){
      
      lgb.save(model,  name)
      
    }, object, fileName)
    
  }
  else{
    
    mapply(function(model, name){
      
      saveRDS(model,  name)
      
    }, object, fileName)
    
  }
  
  return(TRUE)
}