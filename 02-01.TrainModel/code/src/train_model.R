


train_model <- function(data.mat, param, target, save_model_label){

  model_name <- param$model_name
 
  if(model_name == 'xgb') {
  
    trained_model<-xgboost(data = data.mat[, colnames(data.mat) != target], 
                           label = data.mat[, target],
                           params = param,
                           nrounds = param$nrounds)
  
  }
  
  
  
  if(model_name == 'lgb'){
    
  lgb.dy1 <- lightgbm::lgb.Dataset(data = data.mat[, colnames(data.mat) != target], 
                                   label = data.mat[, target])

  trained_model <- lightgbm::lightgbm(data = lgb.dy1, 
                                      nrounds = param$nrounds,
                                      params = param,
                                      save_name = save_model_label) 
 
  }
  
  if (model_name == "rf"){
    trained_model = ranger::ranger(get(target) ~.,
                                   data = data.mat,
                                   mtry = length(data.mat/3),
                                   num.trees = 10,
                                   min.node.size = 5) 
  }
  
  if (model_name == "lasso"){
    
    label <- data.mat[, target]
    df <- data.mat[, colnames(data.mat) != target]
    trained_model <- glmnet(x = df,
                            y = label, alpha = 1)
  
  }
  
  return(trained_model)

}
