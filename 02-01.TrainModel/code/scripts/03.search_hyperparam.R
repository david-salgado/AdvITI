#####                          :::::::::::::::                              #### 
#####                    SPECIFY PARAMETERS FROM SHELL                      ####
args <- commandArgs(trailingOnly = TRUE)
#args <- list("E30052", "MM042016", "01", "E30052.TrainModel_LocalParam.xml", "N:/UDMTD/UDTMDCOM/AdvITI_github")

#####                        Read arguments                                 ####
survey                  <- as.character(args[1])
refPeriod               <- as.character(args[2])
batch                   <- as.character(args[3])
localParamName          <- as.character(args[4])
main_path               <- as.character(args[5])

#####                          :::::::::::::::                              #### 
#####                LOAD FIRST PACKAGES AND FUNCTIONS                      ####

if(!require(xml2)) {
  
  install.packages("xml2", quiet = TRUE)
  library(xml2)
  
}


source(file.path(main_path, "src", "read_paths.R"))
source(file.path(main_path, "src", "load_functions.R"))


#####                          :::::::::::::::                              #### 
#####                    SPECIFY GLOBAL PARAMETERS FROM XML                 ####

# In this file we specify paths, packages and functions
paramGlobal.xml        <- read_xml(file.path(main_path, 
                                             '02-01.TrainModel/param/global/Ennnnn.TrainModel_GlobalParam.xml'))

#####                        Load packages                                  ####
execPackages <- xml_text(xml_find_all(paramGlobal.xml, "//packages//package//name"))
packLoaded   <- sapply(execPackages, function(x){require(x, character.only = TRUE)})
notLoaded    <- execPackages[!packLoaded]
if (length(notLoaded) > 0) {
  
  cat('The following packages cannot be loaded:')
  cat(paste0(paste(notLoaded, collapse = ', '), '.'))
  stop()
  
} 

#####                           Read paths                                  ####
# WARNING: This function loads variables path_[path name] into the global environment
read_paths(paramGlobal.xml)

#####                          :::::::::::::::                              #### 
#####                        LOAD MAIN FUNCTIONS                            ####
# WARNING: This function sources functions specified in the global parameter xml file
#          into the global environment
load_functions(paramGlobal.xml)

#####                          :::::::::::::::                              #### 
#####                  SPECIFY LOCAL PARAMETERS FROM XML                    ####

paramLocal.xml <- read_xml(file.path(path_param_local_survey, localParamName))

input_FD_fileName    <- xml_text(xml_find_all(paramLocal.xml, "//files_info//encoded_historical_FD_fileName"))
inputFD_fullFileName <- file.path(path_data_survey, input_FD_fileName)

initialFD     <- xml_text(xml_find_all(paramLocal.xml, "//time//initialFD"))
finalFD       <- getRepo(newRepoTime(refPeriod) - months(2))
if(int_end(RepoTimeTolubri(finalFD)[[1]]) < int_end(RepoTimeTolubri(initialFD)[[1]])){
  stop("More periods are needed for doing hyperparameters search.")
}
periods_train.RTI <- Seq(newRepoTime(initialFD), newRepoTime(finalFD), Rot = FALSE)
periods_test.RTI  <- periods_train.RTI + months(1)
periods_test      <- getRepo(periods_test.RTI) 
lper_test         <- periods_test[length(periods_test)]
lper_test.RTI     <- newRepoTime(lper_test)
date_test         <- as.Date(paste0(substr(lper_test, 5, 8), '-', substr(lper_test, 3, 4), "-01"), format = "%Y-%m-%d")

best_params_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//best_params_fileName"))
best_params_parsedFileName <- ParseUnitName(best_params_fileName, lper_test.RTI)
best_params_fullFileName   <- file.path(path_data_intermediate_survey, best_params_parsedFileName)

hyperParam_results_fileName     <- xml_text(xml_find_all(paramLocal.xml, "//files_info//hyperParam_results_fileName"))
hyperParam_results_fullFileName <- file.path(path_data_intermediate_survey, hyperParam_results_fileName)

model_label      <- xml_text(xml_find_all(paramLocal.xml, "//execution//model_params//label"))
save_model_label <- file.path(path_data_intermediate_survey, model_label)

target_name   <- xml_text(xml_find_all(paramLocal.xml, "//execution//target_var"))
id_var        <- xml_text(xml_find_all(paramLocal.xml, "//execution//id_var"))
id_var        <- trimws(strsplit(id_var, ",")[[1]])
collected_var <- xml_text(xml_find_all(paramLocal.xml, "//execution//collected_var"))
varsToRemove  <- xml_text(xml_find_all(paramLocal.xml, "//execution//varsToRemove"))
varsToRemove  <- trimws(strsplit(varsToRemove, ",")[[1]])

model_selection_params      <- xml_find_all(paramLocal.xml, "//execution//model_selection_params")
model_selection_params.list <- as_list(model_selection_params)[[1]]
attrib_params               <- unlist(purrr::map(xml_children(model_selection_params), ~xml_attr(., "type")))

model_selection_params.list <- mapply(function(param, atri){
  
  aux <- trimws(strsplit(param[[1]], ",")[[1]])
  if(atri == "num") aux <- as.numeric(aux)
  if(atri == "char") aux <- as.character(aux)
  return(aux)
  
}, model_selection_params.list, attrib_params)

criteria <- xml_text(xml_find_all(paramLocal.xml, "//execution//criteria"))


#####                          :::::::::::::::                              #### 
#####                           CREATE LOG                                  ####
logFile <- create_log(path_log_survey, name_logFile = "log_grid_search", sink_log = TRUE)

#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

#####                        Read input data                                ####


dt <- readRDS(inputFD_fullFileName)

#####                        Prepare data                                   ####

dt[, date := as.Date(paste0(year, "-", month, "-01"), format = "%Y-%m-%d")] 

allVarstoRemove <- intersect(varsToRemove, colnames(dt))
allVarstoKeep   <- setdiff(names(dt), allVarstoRemove)

#####                    Prepare hyperparameters                            ####

hyperParam_grid    <- expand.grid(model_selection_params.list, stringsAsFactors = FALSE)

if(file.exists(hyperParam_results_fullFileName)){
  
  hyperParam_oldResults.dt <- readRDS(hyperParam_results_fullFileName)
  
}else{
  
  hyperParam_oldResults.dt <- data.table(hyperParam_grid)[0][
    , date := as.Date("", format = "%Y-%m-%d")][
    , c("period") := NA_character_][
    , c("MSE", "total_error") := NA_real_]
  
}

hyperParam_oldResults.dt <- hyperParam_oldResults.dt[date <= date_test]
hyperParam_newResults.dt <- data.table(hyperParam_grid)[0][
  , date := as.Date("", format = "%Y-%m-%d")][
  , c("period") := NA_character_][
  , c("MSE", "total_error") := NA_real_]

#####                     Check the lack of periods                         ####

# Check if there is some period or combination of hyperparameters that is missing
model_selection_params.list$period <- periods_test
all.dt <- data.table(expand.grid(model_selection_params.list, stringsAsFactors = FALSE))

missing.dt <- all.dt[!hyperParam_oldResults.dt, on = c("period", names(hyperParam_grid))]

#####                     Execute the grid search                           ####

if(nrow(missing.dt) > 0){

  for(idx in seq(nrow(missing.dt))){
    
    cat(paste0("NUMBER OF EXECUTION: ", idx, "/", nrow(missing.dt)))
    info.dt <- missing.dt[idx, ]
    period_test <- info.dt[, period]
    param <- as.list(info.dt[, period := NULL])
    
    aux_date_test        <- as.Date(paste0(substr(period_test, 5, 8), '-', substr(period_test, 3, 4), "-01"), format = "%Y-%m-%d")
    aux_train.matrix <- data.matrix(dt[date < aux_date_test, ..allVarstoKeep])
    aux_train.matrix <- aux_train.matrix[, colnames(aux_train.matrix) != 'date']
    
    model_lmt       <- train_model(data.mat = aux_train.matrix, 
                                   param = param,
                                   target = target_name,
                                   save_model_label = save_model_label)

    aux_test.dt      <- dt[date == aux_date_test]
    aux_test.matrix  <- data.matrix(aux_test.dt[ , ..allVarstoKeep])
    aux_test.matrix <- aux_test.matrix[, !colnames(aux_test.matrix) %in% c('date', target_name)]
    
    aux_test.dt[, predictions_model := predict(model_lmt, aux_test.matrix)]
    
    aux_test.dt <- setup_predictions(aux_test.dt, prediction_name = "predictions_model")
    
    ##### Calculo por periodo y envio de los agregados #####
    
    
    total_error.dt <- aux_test.dt[, abs(sum(get(target_name)) - sum(predictions_model)), by = c('batch', 'date')]
    total_error.dt <- total_error.dt[, mean(V1), by = 'date']
    setnames(total_error.dt, "V1", "total_error")
    
    MSE.dt <- aux_test.dt[, sqrt(mean((get(target_name) - predictions_model)^2)), by = c('batch', 'date')]
    MSE.dt <- MSE.dt[, mean(V1), by = 'date']
    setnames(MSE.dt, "V1", "MSE")
    
    hyperParam_gridResults.dt <- Reduce(merge, list(total_error.dt, MSE.dt))
    
    hyperParam_gridResults.dt[
      , (names(info.dt)) := info.dt][
      , period := period_test]
    
    setcolorder(hyperParam_gridResults.dt, names(hyperParam_newResults.dt))
    
    hyperParam_newResults.dt <- rbindlist(list(hyperParam_newResults.dt, hyperParam_gridResults.dt))
    
  }  
  
}

hyperParam_grid_results_total.dt <- rbindlist(list(hyperParam_oldResults.dt, hyperParam_newResults.dt))


saveRDS(hyperParam_grid_results_total.dt, hyperParam_results_fullFileName)
  


#####                  Select best hyperparameters                          ####


aggre_names <- setdiff(names(hyperParam_grid_results_total.dt), c(names(hyperParam_grid), "date", "period"))
selection.dt <- hyperParam_grid_results_total.dt[as.Date(date) <= as.Date(date_test)][!is.na(get(criteria))]
aggre.dt <- selection.dt[, lapply(.SD, sum), .SDcols = aggre_names, by = names(hyperParam_grid)]
best_params.dt <- aggre.dt[get(criteria) == min(get(criteria)),][,  names(hyperParam_grid), with = FALSE]
best_params.list <- as.list(best_params.dt)


#####                          :::::::::::::::                              #### 
#####                             SAVE RDS                                  ####

saveRDS(best_params.list, best_params_fullFileName)


###                            :::::::::::::::                              ####
#####                             CLOSE LOG                                #####
flog.info(msg = paste0(survey, '::: Closing log file.'), name = 'logFile')
close_log(logFile = logFile, sink_log = TRUE)
