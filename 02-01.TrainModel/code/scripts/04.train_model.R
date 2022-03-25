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

input_FD_fileName     <- xml_text(xml_find_all(paramLocal.xml, "//files_info//encoded_historical_FD_fileName"))
input_FD_fullFileName <- file.path(path_data_survey, input_FD_fileName)

initialFD     <- xml_text(xml_find_all(paramLocal.xml, "//time//initialFD"))
finalFD       <- getRepo(newRepoTime(refPeriod) - months(1))
if(int_end(RepoTimeTolubri(finalFD)[[1]]) < int_end(RepoTimeTolubri(initialFD)[[1]])){
  stop("More periods are needed for doing hyperparameters search.")
}
lmtrain.RTI   <- newRepoTime(finalFD)

save_model_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//model_fileName"))
save_model_parsedfileName <- ParseUnitName(save_model_fileName, lmtrain.RTI)
save_model_fullFileName   <- file.path(path_data_survey, save_model_parsedfileName)

save_model0_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//model_t0_fileName"))
save_model0_parsedfileName <- ParseUnitName(save_model0_fileName, lmtrain.RTI)
save_model0_fullFileName   <- file.path(path_data_survey, save_model0_parsedfileName)

best_params_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//best_params_fileName"))
best_params_parsedFileName <- ParseUnitName(best_params_fileName, lmtrain.RTI)
best_params_fullFileName   <- file.path(path_data_intermediate_survey, best_params_parsedFileName)

importance_fileName               <- xml_text(xml_find_all(paramLocal.xml, "//files_info//importance_fileName"))
importance_parsedFileName         <- ParseUnitName(importance_fileName, lmtrain.RTI) 
importance_fullFileName           <- file.path(path_data_intermediate_survey, importance_parsedFileName)

model_label      <- xml_text(xml_find_all(paramLocal.xml, "//execution//model_params//label"))
save_model_label <- file.path(path_data_intermediate_survey, model_label)
plot_importance  <- as.logical(xml_text(xml_find_all(paramLocal.xml, "//execution//plot_importance")))

target_name           <- xml_text(xml_find_all(paramLocal.xml, "//execution//target_var"))
id_var                <- xml_text(xml_find_all(paramLocal.xml, "//execution//id_var"))
id_var                <- trimws(strsplit(id_var, ",")[[1]])
collected_var         <- xml_text(xml_find_all(paramLocal.xml, "//execution//collected_var"))
varsToRemove          <- xml_text(xml_find_all(paramLocal.xml, "//execution//varsToRemove"))
varsToRemove          <- trimws(strsplit(varsToRemove, ",")[[1]])
varsToRemove_current  <- xml_text(xml_find_all(paramLocal.xml, "//execution//varsToRemove_current"))
varsToRemove_current  <- trimws(strsplit(varsToRemove_current, ",")[[1]])

model_selection_params   <- xml_find_all(paramLocal.xml, "//execution//model_selection_params")
model_selection_params.list <- as_list(model_selection_params)[[1]]
attrib_params <- unlist(purrr::map(xml_children(model_selection_params), ~xml_attr(., "type")))

#####                          :::::::::::::::                              #### 
#####                           CREATE LOG                                  ####
logFile <- create_log(path_log_survey, name_logFile = "log_train_model", sink_log = TRUE)



#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

#####                        Read input data                                ####


dt <- readRDS(input_FD_fullFileName)


#####                        Read best params                               ####

if(file.exists(best_params_fullFileName)){

  best_params.list <- readRDS(best_params_fullFileName)
  
}else{
  
  stop(paste0("Hyperparam file is missing: ", best_params_fullFileName))
  
}


#####                        Prepare data                                   ####

dt[, date := as.Date(paste0(year, "-", month, "-01"), format = "%Y-%m-%d")] 

varsToRemove         <- c('date', varsToRemove)
allVarstoRemove      <- intersect(varsToRemove, colnames(dt))
allVarstoKeep        <- setdiff(names(dt), allVarstoRemove)
allVarstoKeepCurrent <- setdiff(allVarstoKeep, varsToRemove_current)

date_finalTrain <- as.Date(paste0(substr(finalFD, 5, 8), '-', substr(finalFD, 3, 4), "-01"), format = "%Y-%m-%d")
train.matrix    <- data.matrix(dt[date <= date_finalTrain, ..allVarstoKeep])
train0.matrix   <- data.matrix(dt[date <= date_finalTrain, ..allVarstoKeepCurrent])

#####                     Train the model                                   ####

model_lmt <- train_model(data.mat = train.matrix, 
                         param = best_params.list,
                         target = target_name,
                         save_model_label = save_model_label)

if (class(model_lmt) == 'lgb.Booster'){
  
  importance.dt <- lgb.importance(model_lmt, percentage = TRUE)

  ############### plot importance ####
  if(plot_importance){
    
    importance.dt <- importance.dt[order(-rank(Gain))]
    
    lgb.plot.importance(importance.dt, top_n=20L)
    
  }
  
}



# train without current regressors to obtained predictions at t=0
model0_lmt <- train_model(data.mat = train0.matrix, 
                          param = best_params.list,
                          target = target_name,
                          save_model_label = save_model_label)

#####                          :::::::::::::::                              #### 
#####                             SAVE RDS                                  ####

save_model(object = list(model_lmt, model0_lmt),
           fileName = list(save_model_fullFileName, save_model0_fullFileName),
           type = best_params.list$model_name)

if(exists("importance.dt")){
  saveRDS(importance.dt, importance_fullFileName)
}

###                            :::::::::::::::                              ####
#####                             CLOSE LOG                                #####
flog.info(msg = paste0(survey, '::: Closing log file.'), name = 'logFile')
close_log(logFile = logFile, sink_log = TRUE)



