#####                          :::::::::::::::                              #### 
#####                    SPECIFY PARAMETERS FROM SHELL                      ####
args <- commandArgs(trailingOnly = TRUE)
#args <- list("E30052", "MM102017", "02", "E30052.ComputeAggregates_LocalParam.xml", "N:/UDMTD/UDTMDCOM/AdvITI_github")

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
                                             '03-01.ComputeAggregates/param/global/Ennnnn.ComputeAggregates_GlobalParam.xml'))

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

DDversion      <- xml_text(xml_find_all(paramLocal.xml, "//files_info//DDversion"))

historical_fileName     <- xml_text(xml_find_all(paramLocal.xml, "//files_info//encoded_historical_FD_fileName"))
historical_fullFileName <- file.path(path_data_survey, historical_fileName)


initialFD     <- xml_text(xml_find_all(paramLocal.xml, "//time//initialFD"))
initialError  <- xml_text(xml_find_all(paramLocal.xml, "//time//initialError"))
finalFD       <- getRepo(newRepoTime(refPeriod) - months(1))
precPeriod12  <- getRepo(newRepoTime(refPeriod) - months(12))

if(int_end(RepoTimeTolubri(refPeriod)[[1]]) < int_end(RepoTimeTolubri(initialError)[[1]])){
  stop("More periods are needed for doing hyperparameters search.")
}

rates1  <- TRUE
rates12 <- TRUE
if(int_end(RepoTimeTolubri(precPeriod12)[[1]]) < int_end(RepoTimeTolubri(initialError)[[1]])){
  rates12 <- FALSE
}
if(int_end(RepoTimeTolubri(finalFD)[[1]]) < int_end(RepoTimeTolubri(initialError)[[1]])){
  rates1 <- FALSE
}
lmtrain.RTI   <- newRepoTime(finalFD)
refPeriod.RTI <- newRepoTime(refPeriod)
precPeriod1.RTI <- newRepoTime(finalFD)
precPeriod12.RTI <- newRepoTime(precPeriod12)

valueBatch <- ifelse(batch == "00", "01", batch)

predictions_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//predictions_fileName"))
predictions_parsedFileName <- ParseUnitName(predictions_fileName, refPeriod.RTI) 
predictions_parsedFileName <- ParseUnitName(predictions_parsedFileName, paste0("00", valueBatch))                                                 
predictions_fullFileName   <- file.path(path_data_survey, predictions_parsedFileName)

errors_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//errors_fileName"))
errors_parsedFileName <- ParseUnitName(errors_fileName, refPeriod.RTI) 
errors_parsedFileName <- ParseUnitName(errors_parsedFileName, paste0("00", valueBatch))                                                 
errors_fullFileName   <- file.path(path_data_survey, errors_parsedFileName)

aggreEstim_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//aggregate_estimation_fileName"))
aggreEstim_parsedFileName <- ParseUnitName(aggreEstim_fileName, refPeriod.RTI) 
aggreEstim_parsedFileName <- ParseUnitName(aggreEstim_parsedFileName, paste0("00", batch))                                                 
aggreEstim_fullFileName   <- file.path(path_data_survey, aggreEstim_parsedFileName)

aggreIndex_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//aggregate_index_estimation_fileName"))
aggreIndex_parsedFileName <- ParseUnitName(aggreIndex_fileName, refPeriod.RTI) 
aggreIndex_parsedFileName <- ParseUnitName(aggreIndex_parsedFileName, paste0("00", batch))                                                 
aggreIndex_fullFileName   <- file.path(path_data_intermediate_survey, aggreIndex_parsedFileName)

aggreIndex_1_parsedFileName <- ParseUnitName(aggreIndex_fileName, precPeriod1.RTI) 
aggreIndex_1_parsedFileName <- ParseUnitName(aggreIndex_1_parsedFileName, paste0("00", batch))                                                 
aggreIndex_1_fullFileName   <- file.path(path_data_intermediate_survey, aggreIndex_1_parsedFileName)

aggreIndex_12_parsedFileName <- ParseUnitName(aggreIndex_fileName, precPeriod12.RTI) 
aggreIndex_12_parsedFileName <- ParseUnitName(aggreIndex_12_parsedFileName, paste0("00", batch))                                                 
aggreIndex_12_fullFileName   <- file.path(path_data_intermediate_survey, aggreIndex_12_parsedFileName)

sampleSize_fileName     <- xml_text(xml_find_all(paramLocal.xml, "//files_info//sampleSize_fileName"))
sampleSize_fullFileName <- file.path(path_indicators_survey, sampleSize_fileName)


target_name   <- xml_text(xml_find_all(paramLocal.xml, "//execution//target_var"))
id_var        <- xml_text(xml_find_all(paramLocal.xml, "//execution//id_var"))
id_var        <- trimws(strsplit(id_var, ",")[[1]])
collected_var <- xml_text(xml_find_all(paramLocal.xml, "//execution//collected_var"))

#####                          :::::::::::::::                              #### 
#####                           CREATE LOG                                  ####
logFile <- create_log(path_log_survey, name_logFile = "log_aggre_estim", sink_log = TRUE)



#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

#####                        Read input data                                ####

predictions.dt                  <- readRDS(predictions_fullFileName)
errors.dt                       <- readRDS(errors_fullFileName)
if(rates1){
  composed_index_estimation_1.dt  <- readRDS(aggreIndex_1_fullFileName)
}
if(rates12){
  composed_index_estimation_12.dt <- readRDS(aggreIndex_12_fullFileName)
}


#####                        Prepare data                                   ####


microdata.list <- prepare_microdata(
  predictions = predictions.dt, 
  errors = errors.dt,
  error_varname = "aggre_rmse",
  collected_var = collected_var,
  refPeriod = refPeriod,
  batch = batch,
  repoInfo = list(DDversion = DDversion, path_repo = path_repo_survey))


#####                      Compute aggregates                               ####

index_estimation.dt <- calculate_elementary_index_batch(microdata.list,
                                                        collected_var, refPeriod, batch)

composed_index_estimation.dt <- calculate_composed_index_batch(all_elemental_index = index_estimation.dt,
                                                               FF_previous.StQ = microdata.list$FF_previous.StQ,
                                                               collected_var, refPeriod, batch)

if(rates1 & !rates12){
  
  
  list_complete_rates.dt <- calculate_rate_of_change_batch(data_refPeriod.dt = composed_index_estimation.dt,
                                                           data_precPeriod1.dt = composed_index_estimation_1.dt,
                                                           annual = FALSE, batch = batch)
  
}
if(rates1 & rates12){
  
  list_complete_rates.dt <- calculate_rate_of_change_batch(data_refPeriod.dt = composed_index_estimation.dt,
                                                           data_precPeriod1.dt = composed_index_estimation_1.dt,
                                                           data_precPeriod12.dt = composed_index_estimation_12.dt,
                                                           annual = TRUE, batch = batch)
}



#####                          :::::::::::::::                              #### 
#####                             SAVE RDS                                  ####


saveRDS(composed_index_estimation.dt, aggreIndex_fullFileName)
if(rates1){
  saveRDS(list_complete_rates.dt, aggreEstim_fullFileName)  
}

#####                     Indicators (sample size)                          ####

n <- nrow(microdata.list$outputVar$sample.dt)
print(paste0("Number of units in sample:", n))
if(file.exists(sampleSize_fullFileName)){
  n.dt <- readRDS(sampleSize_fullFileName)  
}else{
  n.dt <- data.table(period = NA_character_, batch = NA_character_, size = NA_integer_)
}

n.dt <- rbind(n.dt, data.table(period = refPeriod, batch = batch, size = n))
saveRDS(n.dt, sampleSize_fullFileName)


###                            :::::::::::::::                              ####
#####                             CLOSE LOG                                #####
flog.info(msg = paste0(survey, '::: Closing log file.'), name = 'logFile')
close_log(logFile = logFile, sink_log = TRUE)



