#####                          :::::::::::::::                              #### 
#####                    SPECIFY PARAMETERS FROM SHELL                      ####
args <- commandArgs(trailingOnly = TRUE)
#args <- list("E30052", "MM102016", "01", "E30052.EvaluatePredictions_LocalParam.xml", "N:/UDMTD/UDTMDCOM/AdvITI_github")

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
                                             '02-21.EvaluatePredictions/param/global/Ennnnn.EvaluatePredictions_GlobalParam.xml'))

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

target_name   <- xml_text(xml_find_all(paramLocal.xml, "//execution//target_var"))
id_var        <- xml_text(xml_find_all(paramLocal.xml, "//execution//id_var"))
id_var        <- trimws(strsplit(id_var, ",")[[1]])
collected_var <- xml_text(xml_find_all(paramLocal.xml, "//execution//collected_var"))
group_var     <- xml_text(xml_find_all(paramLocal.xml, "//execution//unit_params//group_var"))
group_var     <- trimws(strsplit(group_var, ",")[[1]])
len_estimPer  <- as.integer(xml_text(xml_find_all(paramLocal.xml, "//execution//unit_params//length_estimPeriod")))
aggre_rmse    <- xml_text(xml_find_all(paramLocal.xml, "//execution//unit_params//aggre_rmse"))

firstPred   <- xml_text(xml_find_all(paramLocal.xml, "//time//initialPred"))
finalPred   <- getRepo(newRepoTime(refPeriod) - months(1))
initialPred <- getRepo(newRepoTime(finalPred) - months(len_estimPer - 1))

if(int_end(RepoTimeTolubri(firstPred)[[1]]) > int_end(RepoTimeTolubri(initialPred)[[1]])){
   stop("More periods predicted are needed to obtain rmse.")
}

periodsPred <- getRepo(Seq(newRepoTime(initialPred), newRepoTime(finalPred), Rot = FALSE)) 

pred_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//predictions_fileName"))
prevPred_parsedFileName <- ParseUnitName(pred_fileName, newRepoTime(periodsPred)) 
prevPred_parsedFileName <- ParseUnitName(prevPred_parsedFileName, paste0("00", batch))
prevPred_fullFileName   <- file.path(path_data_survey, prevPred_parsedFileName)

refPred_parsedFileName <- ParseUnitName(pred_fileName, newRepoTime(refPeriod)) 
refPred_parsedFileName <- ParseUnitName(refPred_parsedFileName, paste0("00", batch))
refPred_fullFileName   <- file.path(path_data_survey, refPred_parsedFileName)

errors_fileName     <- xml_text(xml_find_all(paramLocal.xml, "//files_info//errors_fileName"))
errors_parsedFileName <- ParseUnitName(errors_fileName, newRepoTime(refPeriod)) 
errors_parsedFileName <- ParseUnitName(errors_parsedFileName, paste0("00", batch))
errors_fullFileName <- file.path(path_data_survey, errors_parsedFileName)


historical_selectVars <- c(id_var, target_name)
predictions_selectVars <- c(id_var, "final_prediction", "predictions_model", "predictions_model0_t0")
error_selectVars <- c(group_var, "aggre_rmse")

date_refPer  <- as.Date(paste0(substr(refPeriod, 5, 8), '-', substr(refPeriod, 3, 4), "-01"), format = "%Y-%m-%d")
date_iniPred <- as.Date(paste0(substr(initialPred, 5, 8), '-', substr(initialPred, 3, 4), "-01"), format = "%Y-%m-%d")
date_finPred <- as.Date(paste0(substr(finalPred, 5, 8), '-', substr(finalPred, 3, 4), "-01"), format = "%Y-%m-%d")

#####                          :::::::::::::::                              #### 
#####                           CREATE LOG                                  ####
logFile <- create_log(path_log_survey, name_logFile = "log_compute_rmse", sink_log = TRUE)


#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

#####                        Read input data                                ####
historical.dt        <- readRDS(input_FD_fullFileName)
refPredictions.dt    <- readRDS(refPred_fullFileName)
prevPredictions.list <- lapply(prevPred_fullFileName, function(name){
  
  cat(name)
  
  return(readRDS(name))
  
})
prevPredictions.dt   <- rbindlist(prevPredictions.list)


#####                        Prepare data                                   ####
prevPredictions.dt[, (target_name) := NULL]
prevPredictions.dt <- merge(prevPredictions.dt, historical.dt[, ..historical_selectVars],
                            by = id_var, all.x = TRUE)

prevPredictions.dt[
  , target_na := is.na(get(collected_var))][
  , date := as.Date(paste0(year, "-", month, "-01"), format = "%Y-%m-%d")][
  , pred_error := predictions_model - get(target_name)][
  , meas_error := ifelse(is.na(get(collected_var)), -get(target_name), get(collected_var) - get(target_name))][
  , synth_error := ifelse(target_na, pred_error, meas_error)]

refPredictions.dt <- refPredictions.dt[, ..predictions_selectVars]


#####                    Unit-level approach                                ####
error.dt <- prevPredictions.dt[
  , aggre_rmse := eval(parse(text = aggre_rmse)), by = group_var][
  , ..error_selectVars]

error.dt <- unique(error.dt)


refPred_error.dt <- merge(refPredictions.dt, error.dt,
                          by = group_var,
                          all.x = TRUE)

#####                          :::::::::::::::                              #### 
#####                             SAVE RDS                                  ####

saveRDS(refPred_error.dt, errors_fullFileName)



###                            :::::::::::::::                              ####
#####                             CLOSE LOG                                #####
flog.info(msg = paste0(survey, '::: Closing log file.'), name = 'logFile')
close_log(logFile = logFile, sink_log = TRUE)


