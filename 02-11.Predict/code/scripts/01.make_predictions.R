#####                          :::::::::::::::                              #### 
#####                    SPECIFY PARAMETERS FROM SHELL                      ####
args <- commandArgs(trailingOnly = TRUE)
#args <- list("E30052", "MM042016", "01", "E30052.Predict_LocalParam.xml", "N:/UDMTD/UDTMDCOM/AdvITI_github")

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
                                             '02-11.Predict/param/global/Ennnnn.Predict_GlobalParam.xml'))


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
refPeriod.RTI <- newRepoTime(refPeriod)

predictions_fileName     <- xml_text(xml_find_all(paramLocal.xml, "//files_info//predictions_fileName"))
predictions_parsedFileName <- ParseUnitName(predictions_fileName, refPeriod.RTI) 
predictions_parsedFileName <- ParseUnitName(predictions_parsedFileName, paste0("00", batch))                                                 
predictions_fullFileName <- file.path(path_data_survey, predictions_parsedFileName)

save_model_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//model_fileName"))
save_model_parsedfileName <- ParseUnitName(save_model_fileName, lmtrain.RTI)
save_model_fullFileName   <- file.path(path_data_survey, save_model_parsedfileName)

save_model0_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//model_t0_fileName"))
save_model0_parsedfileName <- ParseUnitName(save_model0_fileName, lmtrain.RTI)
save_model0_fullFileName   <- file.path(path_data_survey, save_model0_parsedfileName)

type_model <- str_split(str_split_fixed(save_model_fileName, "\\.", n = 3)[1, 1], "_")[[1]][2]

target_name           <- xml_text(xml_find_all(paramLocal.xml, "//execution//target_var"))
id_var                <- xml_text(xml_find_all(paramLocal.xml, "//execution//id_var"))
id_var                <- trimws(strsplit(id_var, ",")[[1]])
collected_var         <- xml_text(xml_find_all(paramLocal.xml, "//execution//collected_var"))
varsToRemove          <- xml_text(xml_find_all(paramLocal.xml, "//execution//varsToRemove"))
varsToRemove          <- trimws(strsplit(varsToRemove, ",")[[1]])
varsToRemove_current  <- xml_text(xml_find_all(paramLocal.xml, "//execution//varsToRemove_current"))
varsToRemove_current  <- trimws(strsplit(varsToRemove_current, ",")[[1]])

valueBatch <- batch

####                          :::::::::::::::                              #### 
#####                           CREATE LOG                                  ####
logFile <- create_log(path_log_survey, name_logFile = "log_make_predictions", sink_log = TRUE)



#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

#####                        Read input data                                ####


dt <- readRDS(input_FD_fullFileName)

#####                        Read trained models                            ####

models <- load_model(list(save_model_fullFileName, save_model0_fullFileName),
                     type = type_model)

model_lmt  <- models[[1]]
model0_lmt <- models[[2]]

#####                        Prepare data                                   ####

dt[, date := as.Date(paste0(year, "-", month, "-01"), format = "%Y-%m-%d")] 

varsToRemove         <- c('date', target_name, varsToRemove)
allVarstoRemove      <- intersect(varsToRemove, colnames(dt))
allVarstoKeep        <- setdiff(names(dt), allVarstoRemove)
allVarstoKeepCurrent <- setdiff(allVarstoKeep, varsToRemove_current)

date_refPer     <- as.Date(paste0(substr(refPeriod, 5, 8), '-', substr(refPeriod, 3, 4), "-01"), format = "%Y-%m-%d")
test.dt         <- dt[date == date_refPer][batch == valueBatch]
test.matrix  <- data.matrix(test.dt[ , ..allVarstoKeep])
test0.matrix <- data.matrix(test.dt[ , ..allVarstoKeepCurrent])

#####                      Make predictions                                 ####


test.dt[, predictions_model := predict(model_lmt, test.matrix)]
test.dt[, predictions_model0_t0 := predict(model0_lmt, test0.matrix)]

test.dt       <- setup_predictions(test.dt, prediction_name = "predictions_model")
pred_final.dt <- setup_predictions_t0(test.dt, prediction_name = "predictions_model0_t0")


#####                          :::::::::::::::                              #### 
#####                             SAVE RDS                                  ####

saveRDS(pred_final.dt, predictions_fullFileName)


###                            :::::::::::::::                              ####
#####                             CLOSE LOG                                #####
flog.info(msg = paste0(survey, '::: Closing log file.'), name = 'logFile')
close_log(logFile = logFile, sink_log = TRUE)




