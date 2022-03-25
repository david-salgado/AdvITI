#####                          :::::::::::::::                              #### 
#####                    SPECIFY PARAMETERS FROM SHELL                      ####
args <- commandArgs(trailingOnly = TRUE)
#args <- list("E30052", "MM112016", "FF", "E30052.ComputeAggregates_LocalParam.xml", "N:/UDMTD/UDTMDCOM/AdvITI_github")

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

valueBatch <- ifelse(batch == "FF", "00", batch)

aggreTrue_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//aggregate_true_fileName"))
aggreTrue_parsedFileName <- ParseUnitName(aggreTrue_fileName, refPeriod.RTI) 
aggreTrue_parsedFileName <- ParseUnitName(aggreTrue_parsedFileName, paste0("00", batch))                                                 
aggreTrue_fullFileName   <- file.path(path_data_survey, aggreTrue_parsedFileName)

aggreIndexTrue_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//aggregate_index_true_fileName"))
aggreIndexTrue_parsedFileName <- ParseUnitName(aggreIndexTrue_fileName, refPeriod.RTI) 
aggreIndexTrue_parsedFileName <- ParseUnitName(aggreIndexTrue_parsedFileName, paste0("00", batch))                                                 
aggreIndexTrue_fullFileName   <- file.path(path_data_intermediate_survey, aggreIndexTrue_parsedFileName)

aggreIndexTrue_1_parsedFileName <- ParseUnitName(aggreIndexTrue_fileName, precPeriod1.RTI) 
aggreIndexTrue_1_parsedFileName <- ParseUnitName(aggreIndexTrue_1_parsedFileName, paste0("00", batch))                                                 
aggreIndexTrue_1_fullFileName   <- file.path(path_data_intermediate_survey, aggreIndexTrue_1_parsedFileName)

aggreIndexTrue_12_parsedFileName <- ParseUnitName(aggreIndexTrue_fileName, precPeriod12.RTI) 
aggreIndexTrue_12_parsedFileName <- ParseUnitName(aggreIndexTrue_12_parsedFileName, paste0("00", batch))                                                 
aggreIndexTrue_12_fullFileName   <- file.path(path_data_intermediate_survey, aggreIndexTrue_12_parsedFileName)

sampleSize_fileName     <- xml_text(xml_find_all(paramLocal.xml, "//files_info//sampleSize_fileName"))
sampleSize_fullFileName <- file.path(path_indicators_survey, sampleSize_fileName)


target_name   <- xml_text(xml_find_all(paramLocal.xml, "//execution//target_var"))
id_var        <- xml_text(xml_find_all(paramLocal.xml, "//execution//id_var"))
id_var        <- trimws(strsplit(id_var, ",")[[1]])
collected_var <- xml_text(xml_find_all(paramLocal.xml, "//execution//collected_var"))

#####                          :::::::::::::::                              #### 
#####                           CREATE LOG                                  ####
logFile <- create_log(path_log_survey, name_logFile = "log_aggre_true", sink_log = TRUE)



#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

#####                        Read input data                                ####

if(rates1){
  composed_index_1.dt  <- readRDS(aggreIndexTrue_1_fullFileName)
}
if(rates12){
  composed_index_12.dt <- readRDS(aggreIndexTrue_12_fullFileName)
}


#####                        Prepare data                                   ####


microdata.list <- prepare_validatedData(
  collected_var = collected_var,
  refPeriod = refPeriod,
  repoInfo = list(DDversion = DDversion, path_repo = path_repo_survey))


#####                      Compute aggregates                               ####

index.dt <- calculate_elementary_index_batch(microdata.list,
                                             collected_var, 
                                             refPeriod, batch = valueBatch)

composed_index.dt <- calculate_composed_index_batch(all_elemental_index = index.dt,
                                                    FF_previous.StQ = microdata.list$FF_previous.StQ,
                                                    collected_var, refPeriod, batch = valueBatch)

if(rates1 & !rates12){
  
  
  list_complete_rates.dt <- calculate_rate_of_change_batch(data_refPeriod.dt = composed_index.dt,
                                                           data_precPeriod1.dt = composed_index_1.dt,
                                                           annual = FALSE, batch = valueBatch)
  
}
if(rates1 & rates12){
  
  list_complete_rates.dt <- calculate_rate_of_change_batch(data_refPeriod.dt = composed_index.dt,
                                                           data_precPeriod1.dt = composed_index_1.dt,
                                                           data_precPeriod12.dt = composed_index_12.dt,
                                                           annual = TRUE, batch = valueBatch)
}


composed_index.dt      <- composed_index.dt[, batch := batch]

if(rates1){
  
  list_complete_rates.dt <- list_complete_rates.dt[, batch := batch]

}

#####                          :::::::::::::::                              #### 
#####                             SAVE RDS                                  ####


saveRDS(composed_index.dt, aggreIndexTrue_fullFileName)

if(rates1){
  
  saveRDS(list_complete_rates.dt, aggreTrue_fullFileName)  
  
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



