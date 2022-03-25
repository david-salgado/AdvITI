# This script builds a data.table with the historical final validated values of different survey data
# and paradata for each statistical unit (industrial establishment).
#
# Each execution is controlled by the survey code, the reference time period, the batch id code,
# the name of the xml file with local parameters for this survey and the root path of the project.
# These parameters are taken from the shell command executing this script.
#
# Global parameters (paths, packages and functions) are specified in another xml file for this survey.
#
# Paths follow the generic folder structure specified in the working paper.
#
# The local input parameters are the different elements for the names of data files in the
# central repository (data dictionary version, reference time periods in repo notation) and
# the accumulated rds filename storing the data.table with historical final validated values.
#
# The local output is an rds object containing a data.table with the historical final validated values.
#
#
#####                          :::::::::::::::                              #### 
#####                    SPECIFY PARAMETERS FROM SHELL                      ####
args <- commandArgs(trailingOnly = TRUE)
#args <- list("E30052", "MM052021", "01", "E30052.BuildRegressors_LocalParam.xml", "N:/UDMTD/UDTMDCOM/AdvITI_github")

#####                        Read arguments                                 ####
survey         <- as.character(args[1])
refPeriod      <- as.character(args[2])
batch          <- as.character(args[3])
localParamName <- as.character(args[4])
main_path      <- as.character(args[5])

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
paramGlobal.xml        <- read_xml(
  file.path(main_path, 
            '01-01.BuildRegressors/param/global/Ennnnn.BuildRegressors_GlobalParam.xml'))

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
paramLocal.xml   <- read_xml(file.path(path_param_local_survey, localParamName))

DDversion        <- xml_text(xml_find_all(paramLocal.xml, "//files_info//DDversion"))
historicalFFName <- xml_text(xml_find_all(paramLocal.xml, "//files_info//historical_FF_fileName"))
FF_rds_fullName  <- file.path(path_data_intermediate_survey, historicalFFName)

initialFF        <- xml_text(xml_find_all(paramLocal.xml, "//time//initialFF"))
finalFF          <- getRepo(newRepoTime(refPeriod) - months(1))
periodsFF        <- getRepo(Seq(newRepoTime(initialFF), newRepoTime(finalFF), Rot = FALSE)) 
numRevisedFF     <- as.numeric(xml_text(xml_find_all(paramLocal.xml, "//time//numRevisedFF")))
revisedPeriodsFF <- getRepo(Seq(newRepoTime(finalFF) - months(numRevisedFF), newRepoTime(finalFF), Rot = FALSE)) 


#####                          :::::::::::::::                              #### 
#####                           CREATE LOG                                  ####
logFile <- create_log(path_log_survey, name_logFile = "log_longitudinalFF", sink_log = TRUE)


#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

#####                        Read repo info                                 ####
flog.info(msg = paste0(survey, '::: Read data dictionary and repo content.'), name = 'logFile')
DD <- RepoXLSToDD(file.path(path_repo_survey, paste0(survey, '.NombresVariables_V', DDversion, '.xlsx')))
data_files     <- list.files(path_repo_survey)
FF_files_names <- data_files[grep(paste0('.FF_B15_V', DDversion), data_files)]
FFfileSpec.dt  <- spec_repoFiles(FF_files_names, last = TRUE)[
            period %in% periodsFF]

if(nrow(FFfileSpec.dt) == 0){
  
  stop("[01.build_longitudinalFF] There is no FF file to read.")
  
}

#####                        Read historical FF                             ####
# It checks if there is historical table, then the periods in the historical table are removed
if(file.exists(FF_rds_fullName)){
  flog.info(msg = paste0(survey, '::: Read historical time series of FF files.'), name = 'logFile')
  
  
  oldFF.dt <- readRDS(FF_rds_fullName)[
    !period %in% revisedPeriodsFF]
  
  FFfileSpec.dt <- FFfileSpec.dt[
    !period %in% oldFF.dt$period]
  
}

#####                        Update historical FF                           ####
# If there are still periods to be read the historical table is updated with them
if(nrow(FFfileSpec.dt) > 0){
  
  flog.info(msg = paste0(survey, '::: Read new FF files.'), name = 'logFile')
  FF_files_names <- FFfileSpec.dt$fileName
  
  FF.StQ.list <- lapply(FF_files_names, function(FFName){
    
    cat(FFName)
    
    FF.StQ <- ReadRepoFile(file.path(path_repo_survey, FFName), DD, perl = TRUE)
    return(FF.StQ)
    
    
  })
  names(FF.StQ.list) <- FF_files_names
  
  flog.info(msg = paste0(survey, '::: Prepare FF files as a combined data.table.'), name = 'logFile')
  # The setUpFF function belongs to the survey
  newFF.dt <- setup_FF(FF.StQ.list)
  
  flog.info(msg = paste0(survey, '::: Update historical time series of FF files.'), name = 'logFile')
  if (!exists('oldFF.dt')){
    
    oldFF.dt <- newFF.dt[0]
  }
  
  updatedFF.dt <- rbindlist(list(oldFF.dt, newFF.dt))
  
} else {
  
  updatedFF.dt <- oldFF.dt
  
}

#####                          :::::::::::::::                              #### 
#####                             SAVE RDS                                  ####
flog.info(msg = paste0(survey, '::: Save historical time series of FF files as an rds file.'), name = 'logFile')
saveRDS(updatedFF.dt, FF_rds_fullName)


###                            :::::::::::::::                              ####
#####                             CLOSE LOG                                #####
flog.info(msg = paste0(survey, '::: Close log file.'), name = 'logFile')
close_log(logFile = logFile, sink_log = TRUE)
