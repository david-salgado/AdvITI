# This script builds a data.table with the values of the current reference time period regressors 
# for the prediction model.
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
# the accumulated rds filename storing the data.table with longitudinal regressors.
#
# The local output is an rds object containing a data.table with the values of the current period regressors.
#
#####                          :::::::::::::::                              #### 
#####                    SPECIFY PARAMETERS FROM SHELL                      ####
args <- commandArgs(trailingOnly = TRUE)
#args <- list("E30052", "MM022016", "01", "E30052.BuildRegressors_LocalParam.xml", "N:/UDMTD/UDTMDCOM/AdvITI_github")

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
paramGlobal.xml <- read_xml(
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

longReg_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//longitudinalRegressors_fileName"))
longReg_parsedFileName <- ParseUnitName(longReg_fileName, newRepoTime(refPeriod)) 
longReg_parsedFileName <- ParseUnitName(longReg_parsedFileName, paste0("00",batch))                                                 
longReg_fullFileName   <- file.path(path_data_intermediate_survey, longReg_parsedFileName)

curReg_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//currentRegressors_fileName"))
curReg_parsedFileName <- ParseUnitName(curReg_fileName, newRepoTime(refPeriod)) 
curReg_parsedFileName <- ParseUnitName(curReg_parsedFileName, paste0("00",batch))                                                 
curReg_fullFileName   <- file.path(path_data_intermediate_survey, curReg_parsedFileName)

#####                          :::::::::::::::                              #### 
#####                           CREATE LOG                                  ####
logFile <- create_log(path_log_survey, name_logFile = "log_currentRegressors", sink_log = TRUE)


#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

#####                            Read Data                                  ####
FD_long.dt <- readRDS(longReg_fullFileName)


#####                      Current Regressors                               ####
FD_current.dt <- build_FD_current_regressors(FD_long.dt)


#####                          :::::::::::::::                              #### 
#####                             SAVE RDS                                  ####
saveRDS(FD_current.dt, file = curReg_fullFileName)


###                            :::::::::::::::                              ####
#####                             CLOSE LOG                                #####
flog.info(msg = paste0(survey, '::: Closing log file.'), name = 'logFile')
close_log(logFile = logFile, sink_log = TRUE)

