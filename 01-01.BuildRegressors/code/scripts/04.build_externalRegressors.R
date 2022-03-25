# This script builds a data.table with the values of the regressors obtained from external sources
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
# the accumulated rds filename storing the data.table with the current period regressors.
#
# The local output is an rds object containing a data.table with the values of the mentioned regressors.
#
#####                          :::::::::::::::                              #### 
#####                    SPECIFY PARAMETERS FROM SHELL                      ####
args <- commandArgs(trailingOnly = TRUE)
#args <- list("E30052", "MM032016", "01", "E30052.BuildRegressors_LocalParam.xml", "N:/UDMTD/UDTMDCOM/AdvITI_github")

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

curReg_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//currentRegressors_fileName"))
curReg_parsedFileName <- ParseUnitName(curReg_fileName, newRepoTime(refPeriod)) 
curReg_parsedFileName <- ParseUnitName(curReg_parsedFileName, paste0("00",batch))                                                 
curReg_fullFileName   <- file.path(path_data_intermediate_survey, curReg_parsedFileName)

extReg_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//externalRegressors_fileName"))
extReg_parsedFileName <- ParseUnitName(extReg_fileName, newRepoTime(refPeriod)) 
extReg_parsedFileName <- ParseUnitName(extReg_parsedFileName, paste0("00",batch))                                                 
extReg_fullFileName   <- file.path(path_data_intermediate_survey, extReg_parsedFileName)

paths_external_names <- ls()[grep("path_external", ls())]
if(length(paths_external_names) > 0){
  paths_external <- sapply(paths_external_names, function(pth){eval(get(pth))})
  names(paths_external) <- str_split_fixed(names(paths_external), pattern = "\\.", n = 2)[, 2]
}

#####                          :::::::::::::::                              #### 
#####                           CREATE LOG                                  ####
logFile <- create_log(path_log_survey, name_logFile = "log_externalRegressors", sink_log = TRUE)

#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

#####                            Read Data                                  ####
FD_longCurrent.dt <- readRDS(curReg_fullFileName)

if(length(paths_external_names) > 0){
  
  external_data.list <- read_external_data(paths_external)
  
}

#####                       Build External Regressors                       ####
if(length(paths_external_names) > 0){
  
  FD_ext.dt <- build_FD_external_regressors(FD_longCurrent.dt, external_data.list)
  
}else{
  
  FD_ext.dt <- FD_longCurrent.dt
  
}

#####                          :::::::::::::::                              #### 
#####                             SAVE RDS                                  ####
saveRDS(FD_ext.dt, file = extReg_fullFileName)


###                            :::::::::::::::                              ####
#####                             CLOSE LOG                                #####
flog.info(msg = paste0(survey, '::: Closing log file.'), name = 'logFile')
close_log(logFile = logFile, sink_log = TRUE)
