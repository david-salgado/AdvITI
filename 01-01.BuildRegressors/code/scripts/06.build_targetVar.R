# This script builds a data.table with the historical values of different survey data
# and paradata for each statistical unit (industrial establishment) obtained during the execution
# of the statistical data editing strategy.
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
# the accumulated rds filename storing the data.table with the external and current period regressors.
#
# The local output is an rds object containing a data.table with the values of the abovementioned regressors.
#
#####                          :::::::::::::::                              #### 
#####                    SPECIFY PARAMETERS FROM SHELL                      ####
args <- commandArgs(trailingOnly = TRUE)
#args <- list("E30052", "MM042021", "03", "E30052.BuildRegressors_LocalParam.xml", "N:/UDMTD/UDTMDCOM/AdvITI_github")

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

historicalFDName <- xml_text(xml_find_all(paramLocal.xml, "//files_info//historical_FD_fileName"))
FD_rds_fullName  <- file.path(path_data_intermediate_survey, historicalFDName)

targetFDName           <- xml_text(xml_find_all(paramLocal.xml, "//files_info//target_FD_fileName"))
targetFD_rds_fullName  <- file.path(path_data_survey, targetFDName)

historicalFFName <- xml_text(xml_find_all(paramLocal.xml, "//files_info//historical_FF_fileName"))
FF_rds_fullName  <- file.path(path_data_intermediate_survey, historicalFFName)

initialFD          <- xml_text(xml_find_all(paramLocal.xml, "//time//initialFD"))
periodsFD          <- getRepo(Seq(newRepoTime(initialFD), newRepoTime(refPeriod), Rot = FALSE)) 
prevRefPeriod      <- getRepo(newRepoTime(refPeriod) - months(1))
prevRefPeriod.date <- as.Date(paste0(substr(prevRefPeriod, 5, 8), '-', substr(prevRefPeriod, 3, 4), "-01"), format = "%Y-%m-%d")

target_name <- xml_text(xml_find_all(paramLocal.xml, "//execution//target_var"))
id_var      <- xml_text(xml_find_all(paramLocal.xml, "//execution//id_var"))
id_var      <- trimws(strsplit(id_var, ",")[[1]])

#####                          :::::::::::::::                              #### 
#####                           CREATE LOG                                  ####
logFile <- create_log(path_log_survey, name_logFile = "log_longitudinalFD", sink_log = TRUE)



#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

#####                            Read Data                                  ####

FD.dt <- readRDS(FD_rds_fullName)
FF.dt <- readRDS(FF_rds_fullName)


#####               Build target variable                                   ####

FD_target.dt <- build_target_variable(FD.dt, 
                                      FF.dt, 
                                      prevRefPeriod.date,
                                      target_name,
                                      id_var)

#####               Check for duplicates                                    ####

dupli.dt <- FD_target.dt[duplicated(FD_target.dt, by = id_var)]
if(nrow(dupli.dt) > 0){
  stop("There is some duplicates by variables in id_var.")
}

#####                          :::::::::::::::                              #### 
#####                             SAVE RDS                                  ####

saveRDS(FD_target.dt, targetFD_rds_fullName)


###                            :::::::::::::::                              ####
#####                             CLOSE LOG                                #####
flog.info(msg = paste0(survey, '::: Closing log file.'), name = 'logFile')
close_log(logFile = logFile, sink_log = TRUE)
