# This script builds a data.table with the values of the longitudinal regressors for the prediction
# model for the current reference time period.
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
# The local output is an rds object containing a data.table with the values of the longitudinal regressors.
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

DDversion        <- xml_text(xml_find_all(paramLocal.xml, "//files_info//DDversion"))
historicalFFName <- xml_text(xml_find_all(paramLocal.xml, "//files_info//historical_FF_fileName"))
FF_rds_fullName  <- file.path(path_data_intermediate_survey, historicalFFName)

lagsFF           <- as.numeric(xml_text(xml_find_all(paramLocal.xml, "//time//lagsFF")))
initialFF        <- getRepo(newRepoTime(refPeriod) - months(lagsFF))
finalFF          <- getRepo(newRepoTime(refPeriod) - months(1))
periodsFF        <- getRepo(Seq(newRepoTime(initialFF), newRepoTime(finalFF), Rot = FALSE)) 
numRevisedFF     <- as.numeric(xml_text(xml_find_all(paramLocal.xml, "//time//numRevisedFF")))
revisedPeriodsFF <- getRepo(Seq(newRepoTime(finalFF) - months(numRevisedFF), newRepoTime(finalFF), Rot = FALSE)) 

thresh           <- as.numeric(xml_text(xml_children(xml_find_all(paramLocal.xml, "//execution//thresholds"))))

longReg_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//longitudinalRegressors_fileName"))
longReg_parsedFileName <- ParseUnitName(longReg_fileName, newRepoTime(refPeriod)) 
longReg_parsedFileName <- ParseUnitName(longReg_parsedFileName, paste0("00", batch))                                                 
longReg_fullFileName   <- file.path(path_data_intermediate_survey, longReg_parsedFileName)

xlsxName <- paste0(survey, '.NombresVariables_V', DDversion, '.xlsx')
ddName   <- paste0(survey, '.DD_V', DDversion)

#####                          :::::::::::::::                              #### 
#####                           CREATE LOG                                  ####
logFile <- create_log(path_log_survey, name_logFile = "log_longitudinalRegressors", sink_log = TRUE)

#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

####                          Read Historical FF                            ####
flog.info(msg = paste0(survey, '::: Read historical time series of FF files.'), name = 'logFile')
FF.dt <- readRDS(FF_rds_fullName)

#####                Read Data Dictionary and Repo Content                  ####
flog.info(msg = paste0(survey, '::: Read data dictionary and repo content.'), name = 'logFile')
VNC <- RepoXLSToVNC(file.path(path_repo_survey, xlsxName))
DD <- RepoDDToDD(file.path(path_repo_survey, ddName), VNC)
data_files <- list.files(path_repo_survey)

####                         Read and Prepare FD File                       ####
flog.info(msg = paste0(survey, '::: Read and prepare new FD files.'), name = 'logFile')
batch_num <- as.numeric(batch) 
FD_files_names <- data_files[
  intersect(grep(paste0('.D_', batch_num), data_files), 
            grep(paste0('.FD_V', DDversion), data_files))]
FDfileSpec.dt <- spec_repoFiles(FD_files_names)[
  period == refPeriod]
FDName <- FDfileSpec.dt$fileName

FD.StQ <- ReadRepoFile(file.path(path_repo_survey, FDName), DD, perl = TRUE)
FD.dt  <- dcast_StQ(FD.StQ, UnitNames = TRUE)
FD.dt  <- setup_FD(FD.dt, FF.dt, refPeriod)

#####                         Longitudinal Regressors                       ####
flog.info(msg = paste0(survey, '::: Build longitudinal regressors.'), name = 'logFile')
FD_reg.dt <- build_FD_longitudinal_regressors(FD_current.dt = FD.dt, thresholds = thresh)
FD_reg.dt[, batch := batch]

#####                          :::::::::::::::                              #### 
#####                             SAVE RDS                                  ####
flog.info(msg = paste0(survey, '::: Save longitudinal regressors in a data.table as an rds file.'), name = 'logFile')
saveRDS(FD_reg.dt, file = longReg_fullFileName)


###                            :::::::::::::::                              ####
#####                             CLOSE LOG                                #####
flog.info(msg = paste0(survey, '::: Close log file.'), name = 'logFile')
close_log(logFile = logFile, sink_log = TRUE)
