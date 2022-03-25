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

historicalFDName <- xml_text(xml_find_all(paramLocal.xml, "//files_info//historical_FD_fileName"))
FD_rds_fullName  <- file.path(path_data_intermediate_survey, historicalFDName)

initialFD <- xml_text(xml_find_all(paramLocal.xml, "//time//initialFD"))
periodsFD <- getRepo(Seq(newRepoTime(initialFD), newRepoTime(refPeriod), Rot = FALSE)) 

extReg_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//externalRegressors_fileName"))
extReg_parsedFileName <- ParseUnitName(extReg_fileName, newRepoTime(refPeriod)) 
extReg_parsedFileName <- ParseUnitName(extReg_parsedFileName, paste0("00", batch))                                                 
extReg_fullFileName   <- file.path(path_data_intermediate_survey, extReg_parsedFileName)

extReg_rootFileName <- str_split_fixed(extReg_fileName, pattern = "\\.", n = Inf)[, 1]

#####                          :::::::::::::::                              #### 
#####                           CREATE LOG                                  ####
logFile <- create_log(path_log_survey, name_logFile = "log_longitudinalFD", sink_log = TRUE)



#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

#####                            Read Data                                  ####

data_files <- list.files(path_data_intermediate_survey)


#### FD files ####

FD_files_names <- data_files[grep(extReg_rootFileName, data_files)]
FDfileSpec.dt <- data.table(fileName = FD_files_names)[
          , period := tstrsplit(fileName, split = '.', fixed = TRUE)[[2]]][
            , batch_dt := tstrsplit(fileName, split = '.', fixed = TRUE)[[3]]][
              , numBatch := gsub("batch", "", batch_dt)][
            period %in% periodsFD]

#####                        Read historical FD                             ####

# It checks if there is historical table, then the periods in the historical table are removed
if(file.exists(FD_rds_fullName)){
  
  oldFD.dt <- readRDS(FD_rds_fullName)
  periods_oldFD <- unique(paste0("MM", oldFD.dt$month, oldFD.dt$year))
  FDfileSpec.dt <- FDfileSpec.dt[!((period %in% periods_oldFD) & (numBatch %in% oldFD.dt$batch))]
  
}

#####                        Update historical FD                           ####
# If there are still periods to be read the historical table is updated with them
if(nrow(FDfileSpec.dt) > 0){
  
  FD_files_names <- FDfileSpec.dt$fileName
  
  FD.dt.list <- lapply(FD_files_names, function(FDName){
    
    cat(FDName)
    
    return(readRDS(file.path(path_data_intermediate_survey, FDName)))
    
  })
  
  FD.dt <- rbindlist(FD.dt.list)
  
  if (!exists('oldFD.dt')) oldFD.dt <- FD.dt[0]
  
  updatedFD.dt <- rbindlist(list(oldFD.dt, FD.dt))
  
}else{
  
  updatedFD.dt <- oldFD.dt
  
}


#####                          :::::::::::::::                              #### 
#####                             SAVE RDS                                  ####

saveRDS(updatedFD.dt, FD_rds_fullName)


###                            :::::::::::::::                              ####
#####                             CLOSE LOG                                #####
flog.info(msg = paste0(survey, '::: Closing log file.'), name = 'logFile')
close_log(logFile = logFile, sink_log = TRUE)
