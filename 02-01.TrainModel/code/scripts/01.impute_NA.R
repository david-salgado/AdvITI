#####                          :::::::::::::::                              #### 
#####                    SPECIFY PARAMETERS FROM SHELL                      ####
args <- commandArgs(trailingOnly = TRUE)
args <- list("E30052", "MM042021", "01", "E30052.TrainModel_LocalParam.xml", "N:/UDMTD/UDTMDCOM/AdvITI_github")

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
                                             '02-01.TrainModel/param/global/Ennnnn.TrainModel_GlobalParam.xml'))

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

inputFD_fileName <- xml_text(xml_find_all(paramLocal.xml, "//files_info//target_FD_fileName"))
inputFD_fullFileName <- file.path(path_data_survey, inputFD_fileName)

outputFD_fileName <- xml_text(xml_find_all(paramLocal.xml, "//files_info//historical_FD_woNA_fileName"))
outputFD_fullfileName <- file.path(path_data_intermediate_survey, outputFD_fileName)

id_var      <- xml_text(xml_find_all(paramLocal.xml, "//execution//id_var"))
id_var      <- trimws(strsplit(id_var, ",")[[1]])

#####                          :::::::::::::::                              #### 
#####                           CREATE LOG                                  ####
logFile <- create_log(path_log_survey, name_logFile = "log_dealing_with_NA", sink_log = TRUE)

#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

#####                        Read input data                                ####


dt <- readRDS(inputFD_fullFileName)


#####                  Deal with NA values                                  ####

woNA.dt <- impute_NA(dt)

#####               Check for duplicates                                    ####

dupli.dt <- woNA.dt[duplicated(woNA.dt, by = id_var)]
if(nrow(dupli.dt) > 0){
  stop("There is some duplicates by variables in id_var.")
}

#####                          :::::::::::::::                              #### 
#####                             SAVE RDS                                  ####

saveRDS(woNA.dt, outputFD_fullfileName)


###                            :::::::::::::::                              ####
#####                             CLOSE LOG                                #####
flog.info(msg = paste0(survey, '::: Closing log file.'), name = 'logFile')
close_log(logFile = logFile, sink_log = TRUE)



