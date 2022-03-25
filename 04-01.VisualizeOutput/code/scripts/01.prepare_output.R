#####                          :::::::::::::::                              #### 
#####                    SPECIFY PARAMETERS FROM SHELL                      ####
# args <- commandArgs(trailingOnly = TRUE)
args <- list("E30052", "MM042021", "01", "E30052.VisualizeOutput_LocalParam.xml", "N:/UDMTD/UDTMDCOM/AdvITI_github")

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
                                             '04-01.VisualizeOutput/param/global/Ennnnn.VisualizeOutput_GlobalParam.xml'))

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
initialRates  <- xml_text(xml_find_all(paramLocal.xml, "//time//initialRates"))
all_batches   <- xml_text(xml_find_all(paramLocal.xml, "//time//all_batches"))
all_batches   <- trimws(strsplit(all_batches, ",")[[1]])


if(int_end(RepoTimeTolubri(refPeriod)[[1]]) < int_end(RepoTimeTolubri(initialRates)[[1]])){
  stop("More periods are needed for doing hyperparameters search.")
}

refPeriod.RTI <- newRepoTime(refPeriod)
initialRates.RTI <- newRepoTime(initialRates)

periods <- getRepo(Seq(initialRates.RTI, refPeriod.RTI, Rot = FALSE))
periods.RTI <- newRepoTime(periods)


aggreEstim_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//aggregate_estimation_fileName"))
aggreEstim_parsedFileName <- ParseUnitName(aggreEstim_fileName, periods.RTI) 
aggreEstim_parsedFileName <- ParseUnitName(aggreEstim_parsedFileName, paste0("00", all_batches))                                                 
aggreEstim_fullFileName   <- file.path(path_data_survey, aggreEstim_parsedFileName)

aggreEstim_fileName         <- xml_text(xml_find_all(paramLocal.xml, "//files_info//aggregate_estimation_fileName"))
aggreEstim00_parsedFileName <- ParseUnitName(aggreEstim_fileName, periods.RTI) 
aggreEstim00_parsedFileName <- ParseUnitName(aggreEstim00_parsedFileName, paste0("0000"))                                                 
aggreEstim00_fullFileName   <- file.path(path_data_survey, aggreEstim00_parsedFileName)

aggreTrue_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//aggregate_true_fileName"))
aggreTrue_parsedFileName <- ParseUnitName(aggreTrue_fileName, periods.RTI) 
aggreTrue_parsedFileName <- ParseUnitName(aggreTrue_parsedFileName, paste0("00", "FF"))                                                 
aggreTrue_fullFileName   <- file.path(path_data_survey, aggreTrue_parsedFileName)

outputIndex_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//all_index_fileName"))
outputIndex_fullFileName   <- file.path(path_data_intermediate_survey, outputIndex_fileName)
outputAnnual_fileName      <- xml_text(xml_find_all(paramLocal.xml, "//files_info//all_annualRates_fileName"))
outputAnnual_fullFileName  <- file.path(path_data_intermediate_survey, outputAnnual_fileName)
outputMonthly_fileName     <- xml_text(xml_find_all(paramLocal.xml, "//files_info//all_monthlyRates_fileName"))
outputMonthly_fullFileName <- file.path(path_data_intermediate_survey, outputMonthly_fileName)


outputIndexSDC_fileName       <- xml_text(xml_find_all(paramLocal.xml, "//files_info//all_index_SDC_fileName"))
outputIndexSDC_fullFileName   <- file.path(path_shiny, outputIndexSDC_fileName)
outputAnnualSDC_fileName      <- xml_text(xml_find_all(paramLocal.xml, "//files_info//all_annualRates_SDC_fileName"))
outputAnnualSDC_fullFileName  <- file.path(path_shiny, outputAnnualSDC_fileName)
outputMonthlySDC_fileName     <- xml_text(xml_find_all(paramLocal.xml, "//files_info//all_monthlyRates_SDC_fileName"))
outputMonthlySDC_fullFileName <- file.path(path_shiny, outputMonthlySDC_fileName)


#####                          :::::::::::::::                              #### 
#####                           CREATE LOG                                  ####
logFile <- create_log(path_log_survey, name_logFile = "log_prepare_output", sink_log = TRUE)



#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

#####                        Read input data                                ####

estimation00.list <- lapply(aggreEstim00_fullFileName, function(x){  
  out <- readRDS(x)
  out <- out[, c("sub", "agrupacion", "index", "batch", "refPeriod", 
               "index_monthly_rate", "index_annual_rate"), with = FALSE]
return(out)
})
estimation.list <- lapply(aggreEstim_fullFileName, function(x){
  out <- readRDS(x)
  out <- out[, c("sub", "agrupacion", "index", "batch", "refPeriod", 
                 "index_monthly_rate", "index_annual_rate",
                 "index_rmse", "index_monthly_rate_rmse", "index_annual_rate_rmse"), with = FALSE]
  return(out)
})
true.list       <- lapply(aggreTrue_fullFileName, function(x){readRDS(x)})


#####                        Prepare data                                   ####

estimation.dt <- rbindlist(estimation.list)[, indexVersion := "batch"]
estimation00.dt <- rbindlist(estimation00.list)[, indexVersion := "initial"][, batch := NULL]
true.dt <- rbindlist(true.list)[, indexVersion := "final"][, batch := NULL]


estimation00_batches.list <- lapply(all_batches, function(x){
  print(x)
  out.dt <- copy(estimation00.dt)[, batch := x]
  return(out.dt)
  })
estimation00_batches.dt <- rbindlist(estimation00_batches.list)

true_batches.list <- lapply(all_batches, function(x){
  print(x)
  out.dt <- copy(true.dt)[, batch := x]
  return(out.dt)
})
true_batches.dt <- rbindlist(true_batches.list)


all_index.dt <- merge(estimation.dt[, c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index", "index_rmse"), with = FALSE], 
                      estimation00_batches.dt[, c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index"), with = FALSE],
                      by = c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index"), all = TRUE)
all_index.dt <- merge(all_index.dt, 
                      true_batches.dt[, c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index"), with = FALSE],
                      by = c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index"), all = TRUE)

setnames(all_index.dt, "index_rmse", "rmse")
all_index.dt <- all_index.dt[
  indexVersion %in% c("initial", "final"), rmse := 0][
  , batch := paste("batch", batch)][
  , date := as.Date(paste0(substr(refPeriod, 5, 8), '-', substr(refPeriod, 3, 4), "-01"), format = "%Y-%m-%d")][
  , refPeriod := NULL]



all_annual.dt <- merge(estimation.dt[, c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index_annual_rate", "index_annual_rate_rmse"), with = FALSE], 
                       estimation00_batches.dt[, c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index_annual_rate"), with = FALSE],
                       by = c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index_annual_rate"), all = TRUE)
all_annual.dt <- merge(all_annual.dt, 
                       true_batches.dt[, c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index_annual_rate"), with = FALSE],
                       by = c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index_annual_rate"), all = TRUE)
setnames(all_annual.dt, c("index_annual_rate", "index_annual_rate_rmse"), c("annual_rate", "rmse"))
all_annual.dt <- all_annual.dt[
  indexVersion %in% c("initial", "final"), rmse := 0][
  , batch := paste("batch", batch)][
  , date := as.Date(paste0(substr(refPeriod, 5, 8), '-', substr(refPeriod, 3, 4), "-01"), format = "%Y-%m-%d")][
  , refPeriod := NULL]


all_monthly.dt <- merge(estimation.dt[, c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index_monthly_rate", "index_monthly_rate_rmse"), with = FALSE], 
                        estimation00_batches.dt[, c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index_monthly_rate"), with = FALSE],
                        by = c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index_monthly_rate"), all = TRUE)
all_monthly.dt <- merge(all_monthly.dt, 
                        true_batches.dt[, c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index_monthly_rate"), with = FALSE],
                        by = c("sub", "agrupacion", "refPeriod", "batch", "indexVersion", "index_monthly_rate"), all = TRUE)

setnames(all_monthly.dt, c("index_monthly_rate", "index_monthly_rate_rmse"), c("monthly_rate", "rmse"))
all_monthly.dt <- all_monthly.dt[
  indexVersion %in% c("initial", "final"), rmse := 0][
  , batch := paste("batch", batch)][
  , date := as.Date(paste0(substr(refPeriod, 5, 8), '-', substr(refPeriod, 3, 4), "-01"), format = "%Y-%m-%d")][
  , refPeriod := NULL]


#####                   Statistical Disclosure Control                      ####


all_index_SDC.dt <- copy(all_index.dt)[
  agrupacion == "div_subdiv" & sub %in% c("05", "06", "07"), index := NA][
  agrupacion == "division" & sub %in% c("05", "06", "07"), index := NA][
  agrupacion == "div_subdiv" & sub %in% c("05", "06", "07"), rmse := NA][
  agrupacion == "division" & sub %in% c("05", "06", "07"), rmse := NA]
all_annual_SDC.dt <- copy(all_annual.dt)[
  agrupacion == "div_subdiv" & sub %in% c("05", "06", "07"), index := NA][
  agrupacion == "division" & sub %in% c("05", "06", "07"), index := NA][
  agrupacion == "div_subdiv" & sub %in% c("05", "06", "07"), rmse := NA][
  agrupacion == "division" & sub %in% c("05", "06", "07"), rmse := NA]
all_monthly_SDC.dt <- copy(all_monthly.dt)[
  agrupacion == "div_subdiv" & sub %in% c("05", "06", "07"), index := NA][
  agrupacion == "division" & sub %in% c("05", "06", "07"), index := NA][
  agrupacion == "div_subdiv" & sub %in% c("05", "06", "07"), rmse := NA][
  agrupacion == "division" & sub %in% c("05", "06", "07"), rmse := NA]


#####                          :::::::::::::::                              #### 
#####                             SAVE RDS                                  ####


saveRDS(all_index.dt, outputIndex_fullFileName)
saveRDS(all_annual.dt, outputAnnual_fullFileName)
saveRDS(all_monthly.dt, outputMonthly_fullFileName)

saveRDS(all_index_SDC.dt, outputIndexSDC_fullFileName)
saveRDS(all_annual_SDC.dt, outputAnnualSDC_fullFileName)
saveRDS(all_monthly_SDC.dt, outputMonthlySDC_fullFileName)



###                            :::::::::::::::                              ####
#####                             CLOSE LOG                                #####
flog.info(msg = paste0(survey, '::: Closing log file.'), name = 'logFile')
close_log(logFile = logFile, sink_log = TRUE)



