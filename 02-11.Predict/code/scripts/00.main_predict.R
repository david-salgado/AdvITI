
#####                          :::::::::::::::                              #### 
#####                             PACKAGES                                  ####

library(RepoTime)

#####                          :::::::::::::::                              #### 
#####                             PATHS                                     ####

#Rscript <- normalizePath("C:/Users/u852245/Documents/R/R-4.1.1/bin/Rscript.exe")
Rscript <- normalizePath("C:/R/R-4.1.1/bin/x64/Rscript.exe")


main_path <- "N:/UDMTD/UDTMDCOM/AdvITI_github"

# There is 1 script to be run to build different data sets:
#   - the predictions for all the units without response (01.make_predictions)

path_scripts <- file.path(main_path, "02-11.Predict/code/scripts")
current_script_predict  <- normalizePath(file.path(path_scripts, "01.make_predictions.R"))

#####                          :::::::::::::::                              #### 
#####                             PARAMS                                    ####

# We need to specify the following parameters:
#   - survey: the code of the survey
#   - iniPeriod.RTI: the initial reference period of analysis in RepoTimeInt S4 class
#   - finalPeriod.RTI: the final reference period of analysis in RepoTimeInt S4 class
#   - batches: the id code of each data processing batch 
#   - paramFileName: the name of the local parameters file to build the regressors

survey <- "E30052"
iniPeriod.RTI <- newRepoTime('MM042016')
finalPeriod.RTI <- newRepoTime('MM042021')
periods <- getRepo(Seq(iniPeriod.RTI, finalPeriod.RTI, Rot = FALSE))
#periods <- periods[-length(periods)]
batches <- c("00", "01", "02", "03")
paramFileName <- "E30052.Predict_LocalParam.xml"


#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

# This execution runs sequentially the scripts described before for each specified period and batch.

for(refPeriod in periods){
  
  cat(paste0(refPeriod, ": "))
  
  for(batch in batches){
    
    cat(batch)
    
    args <- paste(survey, refPeriod, batch, paramFileName, main_path)
    
    shell(cmd = paste(Rscript, current_script_predict, args), wait = TRUE)
    
    cat(", ")
    
  }
  
  cat("\n")
  
}



