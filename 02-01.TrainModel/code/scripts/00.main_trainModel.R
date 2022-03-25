
#####                          :::::::::::::::                              #### 
#####                             PACKAGES                                  ####

library(RepoTime)

#####                          :::::::::::::::                              #### 
#####                             PATHS                                     ####

#Rscript <- normalizePath("C:/Users/u852245/Documents/R/R-4.1.1/bin/Rscript.exe")
Rscript <- normalizePath("C:/R/R-4.1.1/bin/x64/Rscript.exe")


main_path <- "N:/UDMTD/UDTMDCOM/AdvITI_github"

# There are 4 scripts to be run to build different data sets:
#   - the result after dealing with NA values in the regressors (01.dealing_with_NA)
#   - the encoding of the regressors (02.encoding_regressors.R)
#   - the hyperparameters grid search to find the best parameters of the model (03.grid_search.R)
#   - the model after doing the training with the data preprocessed in previous scripts (04.train_model.R)


path_scripts <- file.path(main_path, "02-01.TrainModel/code/scripts")
current_script_NA  <- normalizePath(file.path(path_scripts, "01.impute_NA.R"))
current_script_encode <- normalizePath(file.path(path_scripts, "02.encode_regressors.R"))
current_script_search  <- normalizePath(file.path(path_scripts, "03.search_hyperparam.R"))
current_script_train  <- normalizePath(file.path(path_scripts, "04.train_model.R"))


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
batches <- c("01", "02", "03")
paramFileName <- "E30052.TrainModel_LocalParam.xml"


#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

# This execution runs sequentially the scripts described before for each specified period and batch.

for(refPeriod in periods){
  
  cat(paste0(refPeriod, ": "))
  
  for(batch in batches){
    
    cat(batch)
    
    args <- paste(survey, refPeriod, batch, paramFileName, main_path)
    
    shell(cmd = paste(Rscript, current_script_NA, args), wait = TRUE)
    
    shell(cmd = paste(Rscript, current_script_encode, args), wait = TRUE)
    
    
    if(batch == "01"){
      
      shell(cmd = paste(Rscript, current_script_search, args), wait = TRUE)
      
      shell(cmd = paste(Rscript, current_script_train, args), wait = TRUE)

    }

    cat(", ")
    
  }
  
  cat("\n")
  
}



