# This is the master script to build regressors for the prediction model.
#
# Every child script is run in batch below from a shell command.
#
# 
# To gain more specific control we specify the path to Rscript command and to the root folder where 
# the whole project resides (in the future we shall consider using .Rproj)
#
# There exist 5 scripts to be run to build different data sets:
#   - historical data set of validated values (by running 01.build_longitudinalFF.R)
#   - longitudinal regressors by using validated and edited values (by running 02.build_longitudinalRegressors.R)
#   - current regressors about the reference period (by running 03.build_currentRegressors.R)
#   - external regressors by using information from other surveys (by running 04.build_externalRegressors.R)
#   - historical data set of edited values and all regressors (by running 05.build_longitudinalFD.R)
#   - the values of the target variable for previous periods (by running 06.build_targetVar.R)
#
# We need to specify the following parameters:
#   - survey:          the code of the survey
#   - iniPeriod.RTI:   the initial reference period of analysis in RepoTimeInt S4 class
#   - finalPeriod.RTI: the final reference period of analysis in RepoTimeInt S4 class
#   - batches:         the id code of each data processing batch 
#   - paramFileName:   the name of the local parameters file to build the regressors
#
# The longitudinal FD can be built at once for all the periods and batches and saving execution time
# We use a control flow variable (longFDperPeriod) for this


#####                          :::::::::::::::                              #### 
#####                            PACKAGES                                   ####
library(RepoTime)

#####                          :::::::::::::::                              #### 
#####                             PATHS                                     ####
Rscript <- normalizePath("C:/R/R-4.1.1/bin/x64/Rscript.exe")

main_path    <- "N:/UDMTD/UDTMDCOM/AdvITI_github"
path_scripts <- file.path(main_path, "01-01.BuildRegressors/code/scripts")
current_script_longFF  <- normalizePath(file.path(path_scripts, "01.build_longitudinalFF.R"))
current_script_longReg <- normalizePath(file.path(path_scripts, "02.build_longitudinalRegressors.R"))
current_script_curReg  <- normalizePath(file.path(path_scripts, "03.build_currentRegressors.R"))
current_script_extReg  <- normalizePath(file.path(path_scripts, "04.build_externalRegressors.R"))
current_script_longFD  <- normalizePath(file.path(path_scripts, "05.build_longitudinalFD.R"))
current_script_target  <- normalizePath(file.path(path_scripts, "06.build_targetVar.R"))

#####                          :::::::::::::::                              #### 
#####                             PARAMS                                    ####
survey          <- "E30052"
iniPeriod.RTI   <- newRepoTime('MM022016')
finalPeriod.RTI <- newRepoTime('MM042021')
periods         <- getRepo(Seq(iniPeriod.RTI, finalPeriod.RTI, Rot = FALSE))
batches         <- c("01", "02", "03")
paramFileName   <- "E30052.BuildRegressors_LocalParam.xml"

longFDperPeriod <- FALSE


#####                          :::::::::::::::                              #### 
#####                            EXECUTION                                  ####

# This execution block runs sequentially the scripts mentioned above for each period and batch.

t0 <- Sys.time()
times.mt <- matrix(nrow = length(periods), ncol = length(batches))
i <- 1

for(refPeriod in periods){
  
  
  cat(paste0(refPeriod, ": "))
  
  for(batch in batches){
    t1 <- Sys.time()
    cat(batch)
    
    args <- paste(survey, refPeriod, batch, paramFileName, main_path)
    
    if(batch == "01"){
      
      # We are supposing the following assumptions:
      #     - Only one validated file per reference period (FF file)
      #     - The validated file of the period before the reference period 
      #            is available at the same time as the first batch
      # If these assumptions are not valid, this script must be run for all the batches.

      # shell(cmd = paste(Rscript, current_script_longFF, args), wait = TRUE)

    }

    shell(cmd = paste(Rscript, current_script_longReg, args), wait = TRUE)

    shell(cmd = paste(Rscript, current_script_curReg, args), wait = TRUE)

    shell(cmd = paste(Rscript, current_script_extReg, args), wait = TRUE)
    
    if(longFDperPeriod){
      
      shell(cmd = paste(Rscript, current_script_longFD, args), wait = TRUE)
      
      shell(cmd = paste(Rscript, current_script_target, args), wait = TRUE)
      
    }
   
    t2 <- Sys.time()
    cat(", ")
    
    times.mt[i, as.numeric(batch)] <- t2 - t1
    
  }
  i <- i + 1
  cat("\n")
  
}

t3 <- Sys.time()

if(!longFDperPeriod){
  
  args <- paste(survey, periods[length(periods)], batches[length(batches)], paramFileName, main_path)
  
  shell(cmd = paste(Rscript, current_script_longFD, args), wait = TRUE)
  
  shell(cmd = paste(Rscript, current_script_target, args), wait = TRUE)

}

t4 <- Sys.time()

t4 - t3
t4 - t0

