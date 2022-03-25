spec_repoFiles <- function(files_names, last = TRUE){


  fileSpec.dt <- data.table(fileName = files_names)[
                , numDDVersion := as.integer(FileDDVersion(fileName))][
                , DDVersion := paste0('V', numDDVersion)][
                , fileVersion := tstrsplit(fileName, split = '.', fixed = TRUE)[[4]]][
                , numFileVersion := as.integer(tstrsplit(fileVersion, split = '_')[[2]])][
                , period := tstrsplit(fileName, split = '.', fixed = TRUE)[[3]]]
  
  if(last){
    
    fileSpec_D.dt <- fileSpec.dt[grep('D_[0-9]+', fileVersion)]
    fileSpec_P.dt <- fileSpec.dt[grep('P_[0-9]+', fileVersion)][!period %in% fileSpec_D.dt[, period]]
    
    fileSpec.dt <- rbindlist(list(fileSpec_D.dt, fileSpec_P.dt))
    
    fileSpec.dt <- fileSpec.dt[
            , .SD[numFileVersion %in% head(sort(numFileVersion, decreasing = TRUE), 1)], by = period]
  
  }
          

  return(fileSpec.dt)

}