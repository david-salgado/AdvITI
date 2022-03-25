create_log <- function(path_log, name_logFile, sink_log = TRUE, encoding = 'UTF-8'){
  
  localTime   <- gsub(' ', '_', Sys.time(), fixed = TRUE)
  localTime   <- gsub(':', '-', localTime, fixed = TRUE)
  
  log_fullFileName <- file.path(path_log, paste(survey, name_logFile, localTime, 'txt', sep = "."))
  
  cat(paste0('\n Check the log in ', log_fullFileName, '\n\n'))
  
  if (file.exists(log_fullFileName)) file.remove(log_fullFileName)
  logFile     <- file(log_fullFileName, 'w+', encoding = encoding)
  
  flog.appender(appender.file(logFile), name = 'logFile')
  
  if(sink_log){

    sink(file = logFile)
    sink(file = logFile, type = c('message'))
    sink(file = logFile, type = c('output'))

  }
  
  return(logFile)
  
}