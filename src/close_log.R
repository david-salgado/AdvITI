close_log <- function(logFile, sink_log = TRUE){
  
  if(sink_log){
    
    sink(type = c('message'))
    sink()
    
  }
  close(logFile)
  cat("Log closed.\n\n")
  
  return(TRUE)
  
}