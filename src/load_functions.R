load_functions <- function(xmlObject){
  
  cat(paste0('\n Loading functions ...\n\n'))
  
  execFunctions <- xml_text(xml_find_all(xmlObject, "//functions//function//name"))
  infoFun <- unlist(purrr::map(xml_find_all(xmlObject, "//functions//function"), ~ xml_attr(., "path")))
  names(infoFun) <- execFunctions
  
  
  funSourced <- sapply(execFunctions, function(fun_name, infoFun){
    
    fun_fullName <- ifelse("survey" %in% strsplit(infoFun[[fun_name]], '_')[[1]],
                           paste0(survey, '.', fun_name, '.R'),
                           paste0(fun_name, '.R'))
    fun_source <- file.path(get(paste0('path_', infoFun[[fun_name]])), fun_fullName)
    cat(fun_source)
    cat("\n")
    source(fun_source)
    return(fun_name %in% ls(.GlobalEnv))
  }, infoFun)
  names(funSourced) <- execFunctions
  
  
  return(funSourced)

}
