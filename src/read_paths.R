read_paths <- function(xmlObject, envir = .GlobalEnv){
  
  
  paths <- file.path(xml_text(xml_find_all(xmlObject, "//absolute")), 
                     xml_text(xml_find_all(xmlObject, "//relative")))
  paths_labels <- unlist(purrr::map(xml_children(xml_find_all(xmlObject, "//paths")), ~xml_name(.)))
  attrib_names <- unlist(purrr::map(xml_children(xml_find_all(xmlObject, "//paths")), ~xml_attr(., "name")))
  
  out1 <- mapply(function(x, y, z){
    
    var_name <- paste0("path_", x)
    names(y) <- z
    
    assign(var_name, y, envir = envir)
    
    names(y) <- var_name
    return(y)
    
  }, paths_labels, paths, attrib_names)
  
  
  ifSurvey <- as.logical(unlist(purrr::map(xml_children(xml_find_all(xmlObject, "//paths")), ~xml_attr(., "add_surveyCode"))))
  
  out2 <- mapply(function(x, y){
    
    var_name <- paste0("path_", x, "_survey")
    var_content <- file.path(y, survey)
    
    assign(var_name, var_content, envir = .GlobalEnv)
    names(var_content) <- var_name
    return(var_content)
    
  }, paths_labels[ifSurvey], paths[ifSurvey])
  
  
  return(list(out1, out2))
  
}