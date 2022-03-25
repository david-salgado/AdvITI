#' Construcción del nombre completo de un fichero. 
#'
#' \code{constructName} Construye la ruta completa y el nombre de un fichero
#' especificados en los elementos del xml leído como una lista. 
#'
#' @param Fichero_xml_element Objeto de clase \code{list} de longitud la cantidad
#' de ficheros y procesamientos distintos a realizar sobre ellos. Contiene los 
#' distintos elementos necesarios para la construcción del nombre completo y
#' la realización de cada procesamiento.
#' 
#' @return \code{character} que representa la ruta y el nombre completo del fichero
#' construido a partir de los elementos correspondientes del xml.
#'
#' @examples
#' \dontrun{
#' constructName(Fichero_xml_element)
#' }

constructName <- function(Fichero_xml_element){
  
  dataFileName      <- unlist(Fichero_xml_element$name)
  dataFileExtension <- unlist(Fichero_xml_element$extension)
  dataFilePath      <- file.path(unlist(Fichero_xml_element$path$root), unlist(Fichero_xml_element$path$relative))
  name <- file.path(dataFilePath, paste(dataFileName, dataFileExtension, sep = '.'))
  name <- trimws(gsub('\\\\', '/', name))
  return(name)
  
}