#' Construcci?n del nombre completo de un fichero sustituyendo los patrones. 
#'
#' \code{constructNameDate} Construye la ruta completa y el nombre de un fichero
#' especificados en los elementos del xml le?do como una lista, sustituyendo
#' los patrones por el valor (a?o, mes...) correspondiente.
#'
#' @param dataFiles Objeto de clase \code{list} de longitud la cantidad
#' de ficheros y procesamientos distintos a realizar sobre ellos. Contiene los 
#' distintos elementos necesarios para la construcci?n del nombre completo y
#' la realizaci?n de cada procesamiento.
#' 
#' @param period Objeto de clase \code{character} que incluye el periodo temporal
#' a completar en el patr?n. (Por ejemplo: "MM122020")
#' 
#' @param base Objeto de clase \code{character} que incluye la base temporal a 
#' completar en el patr?n. (Por ejemplo: "2015")
#' 
#' @return \code{character} que representa la ruta y el nombre completo del 
#' fichero, sustituyendo los patrones por el periodo y base indicados, 
#' construido a partir de los elementos correspondientes del xml.
#'
#' @examples
#' \dontrun{
#' constructNameDate(dataFiles, period, base)
#' }

constructNameDate <- function(dataFiles, period, base){
  
  namePttrn <- constructName(dataFiles)
  period <- RepoTime::newRepoTime(period)
  name <- StQ::ParseUnitName(namePttrn, period)
  name <- StQ::ParseUnitName(name, base)
  return(name)
}