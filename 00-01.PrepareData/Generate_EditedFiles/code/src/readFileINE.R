#' Lectura de ficheros con distintas extensiones
#'
#' \code{readFileINE} Lee ficheros con una variedad de extensiones
#'
#' @param fileName Nombre completo del fichero a leer (path, nombre y extensión).
#' 
#' @param fileExtension Extensión del fichero, si no existe será "0". 
#' 
#' @param infoSchema Es una lista con la información sobre el diseño de registro 
#' en formato Schema, cuando sea necesario. Las componentes de la lista son:
#' \code{schemaName}, \code{schemaExt}, \code{sheet}, \code{type}, \code{validate}.
#' 
#' @param extAttributes Atributos de la extensión, si existen.
#' 
#' @return \code{data.table} con el conjunto de datos leído del fichero.
#'
#' @examples
#' \dontrun{
#' constructName(Fichero_xml_element)
#' }

readFileINE <- function(fileName, fileExtension, infoSchema, extAttributes){
  
  
  z <- fileExtension
  attributes(z) <- extAttributes
  
  x <- fileName
  s <- infoSchema
  
  read <- FALSE
  
  if(z == "sas7bdat"){
    
    output <- as.data.table(haven::read_sas(x))
    read <- TRUE
  }
  if(z == "xlsx"){
    
    attr(z, "xlsxFile") <- x
    output <- as.data.table(do.call(openxlsx::read.xlsx, attributes(z)))
    read <- TRUE
  }
  if(z == "csv"){
    
    attr(z, "file") <- x
    output <- do.call(data.table::fread, attributes(z))
    read <- TRUE
    
  }
  if(z == "accdb"){
    
    db <- odbcConnectAccess2007(x)
    output <- as.data.table(do.call(sqlFetch, list(db, attr(z, "table"))))
    odbcCloseAll()
    read <- TRUE
    
    
  }
  if(!z %in% c("sas7bdat", "xlsx", "csv", "accdb") & !is.null(s)){
    
    cat(paste0('\n con el schema definido por el fichero ', s$schemaName, '...\n '))
    
    if(s$type == "Schema"){
      
      schema <- fastReadfwf::StxlsxToSchema(xlsxName = s$schemaName, 
                                            sheetToRead = s$sheet)
      output <- fastReadfwf::fread_fwf(x, schema, 
                                       outFormat = "data.table",
                                       validate = s$validate)
      read <- TRUE
      
    }
    if(s$type == "INE"){
      
      if(s$schemaExt == "xlsx"){
        
        argsSchema <- list(xlsxName = s$schemaName, 
                           sheetToRead = s$sheet)
        if(!is.null(s$regionName)) argsSchema$regionName <- s$regionName
        
        schema <- do.call(fastReadfwf::INExlsxToSchema, argsSchema)
        
      }
      if(s$schemaExt == "xml"){
        
        schema <- fastReadfwf::INExmlToSchema(s$schemaName)
        
      }
      
      output <- fastReadfwf::fread_fwf(x, schema, 
                                       outFormat = "data.table",
                                       validate = s$validate)
      read <- TRUE
      
    }
  }
  if(!read){
    stop(paste0("El fichero:\n",
                x, "\n tiene un formato no soportado."))
  }
    
    
  return(output)
  
 
}