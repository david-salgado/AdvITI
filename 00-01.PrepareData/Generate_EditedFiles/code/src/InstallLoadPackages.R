#' @title Instalacion y carga de paquetes en R desde un repositorio local de paquetes.
#'
#' @description \code{InstallLoadpackages} instala paquetes desde un respositorio local de paquetes.
#'
#' @param repo \code{Vector} de tipo \code{character} de longitud 1 con las ruta completa en
#' la que se ubibca el repositorio local de paquetes.
#'
#' @param packages \code{Vector} de tipo \code{character} con los nombres de los paquetes que se 
#' desean instalar.
#' 
#' @param Load \code{Vector} de tipo \code{logical} indicando si se desean cargar (TRUE) o no 
#' (FALSE) los paquetes instalados.
#'
#' @return NULL
#'
#' @examples
#' dontrun{
#' repo <- 'G:/GRUPO_SINODO_METODOLOGIA/miniCRAN'
#' PaquetesR <- c('openxlsx', 'lubridate', 'data.table', 'gdata', 'XML', 'haven',
#'     'R2HTML', 'R.utils', 'formula.tools', 'stringi', 'stringr',
#'     'forecast', 'imputeTS')
#' PaqRepo <- c('RepoTime', 'StQ', 'RepoReadWrite', 'RepoUtils', 'GenFunctions',
#'              'TSPred','BestTSPred', 'HRInterval')
#' Paquetes <- c(PaquetesR, PaqRepo)
#' 
#' InstallLoadPackages(repo, packages = Paquetes)
#'}
#'
#' @export
InstallLoadPackages <- function(repo, packages, load = FALSE, type = 'binary', Rversion = '3.5', verbose = FALSE){
  
  pcRversion_major <- R.version$major
  pcRversion_minor <- strsplit(R.version$minor, split = '.', fixed = TRUE)[[1]][1]
  pcRver <- paste(pcRversion_major, pcRversion_minor, sep = '.')

  if (pcRver != Rversion) stop(paste0('[InstallLoadPackages] Version of R must be ', Rversion, '.\n\n'))
  
  instPackages <- installed.packages()[, 'Package']
  if (!'miniCRAN' %in% instPackages) {
    
    if (verbose) cat('[InstallLoadPackages] Installing miniCRAN from https://cran.rstudio.com...')
    install.packages('miniCRAN', dependencies = TRUE, type = "binary", 
                     repos = "https://cran.rstudio.com/", verbose = TRUE)
    if (verbose) cat(' ok.\n\n')
    
  }
  
  if (type == 'source') {
    miniCRAN <- as.data.frame(
      miniCRAN::pkgAvail(repo, type = "source", Rversion = R.version), 
                         stringsAsFactors = FALSE)
  }
  
  if (type == 'binary') {
    
    miniCRAN <- as.data.frame(
      miniCRAN::pkgAvail(repo, type = "binary", Rversion = R.version), 
      stringsAsFactors = FALSE)
    
  }

  installedVers <- installed.packages()[, 'Version']

  sessionPkgs <- names(sessionInfo()$otherPkgs)
  alreadyLoaded <- packages[packages %in% sessionPkgs]
  if (length(alreadyLoaded) > 0) {
      
      if (verbose) cat('[InstallLoadPackages] Detaching packages...')
      lapply(paste0('package:', alreadyLoaded), detach, 
             character.only = TRUE, unload = TRUE, force = TRUE)
      if (verbose) cat(' ok.\n\n')
      
  }
  
  pkgNotInMiniCRAN <- setdiff(packages[!packages %in% miniCRAN$Package], 'miniCRAN')
  if (length(pkgNotInMiniCRAN) > 0) {
    
    stop(paste0('[InstallLoadPackages] The following packages are not in ', 
                repo, ': \n', paste0(pkgNotInMiniCRAN, collapse = ', ')))
    
  }

  for (Paq in packages) {
    
    CRANver <- miniCRAN[Paq, 'Version']
    if ( (!Paq %in% instPackages) || (installedVers[Paq] != CRANver) ) {
      
      if (verbose) cat(paste0('[InstallLoadPackages] Installing package ', Paq, '...'))
      if (type == 'source'){
        
        tempRepo <- file.path(
          "file://", 
          normalizePath(paste0(repo, '/src/contrib'), winslash = "/"))
        install.packages(
          pkgs = Paq,
          lib = .libPaths()[1],
          repos = tempRepo,
          contriburl = tempRepo,
          type = "source",
          dependencies = TRUE)
      }
      
      if (type == 'binary') {
        
        tempRepo <- file.path(
          "file://", 
          normalizePath(paste0(repo, '/bin/windows/contrib/3.5'), 
                        winslash = "/")) 
        install.packages(
          pkgs = Paq,
          lib = .libPaths()[1],
          repos = tempRepo,
          contriburl = tempRepo,
          type = "binary",
          dependencies = TRUE)
        
      }
      if (verbose) cat(' ok.\n\n')
    }
  }
  
  if (load) {
    
    loaded <- NULL
    
    for (idx in seq_along(packages)) {
      
      Paq <- packages[idx]
      
      if (verbose) {
        
        cat(paste0('[InstallLoadPackages] Loading library ', Paq, '... '))
        
      }
      loaded[idx] <- require(Paq, character.only = TRUE)
      
    }
    if (any(!loaded) & verbose) {
      
      paste0('[InstallLoadPackages] Package(s): '
             , paste0(loaded[!loaded], collapse = ", "), ' has not been loaded.\n\n')
      
    }
    if (verbose) cat(' ok.\n\n')
    return(loaded)
  }
  if(!load) return(NULL)
}


