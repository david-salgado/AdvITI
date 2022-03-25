#####                          :::::::::::::::                              #### 
#####                ASIGNACIÓN DE PARÁMETROS DEL CMD                       ####
#args <- commandArgs(trailingOnly = TRUE)
args <- list("E30052", "FD", "MM012015", "MM052021", "DD_V1", "3")

if(Sys.info()[["sysname"]] == "Linux"){
  
  main_path <- "/GRINE/Repositorio"
  
}

if(Sys.info()[["sysname"]] == "Windows"){
  
  main_path <- "N:/UDMTD/UDTMDCOM/AdvITI_github"
  
}

#####                          :::::::::::::::                              #### 
#####               LECTURA PARÁMETROS GLOBALES XML                         ####
if(!require(xml2)) {
  
  install.packages("xml2", quiet = TRUE)
  library(xml2)
  
}

survey                  <- as.character(args[1])
batch                   <- as.character(args[6])

paramGlobal.list        <- as_list(read_xml(file.path(main_path, 
                                                      '/00-01.PrepareData/Generate_EditedFiles/param/global/Ennnnn.Reading_GlobalParam.xml')))
path_root               <- unlist(paramGlobal.list$parametros_globales$paths$root)
path_param_global       <- file.path(path_root, unlist(paramGlobal.list$parametros_globales$paths$param_global))
paramGlobal_survey.list <- as_list(read_xml(file.path(path_param_global, survey, paste0(survey, '.Reading_GlobalParam.xml'))))

path_scripts            <- file.path(path_root, paramGlobal.list$parametros_globales$paths$script)
path_source             <- file.path(path_root, paramGlobal.list$parametros_globales$paths$source[[1]])
path_param_local        <- file.path(path_root, paramGlobal.list$parametros_globales$paths$param_local[[1]])
path_param_local_survey <- file.path(path_param_local, survey)

path_data_intermediate  <- file.path(paramGlobal.list$parametros_globales$paths$data_intermediate[[1]])
path_data_intermediate_survey <- file.path(path_data_intermediate, survey)

path_data_global  <- file.path(paramGlobal.list$parametros_globales$paths$data[[1]])
path_data_global_survey <- file.path(path_data_intermediate, survey)

path_repo               <- file.path(paramGlobal.list$parametros_globales$paths$repo[[1]], survey)

path_log  <- file.path(path_root, paramGlobal.list$parametros_globales$paths$log[[1]], survey) 


#####                          :::::::::::::::                              #### 
#####                           CREACIÓN LOG                                ####
if(!require(futile.logger)) {
  
  install.packages("futile.logger", quiet = TRUE)
  library(futile.logger)
  
}
localTime <- gsub(' ', '_', Sys.time(), fixed = TRUE)
localTime <- gsub(':', '-', localTime, fixed = TRUE)
logFileName <- file.path(path_log, paste0(survey, '.Inicializacion', args[[2]],'.log.', localTime, '.txt'))
if (file.exists(logFileName)) file.remove(logFileName)
logFile <- file(logFileName, 'w+', encoding = 'UTF-8')
##flog.info(paste0('\n Compruebe el log en ', logFileName, '\n\n'))
##flog.appender(appender.file(logFile), name = 'logFile')
#sink(file = logFile)
#sink(file = logFile, type = c('message'))
#sink(file = logFile, type = c('output'))


#####                          :::::::::::::::                              #### 
#####                     CARGA DE PAQUETES Y FUNCIONES                     ####
##flog.info(paste0('\n Cargando funciones y paquetes ...\n\n'))
execFunctions <- sapply(paramGlobal.list$parametros_globales$functions, function(fun){
  
  fun$name[[1]]
  
})
funSourced <- sapply(execFunctions, function(fun_name){
  
  fun_source <- file.path(path_source, paste0(fun_name, '.R'))
  source(fun_source)
  fun_name %in% ls(.GlobalEnv)
})
names(funSourced) <- execFunctions



packages.df <- data.frame(name = unlist(paramGlobal.list$parametros_globales$packages))
infoPack <- sapply(seq_along(paramGlobal.list$parametros_globales$packages), 
                   function(idx){
                     repo <- attr(paramGlobal.list$parametros_globales$packages[[idx]], 'repo')
                     neededIn <- strsplit(attr(paramGlobal.list$parametros_globales$packages[[idx]], 'neededIn'), ',')[[1]]
                     neededInFF <- ifelse('FF' %in% neededIn | 'all' %in% neededIn, 1, 0)
                     neededInFT <- ifelse('FT' %in% neededIn | 'all' %in% neededIn, 1, 0)
                     neededInFL <- ifelse('FL' %in% neededIn | 'all' %in% neededIn, 1, 0)
                     return(c(repo, neededInFF, neededInFT, neededInFL))
                   })
packages.df$repo <- infoPack[1,]
packages.df$neededInFF <- infoPack[2,]
packages.df$neededInFT <- infoPack[3,]
packages.df$neededInFL <- infoPack[4,]

if(args[[2]] %in% c("FF", "FD", "FG")){
  
  packages.df <- packages.df[which(packages.df$neededInFF == 1),]
  
} else if(args[[2]] == "FT"){
  
  packages.df <- packages.df[which(packages.df$neededInFT == 1),]
  
} else if(args[[2]] == "FL"){
  
  packages.df <- packages.df[which(packages.df$neededInFL == 1),]
  
} else{
  
  stop("Los tipos de ficheros soportados son: FG, FD, FF, FT y FL.")
}


execPackages <- as.character(packages.df$name)
notLoaded <- logical()
if(length(execPackages) > 0){
  packLoaded <- sapply(execPackages, function(x){
    #if(!require(x, character.only = TRUE)){install.packages(x)}
    out <- require(x, character.only = TRUE)
    return(out)})
  notLoaded <- execPackages[!packLoaded]
}  

# Check loaded
if (length(notLoaded) > 0) {
  
  flog.info('Los siguientes paquetes no pueden cargarse:', name = 'logFile', appender = logFile)
  flog.info(paste0(paste(notLoaded, collapse = ', '), '.'), name = 'logFile', appender = logFile)
  stop()
  
} 


#####                          :::::::::::::::                              #### 
#####                             PARÁMETROS                                ####

### Parámetros de la operación estadística                                  ####
paramLocal_name <- paste0(survey, ".Reading_LocalParam_EditedFiles_batch", batch, ".xml")
paramLocal.list <- as_list(read_xml(file.path(path_param_local_survey, paramLocal_name)))
localParamFile  <- paramLocal.list$parametros_locales[[args[[2]]]]
base_FF         <- unlist(localParamFile$time$base$code_repo)
if(is.null(base_FF)){ base_FF <- "" }
path_output     <- file.path(
  trimws(unlist(localParamFile$path$output$root)),
  trimws(unlist(localParamFile$path$output$relative)))
if (length(path_output) == 0) path_output <- path_repo

allRefPeriod_Repo            <- Seq(newRepoTime(args[[3]]), newRepoTime(args[[4]]), Rot = FALSE)
allRefPeriod                 <- getRepo(allRefPeriod_Repo) 


#####                          :::::::::::::::                              ####
#####                   CONDICIONES PARA LA  EJECUCIÓN                      ####

currentDDversion <- as.numeric(substr(args[[5]], 5, nchar(args[[5]])))

conditions.dt <- data.table(
  periodoReferencia = allRefPeriod
  , versionFichero = paste0(".P_", batch))[
  , fileName := paste0(survey, paste0(".", args[[2]]), '_V', currentDDversion, '.', periodoReferencia, versionFichero)]



#####                          :::::::::::::::                              ####                          
#####                         GENERACIÓN DE StQ                             ####

VNC <- RepoXLSToVNC(file.path(path_repo, paste0(survey, '.NombresVariables_V', currentDDversion, '.xlsx')))
DD  <- RepoDDToDD(file.path(path_repo, paste0(survey, '.DD_V', currentDDversion)), VNC)

sourceFiles <- localParamFile$sourceFiles

StQ_list <- lapply(allRefPeriod, constructStQ, dataFiles = sourceFiles, DD = DD, base = base_FF)
names(StQ_list) <- allRefPeriod

#####                          :::::::::::::::                              ####
#####                     ESCRITURA DE LOS FICHEROS                        #####
# flog.info(paste0('\n\n', survey, '::: Escritura de los ficheros ', args[[2]], '...\n'), name = 'logFile', appender = logFile)
for (period in allRefPeriod){
  
  fileName <- conditions.dt[periodoReferencia == period]$fileName
  flog.info(paste0('    ', fileName, '...'), name = 'logFile', appender = logFile)
  WriteRepoFile(StQ_list[[period]], file.path(path_output, fileName))
  flog.info('    ok.\n', name = 'logFile', appender = logFile)
  
}
# flog.info('ok.\n', name = 'logFile', appender = logFile)


####                            :::::::::::::::                              ####
######                          CIERRE DEL LOG                              #####
# flog.info(paste0('\n\n', survey, '::: Cerrando fichero log.'), name = 'logFile', appender = logFile)

# sink(type = c('message'))
# sink()
# close(logFile)
# closeAllConnections()
# flog.info("Terminado el proceso.")

  
