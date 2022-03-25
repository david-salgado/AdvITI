#####                          :::::::::::::::                              #### 
#####                ASIGNACIÓN DE PARÁMETROS DEL CMD                       ####
#args <- commandArgs(trailingOnly = TRUE)
args <- list("E30052", "FD", "MM012015", "MM052021", "DD_V1")


#####                          :::::::::::::::                              ####
#####                     CARGA DE PAQUETES Y FUNCIONES                     ####

library(data.table)
library(fastReadfwf)
library(RepoTime, quietly = TRUE)       # Para per??odos del repositorio
library(RepoUtils, quietly = TRUE)      # Para conectarse al repositorio
library(StQ, quietly = TRUE)            # Para gestionar ficheros clave-valor
library(RepoReadWrite, quietly = TRUE)  # Para leer y escribir en el repositorio

#####                          :::::::::::::::                              ####
#####                           PARAMETROS                                  ####

survey <- args[[1]]

path_repo   <- file.path('N:/UDMTD/UDTMDCOM/ICN_Adelantado', 'repo', survey)

DDversion <- as.numeric(substr(args[[5]], 5, nchar(args[[5]])))

DD <- RepoXLSToDD(file.path(path_repo, paste0(survey, '.NombresVariables_V', DDversion, '.xlsx')))

#####                          :::::::::::::::                              ####
#####                        FICHEROS DE ENTRADA                            ####

repo_files <- list.files(path_repo, pattern = 'FD_')

allRefPeriod_Repo            <- Seq(newRepoTime(args[[3]]), newRepoTime(args[[4]]), Rot = FALSE)
allRefPeriod                 <- getRepo(allRefPeriod_Repo)

fileNames<-lapply(paste0(file.path(path_repo, paste0(survey, '.FD_V', DDversion, '.',allRefPeriod))), paste0, c('.P_3', '.P_2', '.P_1'))
names(fileNames) <- allRefPeriod

StQ.list <- lapply(fileNames, function(fn_vec){

  tempStQ.list <- lapply(fn_vec, ReadRepoFile, DD = DD, perl = TRUE, encoding = 'Latin-1')
  return(tempStQ.list)
})
names(StQ.list) <- allRefPeriod


#####                          :::::::::::::::                              ####
#####                          TRANSFORMACIÓN                               ####

StQ_FD.list<-lapply(names(StQ.list),function (refPeriod){

  temp.list <- StQ.list[[refPeriod]]
  FD1 <- temp.list[[3]]
  FD2 <- temp.list[[2]] + temp.list[[3]]
  FD3 <- temp.list[[1]] + FD2 # cambiado de orden
  FD3 <- FD3 + temp.list[[3]] # añadido
  out <- list(FD1, FD2, FD3)
  names(out) <- paste0(file.path(path_repo, paste0(survey, '.FD_V', DDversion, '.', refPeriod)), c('.D_1','.D_2','.D_3'))
  return(out)
})


names(StQ_FD.list) <- NULL
StQ_FD.list <- unlist(StQ_FD.list, recursive=FALSE)

#####                          :::::::::::::::                              ####
#####                     ESCRITURA FICHEROS DEFINITIVOS                    ####

lapply(names(StQ_FD.list),function(fn){WriteRepoFile(StQ_FD.list[[fn]][NOrden != ''],fn)})


