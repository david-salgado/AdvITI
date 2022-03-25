#' Construcci?n de un StQ a partir de los procesamientos realizados sobre un
#' conjunto de ficheros.
#'
#' \code{constructStQ} Construye un objeto StQ tras realizar los procesamientos
#' en forma de \linkS4class{data.tables} de un conjunto de ficheros
#' especificados en el XML.
#'
#' @param dataFiles Objeto de clase \code{list} de longitud la cantidad
#' de ficheros y procesamientos distintos a realizar sobre ellos. Contiene los
#' distintos elementos necesarios para la realizaci?n de cada procesamiento.
#'
#' @param DD Objeto de clase \linkS4class{DD} que contiene el diccionario de
#' datos necesario para la construcci?n del \linkS4class{StQ}.
#'
#' @param period Objeto de clase \code{character} que incluye el periodo temporal
#' a completar en el patr?n. (Por ejemplo: "MM122020")
#'
#' @param base Objeto de clase \code{character} que incluye la base temporal a
#' completar en el patr?n. (Por ejemplo: "2015")
#'
#' @return \linkS4class{StQ} construido a partir del conjunto de ficheros
#' procesados seg?n las especificaciones del xml almacenado en la lista
#' dataFiles.
#'
#' @examples
#' \dontrun{
#' constructStQ(dataFiles, DD, period, base)
#' }

constructStQ <- function(dataFiles, DD, period, base){

  cat(paste0('INICIANDO periodo: ', period, '...\n'))

  #####                          :::::::::::::::                              ####
  #####                       LECTURA DE FICHEROS                             ####

  #### Construcción del nombre completo                                       ####
  filenames <- lapply(dataFiles, function(Fichero){
    constructNameDate(Fichero, period, base)
  })

  condToChooseFile.list <- lapply(dataFiles, function(Fichero){
    trimws(Fichero$conditions$condToChooseFile)
  })

  fileExtensions.list <- lapply(dataFiles, function(Fichero){
    out <- trimws(unlist(Fichero$extension))
    if(is.null(out) | length(out) == 0) out <- "0"
    atri <- attributes(Fichero$extension)
    if(!is.null(atri)){
      atri <- lapply(atri, function(x){
        if(x == "TRUE" | x == "FALSE"){
          return(as.logical(x))
        }else{return(x)}
      })
      attributes(out) <- atri
    }
    return(out)
  })

  schemas.list <- lapply(dataFiles, function(Fichero){

    if(is.null(Fichero$schema)){ return(NULL) }

    out <- list()
    out$schemaName <- constructNameDate(Fichero$schema, period, base)
    out$schemaExt  <- trimws(unlist(Fichero$schema$extension))

    atri <- attributes(Fichero$schema$extension)
    if(out$schemaExt %in% c("xlsx", "xls") & is.null(atri)){

      stop(paste0("El diseño de registro:\n", out$schemaName,
                  "\n no tiene indicado el tipo: type = schema or INE."))

    }
    if(!is.null(atri)){

      out$sheet <- ifelse(is.null(attr(Fichero$schema$extension, "sheet")),
                          1, attr(Fichero$schema$extension, "sheet"))
      out$type <- attr(Fichero$schema$extension, "type")
      out$regionName <- attr(Fichero$schema$extension, "regionName")
      validate <- ifelse(is.null(attr(Fichero$schema$extension, "validate")),
                          "FALSE", attr(Fichero$schema$extension, "validate"))
      out$validate <- ifelse(validate %in% c("TRUE", "FALSE"),
                             as.logical(validate), validate)

    }

    return(out)
  })

  #### Lectura                                                                ####
  DT_list <- mapply(function(x, y, z, s){
    cat(paste0('Procesando fichero ', x, '... '))

    if (length(y) > 0){

      if (eval(parse(text = y))){
        readCond <- TRUE
      } else {
        readCond <- FALSE
      }
    } else {
      readCond <- TRUE
    }

    if(readCond){

      output <- readFileINE(fileName = x, fileExtension = z,
                  infoSchema = s, extAttributes = attributes(z))

      cat('Leído.\n')
    }else{

      output <- NULL
      cat('NO leído.\n')

    }

    return(output)
  }, filenames, condToChooseFile.list,
  fileExtensions.list, schemas.list, SIMPLIFY = FALSE)


  #### Asignación de etiquetas (labels)                                       ####
  cat(paste0('Asignando etiquetas a los ficheros... '))
  labels.list <- lapply(dataFiles, function(x){

    return(unlist(x$label))

  })

  if(length(unlist(labels.list)) != length(unique(unlist(labels.list)))){
    stop("La etiqueta de cada objeto file debe ser única. Revisar el fichero XML de parámetros.")
  }

  names(DT_list) <- unlist(labels.list)
  cat(paste0('ok.\n'))

  #####                          :::::::::::::::                              ####
  #####                              RENAME                                   ####

  cat(paste0('Renombrando variables... '))

  #### Info del XML                                                           ####
  rename.list <- lapply(dataFiles, function(Fichero){
    rbindlist(lapply(Fichero$rename,
                     as.data.table))
  })

  #### Aplicación sobre DT_list                                               ####
  DT_list <- mapply(function(x, y){
    if(is.null(x)){return(x)}
    if(dim(y)[1] == 0){return(x)}

    fileName <- unlist(y$fileName)
    parseFlag <- TRUE
    if(length(grep('_\\[([A-Za-z]+)\\]_', fileName)) > 0){
      # no parsear con period
      fileName <- gsub('_\\[([A-Za-z]+)\\]_', '"\\1"', fileName)
      parseFlag <- FALSE
    }
    if(length(grep('\\[([A-Za-z]+)\\]', fileName)) > 0 & parseFlag){
      fileName <- StQ::ParseUnitName(fileName,
                                     RepoTime::newRepoTime(period))
    }

    newName <- unlist(y$newName)
    parseFlag <- TRUE
    if(length(grep('_\\[([A-Za-z]+)\\]_', newName)) > 0){
      # no parsear con period
      newName <- gsub('_\\[([A-Za-z]+)\\]_', '[\\1]', newName)
      parseFlag <- FALSE
    }
    if(length(grep('\\[([A-Za-z]+)\\]', newName)) > 0 & parseFlag){
      newName <- StQ::ParseUnitName(newName,
                                     RepoTime::newRepoTime(period))
    }

    setnames(x, fileName, newName, skip_absent = TRUE)
    return(x)
  }, DT_list, rename.list, SIMPLIFY = FALSE)

  cat(paste0('ok.\n'))

  #####                          :::::::::::::::                              ####
  #####                             SELECTION                                 ####

  cat(paste0('Seleccionando filas y columnas... '))

  #### Info del XML                                                           ####
  selection.list <- lapply(dataFiles, function(Fichero){
    colNames <- trimws(
      strsplit(unlist(Fichero$selection$columns$varnames), split = ',')[[1]]
    )

    rowCondition <- trimws(Fichero$selection$rows$condition)
    rowCondition <- StQ::ParseUnitName(rowCondition,
                                   RepoTime::newRepoTime(period))
    rowCondition <- gsub("_([A-Za-z]+)_", 'DT_list[["\\1"]]', rowCondition)

    if(length(rowCondition) > 0){
      rowCondition <- as.list(trimws(strsplit(rowCondition, ";")[[1]]))

      if(length(rowCondition) > 3){

        rowCondition[[3]] <- paste(rowCondition[3:length(rowCondition)], collapse = ", ")
        rowCondition <- rowCondition[1:3]
      }
    }

    rowCondition <- ifelse(length(rowCondition) == 0, TRUE, rowCondition[[1]])


    return(list(col = colNames, row = rowCondition))
  })

  #### Chequeos sobre las variables                                           ####
  check.list <- lapply(seq_along(DT_list), function(idx, x, y){

    if(length(setdiff(y[[idx]]$col, names(x[[idx]]))) > 0 & !is.null(x[[idx]])){

      cat(paste0("\n Las siguientes columnas:\n",
                       paste(setdiff(y[[idx]]$col, names(x[[idx]])), collapse = ", "),
                      "\n faltan en el fichero: ", filenames[idx], ".\n"))

      return(FALSE)

    } else{

      return(TRUE)

    }

  }, DT_list, selection.list)

  if(!all(unlist(check.list))){

    stop("Revise la definición de los parametros locales en el XML para los ficheros afectados.")

  }

  #### Aplicación sobre DT_list                                               ####
  DT_list <- mapply(function(x, y){

    if(is.null(x)){return(x)}

    z <- x[eval(parse(text = y$row[[1]]))]

    if(length(y$row) > 1){

      vars_by <- eval(parse(text = strsplit(y$row[[3]], "=")[[1]][2]))

      z <- z[, eval(parse(text = y$row[[2]])), by = vars_by]

    }

    z <- z[, y$col, with = FALSE]

    return(z)

  }, DT_list, selection.list, SIMPLIFY = FALSE)

  cat(paste0('ok.\n'))

  #####                          :::::::::::::::                              ####
  #####                             ADD COLS                                 ####

  cat(paste0('Añadiendo columnas... '))

  #### Info del XML                                                           ####
  addCols.list <- lapply(dataFiles, function(Fichero){

    rbindlist(lapply(Fichero$addCols, as.data.table))

  })

  #### Aplicación sobre DT_list                                               ####
  DT_list <- mapply(function(x, y){

    if(is.null(x)){return(x)}
    if(dim(y)[1] == 0){return(x)}

    lapply(seq_along(y$newVarName), function(nVN.idx){

      newVarName <- unlist(y$newVarName[nVN.idx])

      newVarValue <- unlist(y$newVarValue[nVN.idx])
      newVarValue <- gsub("_([A-Za-z]+)_", 'DT_list[["\\1"]]', newVarValue)

      x[, (newVarName) := eval(parse(text = newVarValue))]

    })
    return(x)
  }, DT_list, addCols.list, SIMPLIFY = FALSE)

  cat(paste0('ok.\n'))

  #####                          :::::::::::::::                              ####
  #####                             TRANSFORM                                 ####

  cat(paste0('Realizando transformaciones (melt)... '))

  #### Info del XML                                                           ####
  transform.list <- lapply(dataFiles, function(Fichero){
    pivotName <- unlist(Fichero$transform$pivotName)
    variableName <- unlist(Fichero$transform$variableName)
    valueName <- unlist(Fichero$transform$valueName)
    output <- list(pivotName = pivotName, variableName = variableName, valueName = valueName)
    return(output)
  })

  #### Aplicación sobre DT_list                                               ####
  DT_list <- mapply(function(x, y){

    if(is.null(x)){return(x)}
    if(is.null(y$pivotName)){return(x)}

    output <- melt(
      x, id.vars = y$pivotName, variable.factor = FALSE,
      variable.name = y$variableName, value.name = y$valueName)

    return(output)
  }, DT_list, transform.list, SIMPLIFY = FALSE)

  cat(paste0('ok.\n'))

  #####                          :::::::::::::::                              ####
  #####                             SET KEY                                   ####

  cat(paste0('Comprobando key y añadiendola si falta... '))

  #### Info del XML                                                           ####
  setKey.list <- lapply(dataFiles, function(Fichero){
        x <- as.data.table(list(fileKey = Fichero$setKey$fileKey,
                                repoKey = Fichero$setKey$repoKey))
        if(is.null(unlist(Fichero$setKey$equivFile$name))) y <- NA_character_
        if(!is.null(Fichero$setKey$equivFile$name)){
          y <- constructNameDate(Fichero$setKey$equivFile, period, base)
        }
        x[, equivFileName := y]
        label <- ifelse(!is.null(unlist(Fichero$setKey$equivFile$label)),
                        unlist(Fichero$setKey$equivFile$label), NA_character_)
        x[, equivFileLabel := label]
        return(x)
      })

  equivFileExtensions.list <- lapply(dataFiles, function(Fichero){
    out <- trimws(unlist(Fichero$setKey$equivFile$extension))
    if(is.null(out) | length(out) == 0) out <- "0"
    atri <- attributes(Fichero$setKey$equivFile$extension)
    if(!is.null(atri)){
      atri <- lapply(atri, function(x){
        if(x == "TRUE" | x == "FALSE"){
          return(as.logical(x))
        }else{return(x)}
      })
      attributes(out) <- atri
    }
    return(out)
  })


  schemasEquivFile.list <- lapply(dataFiles, function(Fichero){

    if(is.null(Fichero$setKey$equivFile$schema)){ return(NULL) }

    out <- list()
    out$schemaName <- constructNameDate(Fichero$setKey$equivFile$schema, period, base)
    out$schemaExt  <- trimws(unlist(Fichero$setKey$equivFile$schema$extension))

    atri <- attributes(Fichero$setKey$equivFile$schema$extension)
    if(out$schemaExt %in% c("xlsx", "xls") & is.null(atri)){

      stop(paste0("El diseño de registro:\n", out$schemaName,
                  "\n no tiene indicado el tipo: type = schema or INE."))

    }
    if(!is.null(atri)){

      out$sheet <- ifelse(is.null(attr(Fichero$setKey$equivFile$schema$extension, "sheet")),
                          1, attr(Fichero$setKey$equivFile$schema$extension, "sheet"))
      out$type <- attr(Fichero$setKey$equivFile$schema$extension, "type")
      out$regionName <- attr(Fichero$setKey$equivFile$schema$extension, "regionName")
      validate <- ifelse(is.null(attr(Fichero$setKey$equivFile$schema$extension, "validate")),
                         "FALSE", attr(Fichero$setKey$equivFile$schema$extension, "validate"))
      out$validate <- ifelse(validate %in% c("TRUE", "FALSE"),
                             as.logical(validate), validate)

    }

    return(out)
  })


  #### Aplicación sobre DT_list                                               ####
  DT_list <- mapply(function(x, y, z, s){

    if(is.null(x)){return(x)}
    fk <- trimws(
      strsplit(unlist(y$fileKey), split = ',')[[1]]
    )
    rk <- trimws(
      strsplit(unlist(y$repoKey), split = ',')[[1]]
    )

    if(length(setdiff(rk, fk)) == 0){return(x)}

    if(is.na(y$equivFileLabel)){

      equivFile.dt <- readFileINE(fileName = y$equivFileName, fileExtension = z,
                                  infoSchema = s, extAttributes = attributes(z))

    }
    if(!is.na(y$equivFileLabel)){

      equivFile.dt <- DT_list[[y$equivFile_label]]

    }

    notIn <- setdiff(c(fk, rk), names(equivFile.dt))
    if(length(notIn) > 0){
      stop(paste0("Falta(n) la(s) variable(s): ",
                  paste(notIn, collapse = ", "), "en el fichero asignado como equivFile:\n",
                  y$equivFile))
    }

    z <- equivFile.dt[, rk, with = FALSE]

    output <- merge(x, z, by = fk, all.x = TRUE)

    return(output)

  }, DT_list, setKey.list, equivFileExtensions.list, schemasEquivFile.list,
  SIMPLIFY = FALSE)

  cat(paste0('ok.\n'))

  #####                          :::::::::::::::                              ####
  #####                           CONSTRUCT STQ                               ####

  cat(paste0('Generando los StQ... '))

  StQ_list <- lapply(seq_along(DT_list), function(idx, DD){

    x <- DT_list[[idx]]
    y <- dataFiles[[idx]]$constructStQ$method[[1]]
    cat(idx)
    if(!is.null(x)){

      if(y == "StQ"){

        notIn <- setdiff(c("IDDD", "Value"), names(x))
        if(length(notIn) > 0){
          stop(paste0("En el fichero con label: ", names(DT_list)[idx],
                      "faltan las variables: ", paste(notIn, collapse = ", "),
                      "para poder construir el StQ con method=StQ. Revise el xml de parámetros locales.")
               )
        }
        setcolorder(x, c(setdiff(names(x), c("IDDD", "Value")), "IDDD", "Value"))

        output <- StQ(x, DD)
      }
      if(y == "melt_StQ"){

        # Comprobamos que todas las variables están en el DD
        faltan <- names(x)[which(is.na(StQ::UnitToIDDDNames(names(x), DD)))]
        if(length(faltan) > 0){
          cat(paste0("En el fichero ", idx,
                           " las variables ", paste(faltan, collapse = ","),
                           " no están en el diccionario.\n"))
        }

        output <- melt_StQ(x, DD)
      }
    } else{
      output <- NULL
    }
    return(output)
  }, DD = DD)

  StQ_list <- StQ_list[sapply(StQ_list, function(x){!is.null(x)})]
  StQ <- Reduce('+', StQ_list)

  cat(paste0('ok.\n'))

  cat(paste0('OK periodo: ', period, '.\n\n'))

  return(StQ)
}
