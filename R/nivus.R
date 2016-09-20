#' Reads NIVUS's structured .txt or .gz time series data and creates a list of
#' xts-objects.
#'
#' @title Read time series data from NIVUS
#' @param file The file to be read.
#' @param ncol The number of columns (i.e., parameter) to be read.
#' @param parameter Vector of names of parameters to be read.
#' @param verbose logical. Provide additional details?
#' @return A list of xts-objects.
#' @rdname read_nivus
#' @export 
#' @seealso \code{\link[xts]{xts}}.
read_nivus <- function(file, ncol=4, parameter=NULL, verbose=FALSE) {
  
  # debug <- FALSE
  
  if (ncol > 51) stop("ncol limited to 51")
  
  if (!is.null(parameter)) ncol <- 51
  
  if (verbose) print(file)  
  
  #get separator:
  sep.bool  <- is.na(as.numeric(scan(file, 
                                     what = character(),
                                     nlines = 6,
                                     quiet = T,
                                     fileEncoding = "iso-8859-1")[23]))
  if (sep.bool) {
    deci <- ","
  } else {
    deci  <- "."
  }
  
  #if (debug) print(paste("deci: ",deci))
  
  # line 9 -> extract parameter names and units
  line9 <- scan(file, 
                what = character(),
                sep = "\t",
                nlines = 1,
                quiet = T,
                skip = 8,
                fileEncoding = "iso-8859-1")
  
  line9 <- line9[-(1:2)] # exclude columns Datum, Uhrzeit
  
  ls.line9 <- strsplit(line9, split = " \\[")
  
  #if (debug) print(paste("line9: ",ls.line9))
  
  name <- gsub(" ", "", unlist(lapply(ls.line9, "[", 1)))
  name <- gsub("\\(|\\)|\\-", "", name)
  
  #if (debug) print(paste("name: ",name))
  
  unit <- gsub("\\]", "", unlist(lapply(ls.line9, "[", 2)))
    
  #if (debug) print(paste("unit: ",unit))
  
  # replace umlaute
  name <- gsub("Füllstand","Fuellstand",name)
  name <- gsub("Geschw.","Geschwindigkeit",name)
  name <- gsub("T","Temperatur",name)
  
  #if (debug) print(paste("name: ",name))
  
  # read data
  #data <- read.table(file, strip.white=TRUE, dec=deci ,skip=8, 
  #                   blank.lines.skip=TRUE,
  #                   fileEncoding="iso-8859-1", fill=TRUE, header=TRUE, 
  #                   na.strings=c("#-1","//Uhrzeit verstellt"))
  
  data <- try(utils::read.table(file, 
                                strip.white = TRUE, 
                                sep = "\t",
                                dec = deci,
                                skip = 8,
                                blank.lines.skip = TRUE,
                                fileEncoding = "iso-8859-1",
                                fill = TRUE,
                                header = TRUE, 
                                na.strings = c("#-1","//Uhrzeit verstellt"),
                                quote = "\"",
                                comment.char = ""))
  
  # check errors
  if (inherits(data, 'try-error')) return(NULL)
  
  #if (debug) debug.data <<- data
  
  ncol <- min(ncol, ncol(data) - 3)

  #if (debug) print(paste("ncol: ",ncol))
  
  # convert data.frame to numeric matrix
  coredata <- data.matrix(data[, 3:(3 + ncol - 1)])
  
  #if (debug) debug.coredata <<- coredata
  
  # convert timedata as POSIXct
  raw_timestamp <- paste(as.character(data[,1]), 
                         as.character(data[,2]))
  
  timedata <- as.POSIXct(raw_timestamp, 
                         format = "%d.%m.%Y %H:%M:%S",
                         tz = "GMT", 
                         origin = "1970-01-01")
  
  # check if index conversion succeeded
  .check_index_conversion(index = timedata,
                          raw_timestamp = raw_timestamp, 
                          verbose = verbose)
  
  #if (debug) debug.timedata <<- timedata
  
  # detect na rows to prevent NA indices
  na.rows <- which(is.na(timedata))
  
  # if na.rows equals integer(0), so no NA indices, skip removal of NA 
  if (!identical(na.rows,integer(0))) {
    coredata <- coredata[-na.rows,]
    timedata <- timedata[-na.rows]
  }
  
  # create list of xts
  xts.list <- lapply(seq(ncol(coredata)), 
                     FUN = function(x) {xts::xts(order.by = timedata,
                                                 x = coredata[,x],
                                                 # set Ident Attributes
                                                 Parameter = as.character(name[x]),
                                                 Ort = "",
                                                 Subort = "",
                                                 Defart = "K",
                                                 Aussage = "Mes",
                                                 XDistanz = "E",
                                                 XFaktor = "",
                                                 Herkunft = "O",
                                                 Reihenart = "Z",
                                                 Version = "0",
                                                 Quelle = "L",
                                                 # set Descr Atttributes 
                                                 X = "",
                                                 Y = "",
                                                 Hoehe = "",
                                                 Messgenau = "",
                                                 FToleranz = "",
                                                 NWGrenze = "",
                                                 Einheit = as.character(unit[x]),
                                                 Kommentar = gsub(" ", ".", 
                                                                  paste(Sys.time(),
                                                                        "NIVUS Datenlogger"))
                                                 )
    
    
  })
  
  # setup names
  names(xts.list) <- name[seq(ncol)]
  
  if (!is.null(parameter)) xts.list <- xts.list[which(names(xts.list) %in% parameter)]
  
  invisible(xts.list)
}
