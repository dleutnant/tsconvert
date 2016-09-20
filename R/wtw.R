#' Reads WTW's structured .csv time series data and creates a list of xts-objects.
#'
#' @title Read time series data from WTW
#' @param file The file to be read
#' @param verbose logical. Provide additional details?
#' @return A list of xts-objects.
#' @rdname read_wtw
#' @export 
#' @seealso \code{\link[xts]{xts}}.
read_wtw <- function(file, verbose=FALSE){
  
  if (verbose) print(file)
  
  xts.list <- list()
  cn  <- character()
  
  #get separator:
  sep  <- substring((scan(file,
                          what = "character",
                          nmax = 1,
                          quiet = TRUE,
                          fileEncoding = "iso-8859-1"))[1],4,4)
  
  if (is.na(sep)) {
    sep  <- substring((scan(file,
                            what = "character",
                            nmax = 1,
                            quiet = TRUE,
                            fileEncoding = "UCS-2LE"))[1],4,4)
  }
  
  if (sep == ";") {
    fE  <- "iso-8859-1"
    data <- utils::read.csv2(file, 
                             header = FALSE, 
                             sep = ";",
                             dec = ",",
                             skip = 1,
                             fileEncoding = fE,
                             stringsAsFactors = F)
  } else {
    fE  <- "UCS-2LE"
    data <- utils::read.csv(file, 
                            header = FALSE,
                            sep = "\t",
                            dec = ",",
                            skip = 1,
                            fileEncoding = fE,
                            stringsAsFactors = F) 
  }
  # extract sensorname and sensorid
  sensorname <- scan(file,
                     what = "character",
                     nmax = 3,
                     sep = sep,
                     quiet = TRUE,
                     fileEncoding = fE)[2]
  
  sensorid <- scan(file,
                   what = "character",
                   nmax = 3,
                   sep = sep,
                   quiet = TRUE,
                   fileEncoding = fE)[3]
  
  # reorder data and remove duplicates
  data <- data[order(data$V1),]
  data <- data[!duplicated(data$V1),]
  coredata <- data.matrix(data[,c(3,7)])
  unitdata <- as.character(data[1,c(4,8)])
  timedata <- as.POSIXct(as.character(data[,1]),
                         format = "%Y-%m-%d %H:%M",
                         tz = "GMT",
                         origin = "1970-01-01")
  
  # check if index conversion succeeded
  .check_index_conversion(index = timedata, 
                          raw_timestamp = as.character(data[,1]),
                          verbose = verbose)
  
  # set parameter names
  tmp <- substring(data[1,5],
                   first = 1,
                   last = 1)  
  
  if (tmp == "F")  parname  <- "Fuellstand.mA"
  if (tmp == "T")  parname  <- "Truebung"
  if (tmp == "L")  parname  <- "Leitfaehigkeit"
  if (tmp == "p")  parname  <- "pH"
  if (tmp == "D")  parname  <- "Durchfluss" #only occurs at site "flachbau"
  
  par2name <- ""
  if ((sensorname == "T") || (sensorname == "TetraCon700IQ")) {
    tmp2 <- "Leitfaehigkeit"
    par2name <- paste0("temp@", tmp2)
  }
  if ((sensorname == "S") || (sensorname == "SensoLyt700IQ")) {
    tmp2 <- "pH" 
    par2name <- paste0("temp@", tmp2)
  }
  
  # set colnames
  colnames(coredata)   <- c(parname, par2name)
  
  ## NA Handling
  nav <- 4.38331e-038
  coredata[,1][which(coredata[,1] == nav)]  <- NA
  coredata[,2][which(coredata[,2] == nav)]  <- NA
  
  # create xts object with both parameter
  wtw <- xts::xts(coredata, order.by = timedata)
  
  # remove rows where both index and at least one paramter is NA
  wtw <- wtw[which( (!is.na(wtw[,1]) | !is.na(wtw[,2])) & !is.na(zoo::index(wtw)) )]
  
  # set ncol for list
  ncol.wtw <- ifelse(par2name == "",1,2)
    
  #convert to list object
  for (i in seq_len(ncol.wtw)) {

    wtw.xts <- wtw[,i]
    
    # set Ident Attributes
    attributes(wtw.xts)$Parameter   <- cn[i] <- colnames(wtw)[i]
    attributes(wtw.xts)$Ort         <- ""
    attributes(wtw.xts)$Subort      <- ""
    attributes(wtw.xts)$Defart      <- "K"
    attributes(wtw.xts)$Aussage     <- "Mes"
    attributes(wtw.xts)$XDistanz    <- "E"# tick = 5 secVB
    attributes(wtw.xts)$XFaktor     <- "12" # to do: automatic with periodicity
    attributes(wtw.xts)$Herkunft    <- "O"
    attributes(wtw.xts)$Reihenart   <- "Z"
    attributes(wtw.xts)$Version     <- "0"
    attributes(wtw.xts)$Quelle      <- "L"
    
    # set Descr Atttributes 
    attributes(wtw.xts)$X           <- ""
    attributes(wtw.xts)$Y           <- ""
    attributes(wtw.xts)$Hoehe       <- ""
    attributes(wtw.xts)$Messgenau   <- ""
    attributes(wtw.xts)$FToleranz   <- ""
    attributes(wtw.xts)$NWGrenze    <- "2"
    attributes(wtw.xts)$Einheit     <- unitdata[i]
    attributes(wtw.xts)$Kommentar   <- gsub(" ", ".", paste(Sys.time(),
                                                            sensorname, 
                                                            sensorid)) 
    
    xts.list[[i]]  <- wtw.xts
    
  }
  
  # rename list elements
  names(xts.list) <- cn 
  invisible(xts.list)
  
}