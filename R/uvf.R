#' Reads the .uvf structured time series data and creates a list of xts-objects.
#'
#' @title Read time series data from .uvf file format
#' @param file The file to be read
#' @param verbose logical. Provide additional details?
#' @param return.comments logical. Should comments within the file be returned?
#' @return A list of xts-objects.
#' @rdname read_uvf
#' @export 
#' @seealso \code{\link[xts]{xts}}.
read_uvf <- function(file, verbose=TRUE, return.comments=FALSE) {
  
  list.of.xts <- list()
  xts.attributes <- xts()
  
  conn <- file(file,open = "r")
  
  lines <- readLines(conn)
  
  blocks <- which(stringr::str_count(string = lines,pattern = "[$]") == 1)
  blocks <- c(1,blocks[ 1 + which( diff(blocks) != 1)], length(lines))
  
  for (b in 1:(length(blocks) - 1)) {
    
    i <- 1
    
    from <- blocks[b]
    to <- blocks[b + 1]
    
    linn <- lines[from:(to - 1)]
    
    while (identical(substr(x = linn[i], start = 1,stop = 1),"$")) {
      
      type <- substr(x = linn[i], start = 2,stop = 2)
      values <- unlist(strsplit(linn[i],split = " "))
      Attribut.Name <- sub(":", "", values[2])
      Attribut.Wert <- values[3]
      if (type == "f") Attribut.Wert <- as.numeric(Attribut.Wert)
      attr(xts.attributes, Attribut.Name) <-  Attribut.Wert
      i  <- i + 1
    }
    
    #handle coredata
    
    KEN <- stringr::str_trim(linn[i])
    
    i <- i + 1 
    
    attr(xts.attributes, "PARAMETER") <- stringr::str_trim(substr(linn[i],1,15))
    attr(xts.attributes, "EINHEIT.WERTE") <- stringr::str_trim(substr(linn[i],16,30))
    attr(xts.attributes, "ANF100") <- as.numeric(substr(linn[i],31,34))
    attr(xts.attributes, "END100") <- as.numeric(substr(linn[i],36,39))
    
    i <- i + 1
    
    attr(xts.attributes, "NUMMER") <- stringr::str_trim(substr(linn[i],1,15))
    attr(xts.attributes, "XRECHTS") <- as.numeric(substr(linn[i],16,25))
    attr(xts.attributes, "YHOCH") <- as.numeric(substr(linn[i],26,35))
    attr(xts.attributes, "HOEHE") <- as.numeric(substr(linn[i],36,45))
    
    i  <- i + 1
    
    MIN <- switch(KEN,
                  "*Z" = as.POSIXct(strptime(x = substr(linn[i],1,10),
                                             format = "%y%m%d%H%M",
                                             tz = "GMT"),
                                    origin = "1970-01-01"),
                  "*O" =  as.numeric(substr(linn[i],1,10)))
    
    attr(xts.attributes, "MIN") <- MIN
    
    
    MAX <- switch(KEN,
                  "*Z" = as.POSIXct(strptime(x = substr(linn[i],11,20),
                                             format = "%y%m%d%H%M",
                                             tz = "GMT"),
                                    origin = "1970-01-01"),
                  "*O" =  as.numeric(substr(linn[i],11,20)))
    
    attr(xts.attributes, "MAX") <- MAX
    
    attr(xts.attributes, "EINHEIT.ARGUMENTE") <- stringr::str_trim(substr(linn[i],21,28))
    
    i <- i + 1
    
    X <- switch(KEN,
                "*Z" = as.POSIXct(strptime(x = substr(linn[i:length(linn)],1,10),
                                           format = "%y%m%d%H%M",
                                           tz = "GMT"),
                                  origin = "1970-01-01"),
                "*O" =  as.numeric(substr(linn[i:length(linn)],1,10)))
    
    Y <- substr(linn[i:length(linn)],11,20)
    
    data <- as.numeric(Y)
    data[data == -777] <- NA
    xts <- xts::xts(x = data, order.by = X)
    xts::xtsAttributes(xts)  <- xts::xtsAttributes(xts.attributes)
    
    if (return.comments) {
      KOMMENTAR <- stringr::str_sub(linn[i:length(linn)],21)
      KOMMENTAR.xts <- xts::xts(x = KOMMENTAR, order.by = X)
      xts::xtsAttributes(KOMMENTAR.xts)  <- xts::xtsAttributes(xts.attributes)
      xts <- list(xts,KOMMENTAR.xts)
    }
    
    list.of.xts[[b]] <- xts
    
  }
  
  close(conn)
  
  if (b == 1) list.of.xts <- list.of.xts[[1]]
  
  invisible(list.of.xts)
 
}

#' Writes xts or matrix objects to .uvf structured time series data.
#'
#' @title Write xts- or matrix-objects to .uvf file format
#' @param data The data object to be written
#' @param file A character string naming the file to write to.
#' @param function.interpretation The type of function interpretation. Must be: 'Linie', 'Summenlinie', 'Blockanfang', 'Blockende' or 'Punkt'
#' @param measuring.unit Unit of measurement
#' @param arguments.unit Unit of argument
#' @param parameter Name of parameter
#' @param id Number of identification
#' @param easting Easting
#' @param northing Northing
#' @param elevation Elevation
#' @param verbose logical. Provide additional details?
#' @rdname write_uvf
#' @export 
#' @seealso \code{\link{xts}}.
write_uvf <- function(data, file, 
                      function.interpretation, 
                      parameter, id, 
                      measuring.unit, arguments.unit="time",
                      easting = NA, northing = NA, elevation = NA,
                      verbose=TRUE) {
  
  if (!function.interpretation %in% c("Linie","Summenlinie","Blockanfang",
                                      "Blockende","Punkt")) {
    stop("bad function.interpretation Must be: 'Linie', 'Summenlinie', 
         'Blockanfang', 'Blockende' or 'Punkt'")
  }

  
  if (xts::is.xts(data)) {
    
    KEN <- "*Z"
    ind <- strftime(zoo::index(data), format = "%y%m%d%H%M")
    cor <- round(zoo::coredata(data),4)
    cor[is.na(cor)] <- -777
    ANF100 <- unlist(strsplit(as.character(as.numeric(xts::first(lubridate::year(data))) %/% 100 * 100),
                              NULL))
    END100 <- unlist(strsplit(as.character(as.numeric(xts::last(lubridate::year(data))) %/% 100 * 100),
                              NULL))
    MIN <- unlist(strsplit(as.character(strftime(min(zoo::index(data)),
                                                 format = "%y%m%d%H%M")),
                           NULL))
    MAX <- unlist(strsplit(as.character(strftime(max(zoo::index(data)),
                                                 format = "%y%m%d%H%M")),
                           NULL))
    
  } else {
    
    if (is.matrix(data)) {
      
      if (ncol(data) > 2) {
        warning("ncol(data) > 2, setting column one as x and column two as y")
      }
      
      KEN <- "*O"
      ind <- round(data[,1],10)
      cor <- round(data[,2],10)
      cor[is.na(cor)] <- -777
      
      ANF100 <- 1900
      END100 <- 1900
      MIN <- min(ind)
      MAX <- max(ind)
      
    } else {
      
      stop("Data object is neither xts nor matrix.")
      
    }

  }
  
  line.f.interpretation <- paste("$ib Funktion-Interpretation:",
                                 function.interpretation)
  line.mess.einheit <- paste("$sb Mess-Einheit:", measuring.unit)
  line.mess.station <- paste("$sb Mess-Stellennummer:", id)
  line.mess.groesse <- paste("$sb Mess-Groesse:", parameter)

  PARAMETER <- unlist(strsplit(parameter, NULL)) 
  EINHEIT.WERTE <- unlist(strsplit(measuring.unit, NULL))
  EINHEIT.ARGUMENTE <- unlist(strsplit(arguments.unit, NULL))
  NUMMER <- unlist(strsplit(id, NULL))
  XRECHTS <- unlist(strsplit(as.character(easting), NULL))
  YHOCH <- unlist(strsplit(as.character(northing), NULL))
  HOEHE <- unlist(strsplit(as.character(elevation), NULL))
    
  line.39 <- character(39)
  line.39[1:min(length(PARAMETER), 15)] <- PARAMETER
  line.39[16:min(length(EINHEIT.WERTE) + 15, 30)] <- EINHEIT.WERTE
  line.39[31:min(length(ANF100) + 30, 34)] <- ANF100
  line.39[36:min(length(END100) + 35, 39)] <- END100
  line.39[which(line.39 == "")] <- " "
  line.39 <- paste(line.39, collapse = "")
  
  line.45 <- character(45)
  line.45[1:min(length(NUMMER), 15)] <- NUMMER
  line.45[16:min(length(XRECHTS) + 15, 25)] <- XRECHTS
  line.45[26:min(length(YHOCH) + 25, 35)] <- YHOCH
  line.45[36:min(length(HOEHE) + 35, 45)] <- HOEHE
  line.45[which(line.45 == "")] <- " "
  line.45 <- paste(line.45, collapse = "")
  
  line.28 <- character(28)
  line.28[1:min(length(MIN), 10)] <- MIN
  line.28[11:min(length(MAX) + 10, 20)] <- MAX
  line.28[21:min(length(EINHEIT.ARGUMENTE) + 20, 28)] <- EINHEIT.ARGUMENTE
  line.28[which(line.28 == "")] <- " "
  line.28 <- paste(line.28, collapse = "")

  utils::write.table(rbind(line.mess.station ,line.mess.groesse, line.mess.einheit, 
                           line.f.interpretation, KEN, line.39,line.45, line.28),
                     file = file,
                     sep = " ",
                     quote = F,
                     row.names = F,
                     col.names = F)

  utils::write.table(paste0(format(ind,
                            width = 10,
                            justify = "right"), 
                     format(cor, width = 10, justify = "right")),
                     append = T,
                     file = file,
                     sep = " ",
                     quote = F,
                     row.names = F, 
                     col.names = F)
  
}


