#' Reads Ott's structured .OML or .gz time series data and creates a list of 
#' xts-objects.
#'
#' @title Read time series data from OTT
#' @param file The file to be read
#' @param verbose logical. Should informative outputs printed during function evaluation?
#' @return A list of xts-objects.
#' @rdname read_ott
#' @export 
#' @seealso \code{\link[xts]{xts}}, \code{\link[XML]{xmlInternalTreeParse}}.
read_ott <- function(file, verbose=FALSE) {
  
  if (!requireNamespace("XML", quietly = TRUE)) {
    stop("XML needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  if (verbose) print(file)
  
  doc <- tryCatch( XML::xmlInternalTreeParse(file, asText = FALSE, 
                                             getDTD = FALSE, 
                                             error = NULL),
            XMLError = function(e) {
              cat("There was an error in the XML at line", 
                  e$line, "column", e$col, "\n",
                  e$message, "\n")
            })
  
  # if xml is not readable
  if (is.null(doc)) return(NULL)
    
  channel <- XML::getNodeSet(doc, "//StationDataList/StationData/ChannelData")
  XML::free(doc)
  rm(doc)
  ids <- ""
  xts.list <- list()

  for (i in seq_along(channel)) {
    
    #create subdoc for avoiding memory leakage
    subdoc <- XML::xmlDoc(channel[[i]])
    
    #extract information
    data <- as.numeric(XML::xpathSApply(subdoc,'//Values/VT', XML::xmlValue))
    timedata <- as.vector(XML::xpathSApply(subdoc,'//Values/VT', 
                                           function(x) XML::xmlAttrs(x)['t']))
    errorcode  <- as.numeric(XML::xpathSApply(subdoc,'//Values/VT',
                                              function(x) XML::xmlAttrs(x)['errorcode']))
    channelid <-  XML::xmlAttrs(XML::xmlRoot(subdoc))['channelId']
    name  <- XML::xmlAttrs(XML::xmlRoot(subdoc))['name']
    unit  <- XML::xmlAttrs(XML::xmlRoot(subdoc))['unit']
    storageInterval  <- XML::xmlAttrs(XML::xmlRoot(subdoc))['storageInterval']
 
    # set NA where error occured and give a warning 
    
    # Errorcode Meaning
    # 01 A/D conversion fault
    # 02 Communication error on sensor interface
    # 03 Over/Underflow
    # 05 Line break
    
    if (!all(is.na(errorcode))) warning(paste("errors detected at", name))
    data[which(!is.na(errorcode))] <- NA
    
    XML::free(subdoc)
    gc()
    
    # extract timedata
    raw_timestamp <- as.character(gsub("T"," ",timedata))
    timedata  <- as.POSIXct(raw_timestamp,
                            tz = "GMT",
                            format = "%Y-%m-%d %H:%M:%S",
                            origin = "1970-01-01")
    #timedata <- strptime(timedata, format = "%Y-%m-%d %H:%M:%S")
    
    # check if index conversion succeeded
    .check_index_conversion(index = timedata,
                            raw_timestamp = raw_timestamp,
                            verbose = verbose)
    
    #Set Dummy for NA Data
    if (length(data) == 0 & length(timedata) == 0) {
      data[1]  <- NA
      timedata[1]  <- NA
    }
    
    # create vector with parameter names
    ids  <- c(ids,name)
    
    # create xts
    ott <- xts::xts(data, order.by = timedata)
    
    # set Ident Attributes
    attributes(ott)$Parameter   <- as.character(name)
    attributes(ott)$Ort         <- ""
    attributes(ott)$Subort      <- ""
    attributes(ott)$Defart      <- "K"
    attributes(ott)$Aussage     <- "Mes"
    attributes(ott)$XDistanz    <- "E" # tick = 5 sec
    attributes(ott)$XFaktor     <- as.character(as.numeric(storageInterval) / 5)
    attributes(ott)$Herkunft    <- "O"
    attributes(ott)$Reihenart   <- "Z"
    attributes(ott)$Version     <- "0"
    attributes(ott)$Quelle      <- "L"
    
    # set Descr Atttributes 
    attributes(ott)$X           <- ""
    attributes(ott)$Y           <- ""
    attributes(ott)$Hoehe       <- ""
    attributes(ott)$Messgenau   <- ""
    attributes(ott)$FToleranz   <- ""
    attributes(ott)$NWGrenze    <- ""
    attributes(ott)$Einheit     <- as.character(unit)
    attributes(ott)$Kommentar   <- gsub(" ", ".", paste(Sys.time(), "OTT Datenlogger")) 
    
    xts.list[[i]] <- ott[!is.na(zoo::index(ott))]
  }
  
  # Prepare List Names
  cn <- gsub(" ", "", ids[-1])
  cn <- gsub("-", "", cn)
  cn <- gsub("\\(",".", cn)
  cn <- gsub("..",".", cn, fixed = TRUE)
  cn <- gsub("\\)","", cn)
  
  names(xts.list) <- cn
  for (i in seq_along(xts.list)) attr(xts.list[[i]], 'Parameter') <- as.character(cn[i])
  
  invisible(xts.list)
}