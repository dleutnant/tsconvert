#' Reads the .tsf structured time series data (e.g., PCSWMM) and creates an xts-object.
#'
#' @title Read time series data from .tsf file format
#' @param file The file to be read
#' @return An xts-object.
#' @rdname read_tsf
#' @export 
#' @seealso \code{\link[xts]{xts}}.
read_tsf <- function(file) {
  
  data <- utils::read.table(file, header = F, sep = "\t", dec = ".", 
                            stringsAsFactors = F)
  
  # without am/pm
  if (data[3,1] == "dd.MM.yyyy") date_time_format <- "%d.%m.%Y %H:%M:%S"
  
  # with am/pm
  if (data[3,1] == "M/d/yyyy") date_time_format <- "%m/%d/%Y %I:%M:%S %p"
  
  
  coredata  <- as.numeric(data[-c(1:3),2])
  timedata <- as.POSIXct(data[-c(1:3),1], 
                         format = date_time_format,
                         tz = "GMT")
  
  list.of.xts <- xts::xts(x = coredata, order.by = timedata)
  
  attr(list.of.xts, "ID") <- data[1,2]
  attr(list.of.xts, "parameter") <- data[2,2]
  attr(list.of.xts, "unit") <- data[3,2]
  
  invisible(list.of.xts)
  
}

#' Writes xts objects to .tsf structured time series data.
#'
#' @title Write xts-objects to .tsf file format
#' @param xts The xts-object object to be written.
#' @param file A character string naming the file to write to.
#' @param ID Sets IDs of time series data.
#' @param parameter Sets parameters of the time series data.
#' @param unit Sets units of the time series data.
#' @rdname write_tsf
#' @export
#' @seealso \code{\link{xts}}.
write_tsf <- function(xts, file, ID, parameter, unit) {
  
  
  if (ncol(xts) != length(ID)) stop("missing IDs")
  if (ncol(xts) != length(parameter)) stop("missing parameters")
  if (ncol(xts) != length(unit)) stop("missing units")
  
  
  first_line <- paste("IDs:",
                      paste(ID, collapse = "\t"),
                      sep = "\t")
  
  second_line <- paste("Date/Time",
                       paste(parameter, collapse = "\t"),
                       sep = "\t")
  
  third_line <- paste("dd.MM.yyyy",
                      paste(unit, collapse = "\t"),
                      sep = "\t")
  
  utils::write.table(rbind(first_line, second_line, third_line),
                     file = file,
                     sep = "\t",
                     quote = F,
                     row.names = F, 
                     col.names = F)
  
  utils::write.table(data.frame(format.POSIXct(zoo::index(xts),
                                               format = "%d.%m.%Y %H:%M:%S"),
                                zoo::coredata(xts)),
                     append = T,
                     file = file,
                     sep = "\t",
                     quote = F,
                     row.names = F, 
                     col.names = F)
  
}


