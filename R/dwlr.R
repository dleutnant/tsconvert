#' Reads the .dwlr structured time series data and creates an xts-object.
#'
#' @title Read time series data from .dwlr file format
#' @param file The file to be read.
#' @param verbose Provide additional details?
#' @param sep The field separator character.
#' @param dec The character used in the file for decimal points.
#' @return An xts-object.
#' @rdname read_dwlr
#' @export 
#' @seealso \code{\link[xts]{xts}}.
read_dwlr <- function(file, verbose=FALSE, sep = ";", dec = ".") {
  
  #file <- "testdata/dwlr/GD_1_20150828"
  
  col_names <- c("Zeitstempel", "Datensatznummer", "Wasserstand_m", 
                 "Druck_bar", "Druck_Pa", "Abstich_m", 
                 "Temperatur_grad_celsius")
  
  data <- utils::read.table(file, header = T, sep = sep, dec = dec, 
                            col.names = col_names,
                            stringsAsFactors = F)
  
  index <- as.POSIXct(data[,1], 
                         format = "%d.%m.%Y %H:%M:%S",
                         tz = "GMT")
  
  # check if index conversion succeeded
  .check_index_conversion(index = index,
                          raw_timestamp = data[,1], 
                          verbose = verbose)
  
  # extract coredata
  coredata <- data[,-1]
  
  # create xts-object
  xts <- xts::xts(x = coredata, order.by = index)
  
  invisible(xts)
}