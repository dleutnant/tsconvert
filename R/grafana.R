#' Reads the .csv structured time series data produced by an grafana export and 
#' creates a list of xts-objects.
#'
#' @title Read time series data from .csv (grafana) file format
#' @param file The file to be read.
#' @param verbose Provide additional details?
#' @return A list of xts-objects.
#' @rdname read_grafana
#' @export 
#' @seealso \code{\link[xts]{xts}}.
read_grafana <- function(file, verbose=FALSE) {
  
  message("please export data from grafana as 'series as rows'.")
  
  data <- utils::read.table(file, header = T, sep = ";", dec = ".", 
                            na.strings = "null",
                            stringsAsFactors = F)

  # split data by series names
  list_of_data <- split(data, data$Series)
  
  list_of_xts <- lapply(list_of_data, function(x) {
    
    index <- as.POSIXct(x$Time, 
                        format = "%Y-%m-%dT%H:%M:%S",
                        tz = "GMT")
    
    # check if index conversion succeeded
    .check_index_conversion(index = index,
                            raw_timestamp = x$Time, 
                            verbose = verbose)
    
    # create xts-object
    xts <- xts::xts(x = x$Value, order.by = index, Series = unique(x$Series))
  
  })
  
  invisible(list_of_xts)
}