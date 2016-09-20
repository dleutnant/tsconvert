#' Reads the .txt structured time series data output usually used at 
#' sps of waste water treatment plants.
#'
#' @title Read time series data of type .txt (SPS)
#' @param file The file to be read
#' @param verbose logical. Should informative outputs printed during function evaluation?
#' @return A list of xts-objects.
#' @keywords internal.
#' @seealso \code{\link[xts]{xts}}.
read_sps <- function(file, verbose=FALSE) {
  
  #file <- "testdata/SPS/2000212VonMZ150101BisMZ150911.txt"
  
  meta_data <- apply(utils::read.csv2(file, 
                                      nrows = 4,
                                      header = F,
                                      stringsAsFactors = F),
                     MARGIN = 2, paste, collapse = "")
  
  # meta data still need to be polished
  meta_data <- unname(meta_data)
  
  data <- utils::read.csv2(file = file, header = T, sep = ";", dec = ",", skip = 4, 
                           stringsAsFactors = F)
  
  # switch to logical
  data$SOMMERZEIT <- ifelse(data$SOMMERZEIT == "N", FALSE, TRUE)
  
  ## times
  times <- as.POSIXct(paste(as.character(data$Datum),as.character(data$Uhrzeit)),
                      format = "%d.%m.%y %H:%M",
                      tz = "GMT",
                      origin = "1970-01-01")
  
  ## data
  data <- data[, c('WERT', 'MINMAXFLAG', 'KORREKFLAG', 'GRENZ_FLAG', 'STOER_FLAG')]

  list_of_xts <- lapply(data, function(x) xts::xts(x = x,
                                                   order.by = times, 
                                                   meta_data = meta_data))
  
  invisible(list_of_xts)
}
