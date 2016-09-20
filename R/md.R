#' Reads the .md structured rainfall time series data output usually used by 
#' German stormwater quality models and creates an xts-object.
#'
#' @title Read rainfall time series data of type .md
#' @param file The file to be read.
#' @param verbose logical. Provide additional details?
#' @return An xts-object.
#' @rdname read_md
#' @export 
#' @seealso \code{\link[xts]{xts}}.
read_md <- function(file, verbose=FALSE) {
  
  ## if file == vector,  rbind!
  file <- "testdata/md/NFLACN13.dat"
  # data <- read.fwf(file,widths = c(5, 10, 5, rep(5, 12)),skip = 3, 
  #                  stringsAsFactors = FALSE, strip.white = TRUE)
  # 
  # ## times
  # timedata <- as.POSIXct(paste(as.character(data$V2),as.character(data$V3)),
  #                        format = "%d.%m.%Y %H",
  #                        tz = "GMT",
  #                        origin = "1970-01-01")
  # 
  # times <- as.POSIXct(sapply(timedata,  
  #                            function(x) seq(x, x + 11*5*60, by = 5*60)),
  #                     origin = "1970-01-01",
  #                     tz = "GMT")
  # 
  # ## data
  # m <- c(t(as.matrix(data[,4:15])))
  # m <- m/1000
  # 
  # ## xts
  # ts <- xts::xts(x = m, order.by = times)
  # 
  #invisible(ts)
  message("currently not implemented")
  
}

#' Writes the .md structured rainfall time series data usually used by 
#' German stormwater quality models.
#'
#' @title Write rainfall time series to .md file format.
#' @param xts The xts object to be written
#' @param file A character string naming the file to write to.
#' @param verbose logical. Provide additional details?
#' @rdname write_md
#' @export 
#' @seealso \code{\link[xts]{xts}}.
write_md <- function(xts, file, verbose=FALSE) {
  
  message("currently not implemented")

  
}
