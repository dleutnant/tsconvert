#' Writes xts objects to file

#' @title Write xts objects to file
#' @param xts The xts object to write.
#' @param file A connection, or a character string naming the file to write to.
#'  If file is "", print to the standard output connection.
#' @param format The time format.
#' @param sep The field separator string. Values within each row of x are 
#' separated by this string.
#' @param dec The string to use for decimal points in numeric or complex 
#' columns: must be a single character.
#' @rdname write_xts
#' @export 
#' @seealso \code{\link[xts]{xts}}.
write_xts <- function(xts, file="", format= "%Y-%m-%d %H:%M:%S", sep=";", dec = ".") {
  
  utils::write.table(data.frame(Index = format.POSIXct(zoo::index(xts), 
                                                       format = format), 
                                zoo::coredata(xts)),
                     file = file, 
                     dec = dec, 
                     sep = sep, 
                     row.names = FALSE,
                     quote = FALSE)
  
}