#' Reads netCDF structured time series data and creates a list of xts-objects.
#'
#' @title Read time series data from .nc (KALIMOD) file format
#' @param file The file to be read
#' @param display.overview logical. Should an overview of the netCDF file printed?
#' @param verbose logical. Should informative outputs printed during function evaluation?
#' @return A list of xts-objects.
#' @rdname read_nc_xts
#' @export 
#' @seealso \code{\link[xts]{xts}}, \code{\link[RNetCDF]{open.nc}}.
read_nc_xts <- function(file, display.overview=FALSE, verbose=FALSE) {
  
  if (!requireNamespace("RNetCDF", quietly = TRUE)) {
    stop("RNetCDF needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  if (verbose) print(file)
  
  nc  <-  RNetCDF::open.nc(file)
  
  if (display.overview) RNetCDF::print.nc(nc)
  
  ndims <- RNetCDF::file.inq.nc(nc)$ndims
  nvars <- RNetCDF::file.inq.nc(nc)$nvars
  
  ## information of dimensions
  dim.id <- numeric(ndims)
  dim.name <- character(ndims)
  dim.length <- numeric(ndims)
  dim.unlim <- numeric(ndims)
  
  ##  currently not used, just for the sake of completeness
  for (i in seq_len(ndims)) {
    dim.id[i] <- RNetCDF::dim.inq.nc(nc, i - 1)$id
    dim.name[i] <- RNetCDF::dim.inq.nc(nc, i - 1)$name
    dim.length[i] <- RNetCDF::dim.inq.nc(nc, i - 1)$length
    dim.unlim[i] <- RNetCDF::dim.inq.nc(nc, i - 1)$unlim
  }
  
  ## information of variables
  var.id <- numeric(nvars)
  var.name <- character(nvars) ##  <- currently only used
  var.type <- character(nvars)
  var.ndims <- numeric(nvars)
  var.dimids <- numeric(nvars)
  var.natts <- numeric(nvars)
  
  ls.var <- as.list(1:nvars)
  
  for (i in seq_len(nvars)) {
    
    var.name[i] <- RNetCDF::var.inq.nc(nc, i - 1)$name
    
    natts <- RNetCDF::var.inq.nc(nc, i - 1)$natts
    att.name <- character(natts)
    att.name.value  <- character(natts)
    
    ls.att <- as.list(1:natts)
    
    for (j in seq_len(natts)) {
      
      att.name[j] <- RNetCDF::att.inq.nc(nc,var.name[i], j - 1 )$name
      
      att.name.value[j] <- RNetCDF::att.get.nc(nc,var.name[i], att.name[j])
      
      ls.att[j] <- att.name.value[j]
      
    }
    
    if ("Variable_fuer_Zeit" %in% att.name) {
      
      data <- as.numeric(RNetCDF::var.get.nc(nc, i - 1))
      
      time <- as.numeric(RNetCDF::var.get.nc(nc, 
                                             att.name.value[
                                               which(att.name == 
                                                       "Variable_fuer_Zeit")] ))
      
      time <- (time - 25569) * 86400
      
      time <- as.POSIXct( time, tz = "GMT", origin = "1970-1-1 00:00:00")
      
      values  <- xts::xts(x = data, order.by = time)
      
    } else {
      
      ## extend to handle multiple dimensions
      values <- as.numeric(RNetCDF::var.get.nc(nc, i - 1))
      
    }
    
    ls.att[[natts + 1]]  <- values    
    
    names(ls.att)  <- c(att.name, "Werte")
    
    ls.var[[i]] <- ls.att
    
  }
  
  names(ls.var) <- var.name
  
  invisible(ls.var)
  
}