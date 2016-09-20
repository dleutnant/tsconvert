# function is used to check if an index conversion succeeded
.check_index_conversion <- function(index, raw_timestamp, verbose=FALSE) {

  # give a warning if timestamps couldn't coerce to POSIXct
  if (any(is.na(index))) {
    
    warning(paste(length(which(is.na(index))),
                  "timestamps couldn't coerced to POSIXct."))
    
    # print erroneous timestamps 
    if (verbose) {
      
      error_df <- data.frame(line = which(is.na(index)),
                             raw_timestamp = raw_timestamp[is.na(index)])
      
      print(error_df)
      
    }
    
  }
  
}

# function is used to shift the index of an xts-object by seconds.
.shift.index <- function(xts, seconds, tz="GMT"){
  
  from  <- xts::first(zoo::index(xts))
  to  <- xts::last(zoo::index(xts))
  
  Sys.setenv(TZ = tz)
  begin.ep <- match(as.POSIXct(from, tz = tz), zoo::index(xts))
  end.ep <- match(as.POSIXct(to, tz = tz), zoo::index(xts))
  zoo::index(xts)[begin.ep:end.ep]  <- zoo::index(xts)[begin.ep:end.ep] + lubridate::seconds(seconds)
  
  return(xts)
  
}