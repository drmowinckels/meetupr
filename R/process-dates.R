.date_helper <- function(time) {
  if (is.character(time)) {
    time <- tryCatch(expr = as.numeric(time), error = function(e) {
      warning("One or more dates could not be converted properly")
      return(NA)
    })
  }

  if (!is.numeric(time)) {
    warning("One or more dates could not be converted properly")
    return(rep(NA, length(time)))
  }

  seconds <- time / 1000
  as.POSIXct(seconds, origin = "1970-01-01")
}

process_datetime_fields <- function(dt, fields) {
  existing_fields <- intersect(fields, names(dt))
  dt[existing_fields] <- lapply(dt[existing_fields], anytime::anytime)
  dt
}
