normalize_field_names <- function(.data) {
  names(.data) <- gsub("\\.", "_", names(.data))
  .data
}

rename <- function(.data, ...) {
  args <- rlang::enexprs(...)

  for (arg in 1:length(args)) {
    idx <- match(rlang::as_label(args[[arg]]), names(.data))
    if (length(idx) != 1) {
      next
    }
    names(.data)[idx] <- names(args)[arg]
  }
  .data
}

remove <- function(.data, ...) {
  args <- rlang::enexprs(...)

  for (arg in 1:length(args)) {
    col <- rlang::as_label(args[[arg]])
    idx <- match(col, names(.data))
    if (length(idx) != 1) {
      next
    }
    .data[, col] <- NULL
  }
  .data
}

data_to_tbl <- function(data) {
  dplyr::bind_rows(
    lapply(data, function(data_item) {
      rlist::list.flatten(data_item)
    })
  )
}

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
