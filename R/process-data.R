data_to_tbl <- function(data) {
  dplyr::bind_rows(
    lapply(data, function(data_item) {
      rlist::list.flatten(data_item)
    })
  )
}

check_empty_response <- function(dt) {
  is.null(dt) || nrow(dt) == 0
}

handle_empty_response <- function(dt) {
  if (check_empty_response(dt)) {
    return(NULL)
  }
  dt
}
