.collapse <- function(x) {
  paste(x, collapse = ",")
}

capture_str <- function(x) {
  paste0(
    utils::capture.output(utils::str(x)),
    collapse = "\n"
  )
}
