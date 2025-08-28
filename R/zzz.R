.onLoad <- function(libname, pkgname) {
  op <- options()
  op.meetupr <- list(
    meetupr.verbose = FALSE
  )
  toset <- !(names(op.meetupr) %in% names(op))
  if (any(toset)) {
    options(op.meetupr[toset])
  }

  invisible()
}
