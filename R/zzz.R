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

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("meetupr: Access to Meetup.com data via GraphQL API")

  if (
    !nzchar(Sys.getenv("MEETUP_CLIENT_ID")) &&
      !nzchar(Sys.getenv("MEETUP_CLIENT_SECRET")) &&
      !is_testing()
  ) {
    packageStartupMessage(
      "To use meetupr, you need to set up authentication.\n",
      "See ?meetup_auth for details."
    )
  }

  invisible()
}
