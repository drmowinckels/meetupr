.state <- new.env(parent = emptyenv())

token_available <- function(verbose = TRUE) {
  if (is.null(.state$token)) {
    if (verbose) {
      token_path <- meetup_token_path()
      if (!is.null(token_path) && file.exists(token_path)) {
        message(
          "A token file exists.\nWhen/if needed, the credentials cached will be used for this session.\nOr run ",
          "meetup_auth() for explicit authentication and authorization."
        )
      }
    }
    return(FALSE)
  }
  TRUE
}

#' @export
meetupr_verbose <- function(verbose = NULL) {
  if (is.null(verbose)) {
    getOption("meetupr.verbose", FALSE)
  } else {
    options(meetupr.verbose = verbose)
    invisible(verbose)
  }
}

#' @export
meetup_query <- function(.query, ..., .token = meetup_token()) {
  graphql_query(.query = .query, ..., .token = .token)
}
