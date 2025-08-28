is_testing <- function() {
  nzchar(Sys.getenv("MEETUPR_TESTING"))
}

create_mock_token <- function() {
  structure(
    list(access_token = "test-token"),
    class = "httr2_token"
  )
}

validate_token <- function(token) {
  inherits(token, "httr2_token")
}

handle_token_input <- function(token, parent_frame = parent.frame()) {
  if (inherits(token, "httr2_token")) {
    return(token)
  }

  if (is.character(token) && length(token) == 1) {
    if (!file.exists(token)) {
      cli::cli_abort(
        "Token file does not exist: {.path token}",
        .envir = parent_frame
      )
    }

    cached_token <- load_cached_token(token)
    if (is.null(cached_token)) {
      cli::cli_abort(
        "File does not contain a proper httr2 token: {.path token}",
        .envir = parent_frame
      )
    }

    return(cached_token)
  }

  cli::cli_abort(
    "Input provided via {.val token} is neither a token nor a path to an {.val .rds} file containing a token.",
    .envir = parent_frame
  )
}
