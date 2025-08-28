meetup_auth <- function(
  client_id = Sys.getenv("MEETUP_CLIENT_ID"),
  client_secret = Sys.getenv("MEETUP_CLIENT_SECRET"),
  token = NULL,
  cache = TRUE,
  use_appdir = TRUE,
  token_path = NULL,
  parent_frame = parent.frame()
) {
  if (is_testing()) {
    .state$token <- create_mock_token()
    return(invisible(.state$token))
  }

  if (!is.null(token)) {
    processed_token <- handle_token_input(token)
    .state$token <- processed_token
    return(invisible(processed_token))
  }

  if (!nzchar(client_id) || !nzchar(client_secret)) {
    cli::cli_abort(
      c(
        "Meetup client ID and secret are required.",
        "Set {.val MEETUP_CLIENT_ID} and {.val MEETUP_CLIENT_SECRET} environment variables or ",
        "provide them as arguments.",
        "Should you set {.code cache} to {.val TRUE}?"
      ),
      .envir = parent_frame
    )
  }

  client <- httr2::oauth_client(
    id = client_id,
    secret = client_secret,
    token_url = "https://secure.meetup.com/oauth2/access"
  )

  if (cache) {
    if (use_appdir && is.null(token_path)) {
      token_path <- appdir_path()
      parent <- dirname(token_path)
      if (!file.exists(parent)) dir.create(parent, recursive = TRUE)
    }

    if (is.null(token_path)) {
      token_path <- ".httr2-oauth-meetup"
    }
  } else {
    token_path <- NULL
  }

  meetup_token <- httr2::oauth_flow_auth_code(
    client = client,
    auth_url = "https://secure.meetup.com/oauth2/authorize",
    cache_disk = token_path,
    scope = NULL
  )

  .state$token <- meetup_token
  invisible(meetup_token)
}

meetup_token <- function(verbose = FALSE) {
  if (is_testing()) {
    return(create_mock_token())
  }

  if (!token_available(verbose = verbose)) {
    meetup_auth(verbose = verbose)
  }

  .state$token
}

meetup_deauth <- function() {
  .state$token <- NULL
  invisible(TRUE)
}
