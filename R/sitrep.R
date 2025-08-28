#' Show meetupr authentication status
#' @export
meetup_sitrep <- function() {
  cli::cli_h2("meetupr authentication status")

  tryCatch(
    {
      client <- meetup_client()
      cli::cli_alert_success("Credentials configured correctly")
    },
    error = function(e) {
      cli::cli_alert_danger("Credential issue: {e$message}")
      cli::cli_h3("Setup instructions:")
      cli::cli_ol(c(
        "Create OAuth client at {.url https://secure.meetup.com/meetup_api/oauth_consumers/}",
        "Set redirect URI to: {.strong http://localhost:1410}",
        "Set environment variables:",
        "  {.code Sys.setenv(MEETUP_CLIENT_ID = 'your_client_id')}",
        "  {.code Sys.setenv(MEETUP_CLIENT_SECRET = 'your_client_secret')}"
      ))
    }
  )

  cli::cli_alert_info(
    "Authentication handled automatically when making requests"
  )

  # Debug status
  debug_enabled <- nzchar(Sys.getenv("MEETUPR_DEBUG"))
  if (debug_enabled) {
    cli::cli_alert_info("Debug mode: {.strong enabled} (set MEETUPR_DEBUG='')")
  } else {
    cli::cli_alert_info(
      "Debug mode: disabled (set {.envvar MEETUPR_DEBUG=1} to enable)"
    )
  }
}

#' Get/set verbose option
#' @param verbose Logical for verbose output
#' @export
meetupr_verbose <- function(verbose = NULL) {
  if (is.null(verbose)) {
    getOption("meetupr.verbose", FALSE)
  } else {
    options(meetupr.verbose = verbose)
    invisible(verbose)
  }
}
