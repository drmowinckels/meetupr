# Credit to Jenny Bryan for OAuth wisdom.
# OAuth 2.0 authentication using httr2

# environment to store credentials
.state <- new.env(parent = emptyenv())

#' Authorize \code{meetupr}
#'
#' Authorize \code{meetupr} via the OAuth API. You will be directed to a web
#' browser, asked to sign in to your Meetup account, and to grant \code{meetupr}
#' permission to operate on your behalf. By default, these user credentials are
#' saved to a file in your home directory whose path is saved in `.Renviron`
#' as `MEETUPR_PAT`.
#' If you set `use_appdir` to `FALSE` but `cache` to `TRUE`,
#' they are cached in a file named \code{.httr2-oauth} in the current working directory.
#' To control where the file is saved, use `token_path`.
#'
#' @section How to force meetupr to use a given token?:
#'
#' Save this token somewhere on disk (`token_path` argument of `meetup_auth`).
#' Then in future sessions call `meetup_auth()` with `token` set to that path.
#'
#' @section Advanced usage:
#'
#' Most users, most of the time, do not need to call this function explicitly --
#' it will be triggered by the first action that requires authorization. Even
#' when called, the default arguments will often suffice. However, when
#' necessary, this function allows the user to
#'
#' \itemize{
#'   \item force the creation of a new token
#'   \item retrieve current token as an object, for possible storage to an
#'   \code{.rds} file
#'   \item read the token from an object or from an \code{.rds} file
#'   \item provide your own app key and secret -- this requires setting up
#'   a new OAuth consumer on \href{https://secure.meetup.com/meetup_api/oauth_consumers/}{Meetup}
#'   \item prevent caching of credentials
#' }
#'
#' In a direct call to \code{meetup_auth}, the user can provide the token, app
#' key and secret explicitly and can dictate whether interactively-obtained
#' credentials will be cached. If unspecified, these
#' arguments are controlled via options, which, if undefined at the time
#' \code{meetupr} is loaded, are defined like so:
#'
#' \describe{
#'   \item{key}{Set to option \code{meetupr.consumer_key}, which defaults to a
#'   consumer key that ships with the package}
#'   \item{secret}{Set to option \code{meetupr.consumer_secret}, which defaults to
#'   a consumer secret that ships with the package}
#' }
#'
#' To override these defaults in persistent way, predefine one or more of them
#' with lines like this in a \code{.Rprofile} file:
#' \preformatted{
#' options(meetupr.consumer_key = "FOO",
#'         meetupr.consumer_secret = "BAR")
#' }
#' See \code{\link[base]{Startup}} for possible locations for this file and the
#' implications thereof.
#'
#' More detail is available from
#' \href{https://www.meetup.com/meetup_api/auth/#oauth2-resources}{Authenticating
#' with the Meetup API}.
#'
#' @param token optional; an actual token object or the path to a valid token
#'   stored as an \code{.rds} file.
#' @param new_user logical, defaults to \code{FALSE}. Set to \code{TRUE} if you
#'   want to wipe the slate clean and re-authenticate with the same or different
#'   Meetup account. This disables the \code{.httr2-oauth} file in current
#'   working directory.
#' @param key,secret the "Client ID" and "Client secret" for the application;
#'   defaults to the ID and secret built into the \code{meetupr} package
#' @param cache logical indicating if \code{meetupr} should cache
#'   credentials in the default cache file \code{.httr2-oauth} or `token_path`.
#' @param use_appdir Logical indicating whether to save the created token
#'   in app dir as determined by `rappdirs::user_data_dir("meetupr", "meetupr")`.
#'   If \code{cache} is `FALSE` this is ignored.
#' @param token_path Path where to save the token. If `use_appdir` is `TRUE`,
#'  this is ignored.
#' @template verbose
#'
#' @rdname meetup-auth
#' @export
#' @family auth functions
#' @examples
#' \dontrun{
#' ## load/refresh existing credentials, if available
#' ## otherwise, go to browser for authentication and authorization
#' meetup_auth()
#'
#' ## store token in an object and then to file
#' ttt <- meetup_auth()
#' saveRDS(ttt, "ttt.rds")
#'
#' ## load a pre-existing token
#' meetup_auth(token = ttt)       # from an object
#' meetup_auth(token = "ttt.rds") # from .rds file
#' }
meetup_auth <- function(
  token = meetup_token_path(),
  new_user = FALSE,
  key = getOption("meetupr.consumer_key"),
  secret = getOption("meetupr.consumer_secret"),
  cache = getOption("meetupr.httr2_oauth_cache", TRUE),
  verbose = meetupr_verbose(),
  use_appdir = TRUE,
  token_path = NULL
) {
  if (new_user) {
    meetup_deauth(clear_cache = TRUE, verbose = verbose)
  }

  if (is.null(token)) {
    # Create httr2 OAuth client
    client <- httr2::oauth_client(
      id = key,
      secret = secret,
      token_url = "https://secure.meetup.com/oauth2/access",
      name = "meetup"
    )

    if (!cache && !is.null(token_path)) {
      stop(
        "You chose `cache` FALSE (no saving to disk) but input a `token_path`.",
        "Should you set `cache` to TRUE?",
        call. = FALSE
      )
    }

    # Determine cache location
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

    # Create OAuth token
    meetup_token <- httr2::oauth_flow_auth_code(
      client = client,
      auth_url = "https://secure.meetup.com/oauth2/authorize",
      cache_disk = token_path,
      scope = NULL # Meetup doesn't use scopes
    )

    .state$token <- meetup_token
    return(invisible(meetup_token))
  }

  if (inherits(token, "httr2_token")) {
    .state$token <- token
    return(invisible(token))
  }

  if (inherits(token, "character")) {
    token_path <- token

    if (!file.exists(token_path)) {
      spf("Token file does not exist:\n%s", token_path)
    }

    meetup_token <- try(readRDS(token_path), silent = TRUE)

    if (inherits(meetup_token, "try-error")) {
      spf("Cannot read token from alleged .rds file:\n%s", token_path)
    }

    if (!inherits(meetup_token, "httr2_token")) {
      spf("File does not contain a proper httr2 token:\n%s", token_path)
    }

    .state$token <- meetup_token
    return(invisible(meetup_token))
  }

  spf(
    "Input provided via 'token' is neither a token nor a path to an .rds file containing a token."
  )
}

#' Produce Meetup token
#'
#' If token is not already available, call \code{\link{meetup_auth}} to either
#' load from cache or initiate OAuth2.0 flow. Return the token for use in
#' downstream requests.
#'
#' @return an httr2_token object
#'
#' @keywords internal
#' @export
#' @rdname meetup-auth
#' @family auth functions
#' @examples
#' \dontrun{
#' meetup_token()
#' }
meetup_token <- function(verbose = FALSE) {
  if (nzchar(Sys.getenv("MEETUPR_TESTING"))) {
    # Return a mock token for testing
    return(structure(list(access_token = "test-token"), class = "httr2_token"))
  }

  if (!token_available(verbose = verbose)) {
    meetup_auth(verbose = verbose)
  }

  .state$token
}

#' Check token availability
#'
#' Check if a token is available in \code{\link{meetupr}}'s internal
#' \code{.state} environment.
#'
#' @return logical
#'
#' @keywords internal
token_available <- function(verbose = TRUE) {
  if (is.null(.state$token)) {
    if (verbose) {
      token_path <- meetup_token_path()
      if (!is.null(token_path) && file.exists(token_path)) {
        message(
          "A token file exists.\nWhen/if needed, the credentials cached will be used for this session.\nOr run ",
          "meetup_auth() for explicit authentication and authorization."
        )
      } else {
        message(
          "No token file exists.\n",
          "When/if needed, 'meetupr' will initiate authentication ",
          "and authorization.\nOr run meetup_auth() to trigger this ",
          "explicitly."
        )
      }
    }
    return(FALSE)
  }

  TRUE
}

#' Suspend authorization
#'
#' Suspend \code{\link{meetupr}}'s authorization to place requests to the Meetup
#' APIs on behalf of the authenticated user.
#'
#' @param clear_cache logical indicating whether to disable the
#'   token file in working directory, if such exists, by renaming
#'   to \code{*-SUSPENDED}
#' @template verbose
#' @export
#' @rdname meetup-auth
#' @family auth functions
#' @examples
#' \dontrun{
#' meetup_deauth()
#' }
meetup_deauth <- function(clear_cache = TRUE, verbose = meetupr_verbose()) {
  token_path <- meetup_token_path()

  if (clear_cache && !is.null(token_path) && file.exists(token_path)) {
    if (verbose) {
      message(sprintf(
        "Disabling %s by renaming to %s-SUSPENDED",
        token_path,
        token_path
      ))
    }
    file.rename(token_path, paste0(token_path, "-SUSPENDED"))
  }

  if (token_available(verbose = FALSE)) {
    if (verbose) {
      message("Removing meetup token stashed internally in 'meetupr'.")
    }
    rm("token", envir = .state)
  } else {
    message("No token currently in force.")
  }

  invisible(NULL)
}

#' @return Either NULL or the path in which the token is saved.
#' @export
#' @rdname meetup-auth
#' @family auth functions
#'
#' @examples
#' meetup_token_path()
meetup_token_path <- function() {
  token_path <- appdir_path()

  if (file.exists(token_path)) {
    return(token_path)
  }

  if (file.exists(".httr2-oauth-meetup")) {
    return(".httr2-oauth-meetup")
  }

  NULL
}

appdir_path <- function() {
  file.path(rappdirs::user_data_dir("meetupr", "meetupr"), "meetupr-token.rds")
}
