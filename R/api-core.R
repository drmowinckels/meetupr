meetup_api_prefix <- function() {
  Sys.getenv("MEETUP_API_URL", "https://api.meetup.com/gql")
}

#' Create a Meetup OAuth Client
#'
#' This function initializes and returns an OAuth client for authenticating
#' with the Meetup API. It requires the Meetup client ID and secret, which
#' can be passed as arguments or retrieved from environment variables.
#'
#' @param client_id A string representing the Meetup client ID. By default,
#'   it is retrieved from the `MEETUP_CLIENT_ID` environment variable.
#' @param client_secret A string representing the Meetup client secret. By
#'   default, it is retrieved from the `MEETUP_CLIENT_SECRET` environment
#'   variable.
#' @param client_name A string representing the name of the client. By
#'   default, it is set to `"meetupr"` and retrieved from the
#'   `MEETUP_CLIENT_NAME` environment variable.
#'
#' @return An OAuth client object created with the `httr2::oauth_client`
#'   function. This client can be used to handle authentication with the
#'   Meetup API.
#'
#' @examples
#' \dontrun{
#' # Example 1: Using environment variables to set credentials
#' client <- meetup_client()
#'
#' # Example 2: Passing client ID and secret as arguments
#' client <- meetup_client(
#'   client_id = "your_client_id",
#'   client_secret = "your_client_secret"
#' )
#' }
#'
#' @details
#' If the `client_id` or `client_secret` parameters are empty, the function
#' will throw an error prompting you to set the `MEETUP_CLIENT_ID` and
#' `MEETUP_CLIENT_SECRET` environment variables.
#'
#' @export
meetup_client <- function(
  client_id = Sys.getenv("MEETUP_CLIENT_ID"),
  client_secret = Sys.getenv("MEETUP_CLIENT_SECRET"),
  client_name = Sys.getenv("MEETUP_CLIENT_NAME", "meetupr")
) {
  if (!nzchar(client_id) || !nzchar(client_secret)) {
    cli::cli_abort(c(
      "Meetup client ID and secret are required.",
      "i" = "Set {.envvar MEETUP_CLIENT_ID} and {.envvar MEETUP_CLIENT_SECRET} environment variables."
    ))
  }

  httr2::oauth_client(
    id = client_id,
    secret = client_secret,
    name = client_name,
    token_url = "https://secure.meetup.com/oauth2/access"
  )
}


#' Create and Configure a Meetup API Request
#'
#' This function prepares and configures an HTTP request for interacting with
#' the Meetup API. It allows the user to authenticate via OAuth, specify the
#' use of caching, and set custom client configuration.
#'
#' @param cache A logical value indicating whether to cache the OAuth token
#'   on disk. Defaults to `TRUE`.
#' @param ... Additional arguments passed to `meetup_client()` for setting up
#'   the OAuth client.
#'
#' @return A `httr2` request object pre-configured to interact with the
#'   Meetup API. Note that the current implementation is commented out and
#'   serves as a placeholder for the full logic.
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic request with caching enabled
#' req <- meetupr_req(cache = TRUE)
#'
#' # Example 2: Request with custom client ID and secret
#' req <- meetupr_req(
#'   cache = FALSE,
#'   client_id = "your_client_id",
#'   client_secret = "your_client_secret"
#' )
#' }
#'
#' @details
#' This function constructs an HTTP POST request directed to the Meetup API
#' and applies appropriate OAuth headers for authentication. The function
#' is prepared to support caching and provides flexibility for client
#' customization with the `...` parameter. The implementation is currently
#' commented out and would require activation for functionality.
#'
#' @export
meetupr_req <- function(cache = TRUE, ...) {
  client <- meetup_client(...)

  httr2::request(meetup_api_prefix()) |>
    httr2::req_method("POST") |>
    httr2::req_oauth_auth_code(
      client = client,
      auth_url = "https://secure.meetup.com/oauth2/authorize",
      redirect_uri = "http://localhost:1410",
      cache_disk = cache
    ) |>
    httr2::req_headers("Content-Type" = "application/json")
}

build_graphql_request <- function(query, variables = list()) {
  # Ensure variables is always a proper object, not an array
  if (length(variables) == 0 || is.null(variables)) {
    variables <- structure(list(), names = character(0))
  }

  # Debug the request body if enabled
  if (nzchar(Sys.getenv("MEETUPR_DEBUG"))) {
    body <- list(
      query = query,
      variables = variables
    ) |>
      jsonlite::toJSON(
        auto_unbox = TRUE,
        pretty = TRUE
      ) |>
      strsplit("\n|\\\\n") |>
      unlist()
    cli::cli_alert_info("DEBUG: JSON to be sent:")
    cli::cli_code(
      body
    )
  }

  meetupr_req() |>
    httr2::req_body_json(
      list(
        query = query,
        variables = variables
      ),
      auto_unbox = TRUE
    )
}

execute_graphql_request <- function(req) {
  tryCatch(
    {
      httr2::req_perform(req) |>
        httr2::resp_body_json()
    },
    error = function(e) {
      # Try to get more detail from HTTP errors
      if (inherits(e, "httr2_http_400")) {
        # Try to extract the response body for more details
        if (!is.null(e$resp)) {
          body <- tryCatch(
            httr2::resp_body_string(e$resp),
            error = function(x) "Unable to read response body"
          )
          stop("HTTP 400 Bad Request - Server response: ", body, call. = FALSE)
        }
      }
      stop("HTTP request failed: ", e$message, call. = FALSE)
    }
  )
}
