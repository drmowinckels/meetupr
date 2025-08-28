meetup_api_prefix <- function() {
  Sys.getenv("MEETUP_API_URL", "https://api.meetup.com/gql-ext")
}

build_graphql_request <- function(query, variables = list(), token) {
  httr2::request(meetup_api_prefix()) |>
    httr2::req_method("POST") |>
    httr2::req_auth_bearer_token(token$access_token) |>
    httr2::req_body_json(list(
      query = query,
      variables = variables
    )) |>
    httr2::req_headers(
      "Content-Type" = "application/json"
    )
}

execute_graphql_request <- function(req) {
  tryCatch(
    {
      resp <- httr2::req_perform(req)
      httr2::resp_body_json(resp)
    },
    error = function(e) {
      stop("GraphQL request failed: ", e$message, call. = FALSE)
    }
  )
}
