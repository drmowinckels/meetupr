#' Get the events from a meetup group
#'
#' @template urlname
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @template token
#' @export
get_events <- function(
  urlname,
  ...,
  extra_graphql = NULL,
  token = meetup_token()
) {
  ellipsis::check_dots_empty()

  gql_events(
    urlname = urlname,
    .extra_graphql = extra_graphql,
    .token = token
  ) |>
    process_event_data()
}
