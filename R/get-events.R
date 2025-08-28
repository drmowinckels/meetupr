#' Get the events from a meetup group
#'
#' @template urlname
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @export
get_events <- function(
  urlname,
  ...,
  extra_graphql = NULL
) {
  ellipsis::check_dots_empty()
  browser()
  gql_events(
    urlname = urlname,
    .extra_graphql = extra_graphql
  ) |>
    process_event_data()
}

gql_events <- meetup_query_generator(
  "find_events",
  cursor_fn = function(x) {
    pageInfo <- x$data$groupByUrlname$events$pageInfo
    if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
  },
  total_fn = function(x) x$data$groupByUrlname$events$count %||% Inf,
  extract_fn = function(x) {
    lapply(x$data$groupByUrlname$events$edges, function(item) item$node)
  },
  pb_format = "- :current/?? :elapsed :spin"
)
