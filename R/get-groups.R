#' Find groups using text-based search
#'
#' @param query Character string to search for groups
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @return A tibble with group information
#' @examples
#' \dontrun{
#' groups <- find_groups("R-Ladies")
#' }
#' @export
find_groups <- function(
  query,
  ...,
  extra_graphql = NULL
) {
  ellipsis::check_dots_empty()

  dt <- gql_find_groups(
    query = query,
    .extra_graphql = extra_graphql
  )

  if (check_empty_response(dt)) {
    return(NULL)
  }

  dt <- rename(
    dt,
    group_id = id,
    group_name = name,
    group_url = link,
    description = description,
    members = members,
    city = city,
    state = state,
    country = country,
    timezone = timezone
  )

  process_datetime_fields(dt, "created")
}

gql_find_groups <- meetup_query_generator(
  "find_groups",
  cursor_fn = function(x) {
    pageInfo <- x$data$keywordSearch$pageInfo
    if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
  },
  total_fn = function(x) x$data$keywordSearch$count %||% Inf,
  extract_fn = function(x) {
    lapply(x$data$keywordSearch$edges, function(item) item$node)
  },
  pb_format = "- :current/?? :elapsed :spin"
)
