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
  gql_find_groups(
    query = query,
    ...,
    .extra_graphql = extra_graphql
  )
}

gql_find_groups <- function(...) {
  meetup_query_generator(
    "find_groups",
    ...,
    cursor_fn = function(x) {
      pageInfo <- x$data$keywordSearch$pageInfo
      if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
    },
    total_fn = function(x) x$data$keywordSearch$count %||% Inf,
    extract_fn = function(x) {
      groups <- lapply(x$data$keywordSearch$edges, function(item) {
        item$node$result
      })
      add_country_name(groups, get_country = function(group) group$country)
    },
    finalizer_fn = process_group_data
  )
}

process_group_data <- function(ret) {
  if (is.null(ret) || length(ret) == 0) {
    dt <- dplyr::tibble(
      id = character(0),
      name = character(0),
      urlname = character(0),
      description = character(0),
      link = character(0),
      timezone = character(0),
      city = character(0),
      state = character(0),
      country = character(0),
      latitude = numeric(0),
      longitude = numeric(0),
      memberships.count = integer(0),
      groupPhoto.baseUrl = character(0),
      country_name = character(0)
    )
  } else {
    dt <- data_to_tbl(ret)
  }

  dt |>
    remove(country) |>
    rename(
      country = country_name,
      membership_count = memberships.count,
      photo = groupPhoto.baseUrl
    ) |>
    process_datetime_fields("created")
}
