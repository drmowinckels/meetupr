#' Find groups using text-based search
#'
#' @param query Character string to search for groups
#' @param max_results Maximum number of results to return. Default: 200
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @return A tibble with group information
#' @examples
#' \dontrun{
#' groups <- find_groups("R-Ladies")
#' groups <- find_groups("data science", max_results = 50)
#' }
#' @export
find_groups <- function(
  query,
  max_results = 200,
  ...,
  extra_graphql = NULL
) {
  ellipsis::check_dots_empty()

  gql_find_groups(
    query = query,
    first = min(max_results, 200), # Cap individual requests at 200
    max_results = max_results,
    .extra_graphql = extra_graphql
  )
}

gql_find_groups <- function(max_results = 200, ...) {
  results_fetched <- 0

  query_generator(
    "find_groups",
    ...,
    cursor_fn = function(x) {
      pageInfo <- x$data$groupSearch$pageInfo
      if (
        !is.null(pageInfo) &&
          pageInfo$hasNextPage &&
          results_fetched < max_results
      ) {
        # Update the counter (this is a closure, so it persists)
        current_page_size <- length(x$data$groupSearch$edges)
        results_fetched <<- results_fetched + current_page_size

        list(after = pageInfo$endCursor)
      } else {
        NULL
      }
    },
    total_fn = function(x) {
      min(x$data$groupSearch$totalCount %||% 0, max_results)
    },
    extract_fn = function(x) {
      edges <- x$data$groupSearch$edges
      if (!is.null(edges) && length(edges) > 0) {
        groups <- lapply(edges, function(item) {
          item$node
        })
        add_country_name(groups, get_country = function(group) group$country)
      } else {
        list()
      }
    },
    finalizer_fn = function(ret) {
      if (is.null(ret) || length(ret) == 0) {
        return(create_empty_groups_tibble())
      }
      # Ensure we don't exceed max_results
      if (length(ret) > max_results) {
        ret <- ret[1:max_results]
      }
      process_groups_data(ret)
    }
  )
}

create_empty_groups_tibble <- function() {
  dplyr::tibble(
    id = character(0),
    name = character(0),
    urlname = character(0),
    city = character(0),
    state = character(0),
    country = character(0),
    latitude = numeric(0),
    longitude = numeric(0),
    membership_count = integer(0),
    founded_date = character(0),
    timezone = character(0),
    join_mode = character(0),
    who = character(0),
    is_private = logical(0),
    category_id = character(0),
    category_name = character(0),
    membership_status = character(0)
  )
}

process_groups_data <- function(dt) {
  data_to_tbl(dt) |>
    normalize_field_names() |>
    snake_case_names() |>
    groups_mappings() |>
    process_datetime_fields(c("founded_date"))
}

groups_mappings <- function(.data) {
  rename(
    .data,
    membership_count = memberships_total_count,
    membership_status = membership_metadata_status
  )
}
