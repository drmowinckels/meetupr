#' Get the members from a meetup group
#'
#' @template urlname
#' @param max_results Maximum number of results to return. Default: 200
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @return A tibble with the following columns:
#'    * id
#'    * name
#'    * member_url
#'    * photo_link
#'    * status
#'    * role
#'    * joined
#'    * most_recent_visit
#' @export
get_members <- function(
  urlname,
  max_results = 200,
  ...,
  extra_graphql = NULL
) {
  ellipsis::check_dots_empty()

  gql_get_members(
    urlname = urlname,
    first = min(max_results, 200),
    max_results = max_results,
    .extra_graphql = extra_graphql
  )
}

gql_get_members <- function(max_results = 200, ...) {
  results_fetched <- 0

  query_generator(
    "get_members",
    ...,
    cursor_fn = function(x) {
      pageInfo <- x$data$groupByUrlname$memberships$pageInfo
      if (
        !is.null(pageInfo) &&
          pageInfo$hasNextPage &&
          results_fetched < max_results
      ) {
        current_page_size <- length(x$data$groupByUrlname$memberships$edges)
        results_fetched <<- results_fetched + current_page_size

        list(after = pageInfo$endCursor)
      } else {
        NULL
      }
    },
    total_fn = function(x) {
      min(x$data$groupByUrlname$memberships$totalCount %||% 0, max_results)
    },
    extract_fn = function(x) {
      edges <- x$data$groupByUrlname$memberships$edges
      if (!is.null(edges) && length(edges) > 0) {
        lapply(edges, identity)
      } else {
        list()
      }
    },
    finalizer_fn = function(ret) {
      if (is.null(ret) || length(ret) == 0) {
        return(create_empty_members_tibble())
      }
      # Ensure we don't exceed max_results
      if (length(ret) > max_results) {
        ret <- ret[1:max_results]
      }
      process_members_data(ret)
    }
  )
}

create_empty_members_tibble <- function() {
  dplyr::tibble(
    id = character(0),
    name = character(0),
    url = character(0),
    photo = character(0),
    status = character(0),
    role = character(0)
  )
}

process_members_data <- function(dt) {
  data_to_tbl(dt) |>
    normalize_field_names() |>
    snake_case_names() |>
    members_mappings()
}

members_mappings <- function(.data) {
  rename(
    .data,
    id = node_id,
    name = node_name,
    url = node_member_url,
    photo = node_member_photo_base_url,
    status = metadata_status,
    role = metadata_role
  )
}
