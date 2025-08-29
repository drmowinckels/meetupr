#' Get the members from a meetup group
#'
#' @template urlname
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @return A tibble with the following columns:
#'    * id
#'    * name
#'    * member_url
#'    * photo_link
#'    * status
#'    * role
#'    * created
#'    * most_recent_visit
#' @export
get_members <- function(
  urlname,
  ...,
  extra_graphql = NULL
) {
  ellipsis::check_dots_empty()

  dt <- gql_get_members(
    urlname = urlname,
    .extra_graphql = extra_graphql
  )

  if (check_empty_response(dt)) {
    return(NULL)
  }

  dt <- rename(
    dt,
    id = member.id,
    name = member.name,
    member_url = member.memberUrl,
    photo_link = member.memberPhoto.baseUrl,
    status = status,
    role = role,
    created = joined,
    most_recent_visit = lastAccessed
  )

  process_datetime_fields(dt, c("created", "most_recent_visit"))
}

gql_get_members <- function(...) {
  meetup_query_generator(
    "find_members",
    ...,
    cursor_fn = function(x) {
      pageInfo <- x$data$groupByUrlname$memberships$pageInfo
      if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
    },
    total_fn = function(x) {
      x$data$groupByUrlname$memberships$totalCount %||% Inf
    },
    extract_fn = function(x) {
      lapply(x$data$groupByUrlname$memberships$edges, identity)
    }
  )
}

process_member_data <- function(dt) {
  dt |>
    common_member_mappings()
}
