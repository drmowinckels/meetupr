#' Find groups using text-based search
#'
#' @param query Character string to search for groups
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @template token
#' @return A tibble with group information
#' @examples
#' \dontrun{
#' groups <- find_groups("R-Ladies")
#' }
#' @export
find_groups <- function(
  query,
  ...,
  extra_graphql = NULL,
  token = meetup_token()
) {
  ellipsis::check_dots_empty()

  dt <- gql_find_groups(
    query = query,
    .extra_graphql = extra_graphql,
    .token = token
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
