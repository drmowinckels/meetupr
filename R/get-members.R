#' Get the members from a meetup group
#'
#' @template urlname
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @template token
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
  extra_graphql = NULL,
  token = meetup_token()
) {
  ellipsis::check_dots_empty()

  dt <- gql_get_members(
    urlname = urlname,
    .extra_graphql = extra_graphql,
    .token = token
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
