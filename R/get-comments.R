#' Get the comments for a specified event
#'
#' @param id Required event ID
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @template token
#' @return A tibble with comments data
#' @references
#' \url{https://www.meetup.com/api/schema/#Event}
#' \url{https://www.meetup.com/api/schema/#Comment}
#' @examples
#' \dontrun{
#' comments <- get_event_comments(id = "103349942!chp")
#' }
#' @export
get_event_comments <- function(
  id,
  ...,
  extra_graphql = NULL,
  token = meetup_token()
) {
  ellipsis::check_dots_empty()

  dt <- gql_get_event_comments(
    id = id,
    .extra_graphql = extra_graphql,
    .token = token
  )

  if (check_empty_response(dt)) {
    return(NULL)
  }

  dt <- rename(
    dt,
    comment_id = id,
    comment_text = text,
    created = created,
    member_id = member.id,
    member_name = member.name
  )

  process_datetime_fields(dt, "created")
}
