#' Get the attendees for a specified event
#'
#' @param id Required event ID
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @template token
#' @return A tibble with the following columns:
#'    * id
#'    * name
#'    * url
#'    * photo
#'    * organized_group_count
#' @references
#' \url{https://www.meetup.com/api/schema/#Event}
#' \url{https://www.meetup.com/api/schema/#RSVP}
#' \url{https://www.meetup.com/api/schema/#Member}
#' @examples
#' \dontrun{
#' attendees <- get_event_attendees(id = "103349942!chp")
#' }
#' @export
get_event_attendees <- function(
  id,
  ...,
  extra_graphql = NULL,
  token = meetup_token()
) {
  ellipsis::check_dots_empty()

  dt <- gql_get_event_attendees(
    id = id,
    .extra_graphql = extra_graphql,
    .token = token
  )

  if (is.null(dt) || nrow(dt) == 0) {
    return(NULL)
  }

  # Apply field mappings from migration guide
  rename(
    dt,
    # Member field mappings (User -> Member)
    url = memberUrl,
    member_photo = memberPhoto.baseUrl, # Image -> Photo
    organized_group_count = organizedGroupCount,

    # Keep backwards compatibility
    photo = memberPhoto.baseUrl # Keep old field name for compatibility
  )
}
