#' Get the RSVPs for a specified event
#'
#' @param id Required event ID
#' @param ... Should be empty. Used for parameter expansion
#' @param extra_graphql A graphql object. Extra objects to return
#' @param token Meetup token
#' @return A tibble with the following columns:
#'    * member_id
#'    * member_name
#'    * member_url
#'    * member_is_host
#'    * guests
#'    * response
#'    * event_id
#'    * event_title
#'    * event_url
#'    * created
#'    * updated
#' @references
#' \url{https://www.meetup.com/api/schema/#Event}
#' \url{https://www.meetup.com/api/schema/#RSVP}
#' \url{https://www.meetup.com/api/schema/#Member}
#' @examples
#' \dontrun{
#' rsvps <- get_event_rsvps(id = "103349942!chp")
#' }
#' @export
#' @importFrom anytime anytime
get_event_rsvps <- function(
  id,
  ...,
  extra_graphql = NULL,
  token = meetup_token()
) {
  ellipsis::check_dots_empty()

  dt <- gql_get_event_rsvps(
    id = id,
    .extra_graphql = extra_graphql,
    .token = token
  )

  if (is.null(dt) || nrow(dt) == 0) {
    return(NULL)
  }

  # Apply field mappings from migration guide
  dt <- rename(
    dt,
    # RSVP field mappings (Ticket -> RSVP, User -> Member)
    member_id = member.id, # user -> member
    member_name = member.name, # user -> member
    member_url = member.memberUrl, # user -> member

    # Event field mappings
    event_id = event.id,
    event_title = event.title,
    event_url = event.eventUrl,

    # RSVP specific fields
    member_is_host = isHost,
    guests = guestsCount,
    response = status,

    # Date fields (updated -> updated per migration guide)
    created = createdAt,
    updated = updated # updatedAt -> updated
  )

  # Handle cases where old field names might still exist
  # Fallback to old field names if new ones aren't available
  if (!"member.id" %in% names(dt) && "user.id" %in% names(dt)) {
    dt <- rename(
      dt,
      member_id = user.id,
      member_name = user.name,
      member_url = user.memberUrl
    )
  }

  if (!"updated" %in% names(dt) && "updatedAt" %in% names(dt)) {
    dt <- rename(dt, updated = updatedAt)
  }

  # Date/time processing
  dt$created <- anytime::anytime(dt$created)
  dt$updated <- anytime::anytime(dt$updated)

  dt
}
