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

  gql_get_event_attendees(
    id = id,
    .extra_graphql = extra_graphql,
    .token = token
  ) |>
    process_member_data() |>
    (\(dt) {
      if (is.null(dt)) {
        return(NULL)
      }
      rename(dt, organized_group_count = organizedGroupCount)
    })()
}
