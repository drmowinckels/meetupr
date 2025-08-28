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

  gql_get_event_rsvps(
    id = id,
    .extra_graphql = extra_graphql,
    .token = token
  ) |>
    process_rsvp_data()
}
