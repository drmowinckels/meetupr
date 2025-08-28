#' Get the RSVPs for a specified event
#'
#' @param id Required event ID
#' @param ... Should be empty. Used for parameter expansion
#' @param extra_graphql A graphql object. Extra objects to return
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
  extra_graphql = NULL
) {
  ellipsis::check_dots_empty()

  gql_get_event_rsvps(
    id = id,
    .extra_graphql = extra_graphql
  ) |>
    process_rsvp_data()
}

gql_get_event_rsvps <- meetup_query_generator(
  "find_rsvps",
  cursor_fn = function(x) {
    pageInfo <- x$data$event$tickets$pageInfo
    if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
  },
  total_fn = function(x) x$data$event$tickets$count %||% Inf,
  extract_fn = function(x) {
    lapply(x$data$event$tickets$edges, function(item) item$node)
  },
  pb_format = "- :current/?? :elapsed :spin"
)
