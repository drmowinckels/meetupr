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
#' rsvps <- get_event_rsvps(id = "103349942")
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
  )
}

gql_get_event_rsvps <- function(...) {
  meetup_query_generator(
    "get_rsvps",
    ...,
    cursor_fn = function(x) {
      pageInfo <- x$data$event$tickets$pageInfo
      if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
    },
    total_fn = function(x) x$data$event$tickets$count %||% Inf,
    extract_fn = function(x) {
      lapply(x$data$event$tickets$edges, function(item) item$node)
    },
    finalizer_fn = process_rsvp_data
  )
}

process_rsvp_data <- function(ret) {
  if (is.null(ret) || length(ret) == 0) {
    dt <- dplyr::tibble(
      id = character(0),
      status = character(0),
      createdAt = character(0),
      updatedAt = character(0),
      user.id = character(0),
      user.name = character(0),
      user.memberUrl = character(0),
      event.id = character(0),
      event.title = character(0),
      event.eventUrl = character(0)
    )
  } else {
    dt <- data_to_tbl(ret)
  }

  dt |>
    common_rsvp_mappings() |>
    process_datetime_fields(
      c("created", "updated")
    )
}

common_rsvp_mappings <- function(.data) {
  rename(
    .data,
    member_id = user.id,
    member_name = user.name,
    member_url = user.memberUrl,
    event_id = event.id,
    event_title = event.title,
    event_url = event.eventUrl,
    member_is_host = isHost,
    guests = guestsCount,
    response = status,
    created = createdAt,
    updated = updated
  )
}
