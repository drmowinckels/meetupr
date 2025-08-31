#' Get the attendees for a specified event
#'
#' @param id Required event ID
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
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
#' attendees <- get_event_attendees(id = "103349942")
#' }
#' @export
get_event_attendees <- function(
  id,
  ...,
  extra_graphql = NULL
) {
  ellipsis::check_dots_empty()

  gql_get_event_attendees(
    id = id,
    .extra_graphql = extra_graphql
  )
}

gql_get_event_attendees <- function(...) {
  query_generator(
    "get_event_attendees",
    ...,
    cursor_fn = function(x) {
      pageInfo <- x$data$event$rsvps$pageInfo
      if (!is.null(pageInfo) && pageInfo$hasNextPage) {
        list(cursor = pageInfo$endCursor)
      } else {
        NULL
      }
    },
    total_fn = function(x) {
      x$data$event$rsvps$count %||% 0
    },
    extract_fn = function(x) {
      edges <- x$data$event$rsvps$edges
      if (!is.null(edges) && length(edges) > 0) {
        lapply(edges, function(item) {
          item$node
        })
      } else {
        list()
      }
    },
    finalizer_fn = function(ret) {
      if (is.null(ret) || length(ret) == 0) {
        return(create_empty_attendees_tibble())
      }
      process_attendees_data(ret)
    }
  )
}

create_empty_attendees_tibble <- function() {
  dplyr::tibble(
    rsvp_id = character(0),
    id = character(0),
    name = character(0),
    bio = character(0),
    url = character(0),
    photo = character(0),
    organized_group_count = integer(0),
    guests_count = integer(0),
    response = character(0)
  )
}

process_attendees_data <- function(dt) {
  data_to_tbl(dt) |>
    normalize_field_names() |>
    snake_case_names() |>
    attendee_mappings() |>
    remove_prefix("member_") |>
    remove_prefix("member_")
}

attendee_mappings <- function(.data) {
  .data |>
    rename(
      rsvp_id = id,
      url = member_member_url,
      photo = member_member_photo_base_url
    )
}
