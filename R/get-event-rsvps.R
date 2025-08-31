#' Get the RSVPs for a specified event
#'
#' @param id Required event ID
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @return A tibble with the following columns:
#'    * rsvp_id
#'    * member_id
#'    * member_name
#'    * member_url
#'    * member_photo
#'    * guests_count
#'    * response
#' @references
#' \url{https://www.meetup.com/api/schema/#Event}
#' \url{https://www.meetup.com/api/schema/#RSVP}
#' \url{https://www.meetup.com/api/schema/#Member}
#' @examples
#' \dontrun{
#' rsvps <- get_event_rsvps(id = "103349942")
#' }
#' @export
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
  query_generator(
    "get_events_rsvps",
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
        return(create_empty_rsvps_tibble())
      }
      process_rsvps_data(ret)
    }
  )
}

create_empty_rsvps_tibble <- function() {
  dplyr::tibble(
    rsvp_id = character(0),
    member_id = character(0),
    member_name = character(0),
    member_url = character(0),
    member_photo = character(0),
    guests_count = integer(0),
    response = character(0)
  )
}

process_rsvps_data <- function(dt) {
  data_to_tbl(dt) |>
    normalize_field_names() |>
    snake_case_names() |>
    rsvp_mappings()
}

rsvp_mappings <- function(.data) {
  .data |>
    rename(
      rsvp_id = id,
      photo = member_member_photo_base_url,
      response = status
    ) |>
    remove_prefix("member_") |>
    remove_prefix("member_")
}
