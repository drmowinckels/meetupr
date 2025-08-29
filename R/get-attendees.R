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
  ) |>
    process_member_data() |>
    (\(dt) {
      if (is.null(dt)) {
        return(NULL)
      }
      rename(dt, organized_group_count = organizedGroupCount)
    })()
}

gql_get_event_attendees <- function(...) {
  meetup_query_generator(
    "find_attendees",
    ...,
    cursor_fn = function(x) {
      pageInfo <- x$data$event$tickets$pageInfo # Changed from rsvps
      if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
    },
    total_fn = function(x) x$data$event$tickets$count %||% Inf, # Changed from totalCount
    extract_fn = function(x) {
      lapply(x$data$event$tickets$edges, function(item) item$node$user) # Changed to user
    },
    finalizer_fn = function(ret) {
      if (is.null(ret) || length(ret) == 0) {
        return(dplyr::tibble(
          id = character(0),
          name = character(0),
          bio = character(0),
          memberUrl = character(0),
          memberPhoto.baseUrl = character(0),
          joinTime = character(0),
          timezone = character(0),
          organizedGroupCount = integer(0)
        ))
      }
      data_to_tbl(ret)
    }
  )
}
