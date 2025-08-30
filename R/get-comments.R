#' Get the comments for a specified event
#'
#' @param id Required event ID
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
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
  extra_graphql = NULL
) {
  ellipsis::check_dots_empty()

  gql_get_event_comments(
    id = id,
    .extra_graphql = extra_graphql
  )
}

gql_get_event_comments <- function(...) {
  meetup_query_generator(
    "get_event_comments",
    ...,
    cursor_fn = function(response) NULL, # Comments don't use cursor pagination
    total_fn = function(x) x$data$event$comments$count %||% Inf, # Changed from totalCount
    extract_fn = function(x) {
      lapply(x$data$event$comments$edges, function(item) item$node)
    },
    finalizer_fn = function(ret) {
      if (is.null(ret) || length(ret) == 0) {
        return(dplyr::tibble(
          id = character(0),
          text = character(0),
          created = character(0),
          likes = integer(0),
          member_id = character(0),
          member_name = character(0),
          link = character(0)
        ))
      }
      dt <- data_to_tbl(ret)

      dt <- rename(
        dt,
        comment_id = id,
        comment_text = text,
        created = created,
        likes = likeCount,
        member_id = member.id,
        member_name = member.name
      )

      process_datetime_fields(dt, "created")
    }
  )
}
