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

  dt <- gql_get_event_comments(
    id = id,
    .extra_graphql = extra_graphql
  )

  if (check_empty_response(dt)) {
    return(NULL)
  }

  dt <- rename(
    dt,
    comment_id = id,
    comment_text = text,
    created = created,
    member_id = member.id,
    member_name = member.name
  )

  process_datetime_fields(dt, "created")
}

gql_get_event_comments <- meetup_query_generator(
  "find_event_comments",
  cursor_fn = function(response) NULL,
  total_fn = function(x) x$data$event$comments$count %||% Inf,
  extract_fn = function(x) {
    lapply(x$data$event$comments$edges, function(item) item$node)
  },
  pb_format = "- :current/?? :elapsed :spin"
)
