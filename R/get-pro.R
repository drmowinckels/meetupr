#' Meetup pro functions
#'
#' The pro functions only work if the querying users
#' had a meetup pro account.
#'
#' @template urlname
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @template token
#' @param status Which status the events should have.
#'
#' @references
#' \url{https://www.meetup.com/api/schema/#ProNetwork}
#'
#' @examples
#' \dontrun{
#' urlname <- "rladies"
#' members <- get_pro_groups(urlname)
#'
#' past_events <- get_pro_events(urlname = urlname,
#'                       status = "PAST")
#' upcoming_events <- get_pro_events(urlname = urlname,
#'                       status = "UPCOMING")
#' all_events <- get_pro_events(urlname = urlname)
#' }
#' @name meetup_pro
#' @return A tibble with meetup pro information
NULL

#' @export
#' @describeIn meetup_pro retrieve groups in a pro network
get_pro_groups <- function(
  urlname,
  ...,
  extra_graphql = NULL,
  token = meetup_token()
) {
  ellipsis::check_dots_empty()

  gql_get_pro_groups(
    urlname = urlname,
    .extra_graphql = extra_graphql,
    .token = token
  ) |>
    process_pro_group_data()
}

#' @export
#' @describeIn meetup_pro retrieve events from a pro network
get_pro_events <- function(
  urlname,
  status = NULL,
  ...,
  extra_graphql = NULL,
  token = meetup_token()
) {
  ellipsis::check_dots_empty()

  gql_get_pro_events(
    urlname = urlname,
    status = status,
    .extra_graphql = extra_graphql,
    .token = token
  ) |>
    process_pro_event_data()
}
