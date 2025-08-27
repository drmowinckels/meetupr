#' Get the current meetup members from a meetup group
#'
#' @template urlname
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @template token
#' @return A tibble with the following columns:
#'    * id
#'    * name
#'    * member_url
#'    * photo_link
#'    * status
#'    * role
#'    * created
#'    * last_access_time
#' @references
#' \url{https://www.meetup.com/api/schema/#Membership}
#' \url{https://www.meetup.com/api/schema/#Member}
#' @examples
#' \dontrun{
#' members <- get_members("rladies-remote")
#' }
#' @importFrom anytime anytime
#' @export
get_members <- function(
  urlname,
  ...,
  extra_graphql = NULL,
  token = meetup_token()
) {
  ellipsis::check_dots_empty()

  dt <- gql_get_members(
    urlname = urlname,
    .extra_graphql = extra_graphql,
    .token = token
  )

  if (is.null(dt) || nrow(dt) == 0) {
    return(NULL)
  }

  # Apply field mappings from migration guide
  dt <- rename(
    dt,
    # Member field mappings (User -> Member, Ticket -> RSVP)
    id = node.id,
    name = node.name,
    member_url = node.memberUrl,
    photo_link = node.memberPhoto.baseUrl, # Image -> Photo

    # Membership field mappings (from migration guide)
    status = metadata.status,
    role = metadata.role,
    join_time = metadata.joinedDate, # joinedDate -> joinTime
    last_access_time = metadata.lastAccessTime, # mostRecentVisitDate -> lastAccessTime

    # Support both old and new field names for backwards compatibility
    created = metadata.joinedDate,
    most_recent_visit = metadata.lastAccessTime
  )

  # Handle cases where new field names might not exist yet
  # Fallback to old field names if new ones aren't available
  if (
    !"metadata.lastAccessTime" %in% names(dt) &&
      "metadata.mostRecentVisitDate" %in% names(dt)
  ) {
    dt <- rename(dt, last_access_time = metadata.mostRecentVisitDate)
    dt <- rename(dt, most_recent_visit = metadata.mostRecentVisitDate)
  }

  # Date/time processing
  dt$created <- anytime::anytime(dt$created)
  dt$join_time <- anytime::anytime(dt$join_time)
  dt$last_access_time <- anytime::anytime(dt$last_access_time)
  dt$most_recent_visit <- anytime::anytime(dt$most_recent_visit)

  dt
}
