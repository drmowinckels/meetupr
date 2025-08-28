#' Find meetup groups matching a search query
#'
#' @param query Required search text
#' @param ... Should be empty. Used for parameter expansion
#' @param topic_category_id Topic ID e.g 543 for technology topic
#' @param lat Latitude. An integer
#' @param lon Longitutde. An integer
#' @param radius Radius. An integer
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @template token
#' @importFrom anytime anytime
#' @export
find_groups <- function(
  query,
  ...,
  topic_category_id = NULL,
  lat = 0,
  lon = 0,
  radius = 100000000,
  extra_graphql = NULL,
  token = meetup_token()
) {
  ellipsis::check_dots_empty()

  dt <- gql_find_groups(
    query = query,
    topicCategoryId = topic_category_id,
    lat = lat,
    lon = lon,
    radius = radius,
    .extra_graphql = extra_graphql,
    .token = token
  )

  if (is.null(dt) || nrow(dt) == 0) {
    return(NULL)
  }

  # Apply field mappings from migration guide
  dt <- rename(
    dt,
    # Group field mappings (from migration guide)
    created = foundedDate,
    total_count = totalCount, # count -> totalCount
    key_group_photo = keyGroupPhoto, # logo -> keyGroupPhoto
    active_topics = activeTopics, # topics -> activeTopics
    memberships = memberships, # membershipSearch -> memberships

    # Keep existing mappings that are still correct
    members = memberships.count,
    join_mode = joinMode,
    category_id = category.id,
    category_name = category.name,

    # Coordinate field mappings (already correct in new API)
    lat = lat, # latitude -> lat (already renamed)
    lon = lon, # longitude -> lon (already renamed)

    country = country_name
  )

  # Remove the country_name field as it's now mapped to country
  dt$country_name <- NULL

  # Date/time processing
  dt$created <- anytime::anytime(dt$created)

  dt
}
