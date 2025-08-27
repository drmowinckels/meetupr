#' Get the events from a meetup group
#'
#' @template urlname
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @template token
#' @export
get_events <- function(
  urlname,
  ...,
  extra_graphql = NULL,
  token = meetup_token()
) {
  ellipsis::check_dots_empty()

  dt <- gql_events(
    urlname = urlname,
    .extra_graphql = extra_graphql,
    .token = token
  )
  
  if (is.null(dt) || nrow(dt) == 0) return(NULL)

  # Replace dot with underscore for consistency
  names(dt) <- gsub("\\.", "_", names(dt))

  # Apply field mappings from migration guide
  dt <- rename(dt,
    # Event field mappings (from migration guide)
    link = eventUrl,
    featured_event_photo = featuredEventPhoto,  # image/images -> featuredEventPhoto
    event_type = eventType,                     # isOnline -> eventType (now supports online, inPerson, hybrid)
    
    # Venue field mappings  
    venue_zip = venue_postalCode,
    venue_lat = venue_lat,     # Already correct (latitude -> lat)
    venue_lon = venue_lon,     # Already correct (longitude -> lon)
    
    # RSVP field mappings
    attendees = going,         # going -> attendees (for backwards compatibility)
    waitlist = waiting         # waiting -> waitlist (for backwards compatibility)
  )

  # Date/time processing
  dt$time <- anytime::anytime(dt$dateTime)
  dt$venue_country <- dt$country_name

  # Remove deprecated/renamed fields
  remove(dt,
    country_name,
    dateTime,
    # Remove old field names that have been renamed
    going,      # Now mapped to attendees
    waiting     # Now mapped to waitlist
  )
}