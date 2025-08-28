process_event_data <- function(dt) {
  dt |>
    handle_empty_response() |>
    (\(x) if (is.null(x)) return(NULL) else x)() |>
    normalize_field_names() |>
    common_event_mappings() |>
    (\(x) {
      x$time <- anytime::anytime(x$dateTime)
      x$venue_country <- x$country_name
      x
    })() |>
    remove(country_name, dateTime, going, waiting)
}

process_member_data <- function(dt) {
  dt |>
    handle_empty_response() |>
    (\(x) if (is.null(x)) return(NULL) else x)() |>
    common_member_mappings()
}

process_rsvp_data <- function(dt) {
  dt |>
    handle_empty_response() |>
    (\(x) if (is.null(x)) return(NULL) else x)() |>
    common_rsvp_mappings() |>
    (\(x) {
      if (!"member.id" %in% names(x) && "user.id" %in% names(x)) {
        x <- rename(
          x,
          member_id = user.id,
          member_name = user.name,
          member_url = user.memberUrl
        )
      }
      if (!"updated" %in% names(x) && "updatedAt" %in% names(x)) {
        x <- rename(x, updated = updatedAt)
      }
      x
    })() |>
    process_datetime_fields(c("created", "updated"))
}

process_pro_group_data <- function(dt) {
  dt |>
    handle_empty_response() |>
    (\(x) if (is.null(x)) return(NULL) else x)() |>
    rename(
      created = foundedDate,
      members = memberships.count,
      join_mode = joinMode,
      category_id = category.id,
      category_name = category.name,
      country = country_name,
      past_events_count = pastEvents.count,
      upcoming_events_count = upcomingEvents.count,
      membership_status = membershipMetadata.status,
      is_private = isPrivate
    ) |>
    process_datetime_fields("created")
}

process_pro_event_data <- function(dt) {
  dt |>
    handle_empty_response() |>
    (\(x) if (is.null(x)) return(NULL) else x)() |>
    normalize_field_names() |>
    rename(
      link = eventUrl,
      event_type = eventType,
      venue_zip = venue_postalCode
    ) |>
    (\(x) {
      x$time <- anytime::anytime(x$dateTime)
      x
    })() |>
    remove(dateTime)
}
