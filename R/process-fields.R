normalize_field_names <- function(.data) {
  names(.data) <- gsub("\\.", "_", names(.data))
  .data
}

rename <- function(.data, ...) {
  args <- rlang::enexprs(...)

  for (arg in 1:length(args)) {
    idx <- match(rlang::as_label(args[[arg]]), names(.data))
    if (length(idx) != 1) {
      next
    }
    names(.data)[idx] <- names(args)[arg]
  }
  .data
}

remove <- function(.data, ...) {
  args <- rlang::enexprs(...)

  for (arg in 1:length(args)) {
    col <- rlang::as_label(args[[arg]])
    idx <- match(col, names(.data))
    if (length(idx) != 1) {
      next
    }
    .data[, col] <- NULL
  }
  .data
}

common_event_mappings <- function(.data) {
  rename(
    .data,
    link = eventUrl,
    featured_event_photo = featuredEventPhoto,
    event_type = eventType,
    venue_zip = venue_postalCode,
    venue_lat = venue_lat,
    venue_lon = venue_lon,
    attendees = going,
    waitlist = waiting
  )
}

common_member_mappings <- function(.data) {
  rename(
    .data,
    url = memberUrl,
    member_photo = memberPhoto.baseUrl,
    photo = memberPhoto.baseUrl
  )
}

common_rsvp_mappings <- function(.data) {
  rename(
    .data,
    member_id = member.id,
    member_name = member.name,
    member_url = member.memberUrl,
    event_id = event.id,
    event_title = event.title,
    event_url = event.eventUrl,
    member_is_host = isHost,
    guests = guestsCount,
    response = status,
    created = createdAt,
    updated = updated
  )
}
