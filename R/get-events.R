#' Get the events from a meetup group
#'
#' @template urlname
#' @param status Character vector of event statuses to retrieve.
#'   Options: "upcoming", "past", "draft". Default: all three.
#' @param ... Should be empty. Used for parameter expansion
#' @template extra_graphql
#' @export
get_events <- function(
  urlname,
  status = c("active", "past", "draft", "cancelled"),
  ...,
  extra_graphql = NULL
) {
  ellipsis::check_dots_empty()

  # Validate status options
  status <- match.arg(
    status,
    c("active", "past", "draft", "cancelled"),
    several.ok = TRUE
  )

  gql_events(
    urlname = urlname,
    status = status,
    .extra_graphql = extra_graphql
  )
}


gql_events <- function(
  urlname,
  status = c("active", "past", "draft", "cancelled"),
  ...,
  .extra_graphql = NULL
) {
  status_mapping <- list(
    "active" = "ACTIVE",
    "past" = "PAST",
    "draft" = "DRAFT",
    "cancelled" = "CANCELLED"
  )

  status_values <- status_mapping[status]

  query_generator(
    "get_events",
    urlname = urlname,
    status = unname(unlist(status_values)),
    ...,
    .extra_graphql = .extra_graphql,
    cursor_fn = function(x) {
      pageInfo <- x$data$groupByUrlname$events$pageInfo
      if (!is.null(pageInfo) && pageInfo$hasNextPage) {
        list(cursorEvents = pageInfo$endCursor)
      } else {
        NULL
      }
    },
    total_fn = function(x) {
      x$data$groupByUrlname$events$totalCount %||% 0
    },
    extract_fn = function(x) {
      edges <- x$data$groupByUrlname$events$edges
      if (!is.null(edges) && length(edges) > 0) {
        nodes <- lapply(edges, `[[`, "node")
        add_country_name(nodes, get_country = function(event) {
          if (length(event$venues) > 0) {
            event$venues[[1]]$country
          } else {
            NULL
          }
        })
      } else {
        list()
      }
    },
    finalizer_fn = function(ret) {
      if (is.null(ret) || length(ret) == 0) {
        return(create_empty_events_tibble())
      }
      process_event_data(ret)
    }
  )
}

create_empty_events_tibble <- function() {
  dplyr::tibble(
    id = character(0),
    title = character(0),
    link = character(0),
    created = character(0),
    status = character(0),
    date_time = character(0),
    duration = character(0),
    description = character(0),
    group_id = character(0),
    group_name = character(0),
    group_urlname = character(0),
    venues_id = character(0),
    venues_name = character(0),
    venues_address = character(0),
    venues_city = character(0),
    venues_state = character(0),
    venues_zip = character(0),
    venues_country = character(0),
    venues_lat = numeric(0),
    venues_lon = numeric(0),
    venues_type = character(0),
    attendees = integer(0),
    photo_url = character(0)
  )
}

process_event_data <- function(dt) {
  data_to_tbl(dt) |>
    normalize_field_names() |>
    snake_case_names() |>
    common_event_mappings() |>
    dplyr::mutate(
      attendees = as.integer(attendees),
      venue_country = country_name
    ) |>
    process_datetime_fields(c("created", "date_time")) |>
    select(-country_name)
}

common_event_mappings <- function(.data) {
  rename(
    .data,
    link = event_url,
    event_type = eventType,
    venue_zip = venue_postal_code,
    attendees = rsvps_total_count,
    created = created_time,
    photo_url = featured_event_photo_base_url,
    venues_type = venues_venue_type
  )
}
