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
  status = c("upcoming", "past", "draft"),
  ...,
  extra_graphql = NULL
) {
  ellipsis::check_dots_empty()

  # Validate status options
  status <- match.arg(status, c("upcoming", "past", "draft"), several.ok = TRUE)

  gql_events(
    urlname = urlname,
    status = status,
    .extra_graphql = extra_graphql
  )
}

gql_events <- function(status = c("upcoming", "past", "draft"), ...) {
  # Create status mapping for GraphQL variables
  query_upcoming <- "upcoming" %in% status
  query_past <- "past" %in% status
  query_draft <- "draft" %in% status

  meetup_query_generator(
    "get_events",
    # Pass status flags to GraphQL query
    queryUpcoming = query_upcoming,
    queryPast = query_past,
    queryDraft = query_draft,
    ...,
    cursor_fn = function(x) {
      ret <- list()
      has_cursor <- FALSE
      groupByUrlname <- x$data$groupByUrlname

      # Only check event types that were requested
      event_types <- list()
      if (query_upcoming) {
        event_types <- append(
          event_types,
          list(c("upcomingEvents", "Upcoming"))
        )
      }
      if (query_past) {
        event_types <- append(event_types, list(c("pastEvents", "Past")))
      }
      if (query_draft) {
        event_types <- append(event_types, list(c("draftEvents", "Draft")))
      }

      for (event_type in event_types) {
        page_name <- event_type[1]
        arg_name <- event_type[2]
        info <- groupByUrlname[[page_name]]$pageInfo

        if (!is.null(info) && info$hasNextPage) {
          has_cursor <- TRUE
          ret[[paste0("cursor", arg_name)]] <- info$endCursor
        } else {
          ret[[paste0("query", arg_name)]] <- FALSE
        }
      }

      if (has_cursor) ret else NULL
    },
    total_fn = function(x) {
      groupByUrlname <- x$data$groupByUrlname
      total <- 0

      # Only count requested event types
      if (query_upcoming) {
        total <- total + (groupByUrlname$upcomingEvents$count %||% 0)
      }
      if (query_past) {
        total <- total + (groupByUrlname$pastEvents$count %||% 0)
      }
      if (query_draft) {
        total <- total + (groupByUrlname$draftEvents$count %||% 0)
      }

      total
    },
    extract_fn = function(x) {
      groupByUrlname <- x$data$groupByUrlname

      # Only extract requested event types
      event_types <- c()
      if (query_upcoming) {
        event_types <- c(event_types, "upcomingEvents")
      }
      if (query_past) {
        event_types <- c(event_types, "pastEvents")
      }
      if (query_draft) {
        event_types <- c(event_types, "draftEvents")
      }

      all_events <- list()

      for (event_type in event_types) {
        edges <- groupByUrlname[[event_type]]$edges
        if (!is.null(edges) && length(edges) > 0) {
          nodes <- lapply(edges, `[[`, "node")
          all_events <- append(all_events, nodes)
        }
      }
      # Add country names and return
      add_country_name(all_events, get_country = function(event) {
        event$venue$country
      })
    },
    finalizer_fn = function(ret) {
      # Handle empty results by creating properly structured empty tibble
      if (is.null(ret) || length(ret) == 0) {
        return(dplyr::tibble(
          id = character(0),
          title = character(0),
          eventUrl = character(0),
          createdAt = character(0),
          status = character(0),
          dateTime = character(0),
          duration = character(0),
          description = character(0),
          eventType = character(0),
          going = integer(0),
          waiting = integer(0),
          venue_id = character(0),
          venue_lat = numeric(0),
          venue_lng = numeric(0),
          venue_name = character(0),
          venue_address = character(0),
          venue_city = character(0),
          venue_state = character(0),
          venue_postalCode = character(0),
          venue_country = character(0),
          country_name = character(0)
        ))
      }
      process_event_data(ret)
    }
  )
}

process_event_data <- function(dt) {
  data_to_tbl(dt) |>
    normalize_field_names() |>
    common_event_mappings() |>
    dplyr::mutate(
      datetime = anytime::anytime(datetime),
      venue_country = country_name
    ) |>
    remove(country_name)
}

common_event_mappings <- function(.data) {
  rename(
    .data,
    link = eventUrl,
    featured_event_photo = featuredEventPhoto,
    datetime = dateTime,
    event_type = eventType,
    venue_zip = venue_postalCode,
    venue_lat = venue_lat,
    venue_lon = venue_lon,
    attendees = going,
    waitlist = waiting
  )
}
