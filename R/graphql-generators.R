graphql_query_generator <- function(
  template_name,
  cursor_fn,
  total_fn,
  extract_fn,
  pb_format = ":current/:total :elapsed :spin"
) {
  function(..., .extra_graphql = NULL, .token = meetup_token()) {
    all_data <- list()
    cursor <- NULL

    response <- graphql_file(
      template_name,
      ...,
      cursor = cursor,
      .extra_graphql = .extra_graphql,
      .token = .token
    )

    total <- total_fn(response)
    pb <- progress::progress_bar$new(
      format = pb_format,
      total = if (is.finite(total)) total else 100
    )

    repeat {
      current_data <- extract_fn(response)
      if (length(current_data) == 0) {
        break
      }

      all_data <- c(all_data, current_data)
      pb$tick(length(current_data))

      cursor_info <- cursor_fn(response)
      if (is.null(cursor_info)) {
        break
      }

      response <- graphql_file(
        template_name,
        ...,
        cursor = cursor_info$cursor,
        .extra_graphql = .extra_graphql,
        .token = .token
      )
    }

    pb$terminate()

    if (length(all_data) == 0) {
      return(NULL)
    }

    data_to_tbl(all_data)
  }
}

gql_events <- graphql_query_generator(
  "find_events",
  cursor_fn = function(x) {
    pageInfo <- x$data$groupByUrlname$events$pageInfo
    if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
  },
  total_fn = function(x) x$data$groupByUrlname$events$count %||% Inf,
  extract_fn = function(x) {
    lapply(x$data$groupByUrlname$events$edges, function(item) item$node)
  },
  pb_format = "- :current/?? :elapsed :spin"
)

gql_get_members <- graphql_query_generator(
  "find_members",
  cursor_fn = function(x) {
    pageInfo <- x$data$groupByUrlname$memberships$pageInfo
    if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
  },
  total_fn = function(x) x$data$groupByUrlname$memberships$count %||% Inf,
  extract_fn = function(x) {
    lapply(x$data$groupByUrlname$memberships$edges, identity)
  },
  pb_format = "- :current/?? :elapsed :spin"
)

gql_get_event_attendees <- graphql_query_generator(
  "find_attendees",
  cursor_fn = function(x) {
    pageInfo <- x$data$event$tickets$pageInfo
    if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
  },
  total_fn = function(x) x$data$event$tickets$count %||% Inf,
  extract_fn = function(x) {
    lapply(x$data$event$tickets$edges, function(item) item$node$user)
  },
  pb_format = "- :current/?? :elapsed :spin"
)

gql_get_event_rsvps <- graphql_query_generator(
  "find_rsvps",
  cursor_fn = function(x) {
    pageInfo <- x$data$event$tickets$pageInfo
    if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
  },
  total_fn = function(x) x$data$event$tickets$count %||% Inf,
  extract_fn = function(x) {
    lapply(x$data$event$tickets$edges, function(item) item$node)
  },
  pb_format = "- :current/?? :elapsed :spin"
)

gql_get_event_comments <- graphql_query_generator(
  "find_event_comments",
  cursor_fn = function(response) NULL,
  total_fn = function(x) x$data$event$comments$count %||% Inf,
  extract_fn = function(x) {
    lapply(x$data$event$comments$edges, function(item) item$node)
  },
  pb_format = "- :current/?? :elapsed :spin"
)

gql_get_pro_groups <- graphql_query_generator(
  "find_pro_groups",
  cursor_fn = function(x) {
    pageInfo <- x$data$proNetworkByUrlname$groupsSearch$pageInfo
    if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
  },
  total_fn = function(x) x$data$proNetworkByUrlname$groupsSearch$count %||% Inf,
  extract_fn = function(x) {
    lapply(x$data$proNetworkByUrlname$groupsSearch$edges, function(item) {
      item$node
    })
  },
  pb_format = "- :current/?? :elapsed :spin"
)

gql_get_pro_events <- graphql_query_generator(
  "find_pro_events",
  cursor_fn = function(x) {
    pageInfo <- x$data$proNetworkByUrlname$eventsSearch$pageInfo
    if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
  },
  total_fn = function(x) x$data$proNetworkByUrlname$eventsSearch$count %||% Inf,
  extract_fn = function(x) {
    lapply(x$data$proNetworkByUrlname$eventsSearch$edges, function(item) {
      item$node
    })
  },
  pb_format = "- :current/?? :elapsed :spin"
)

gql_find_groups <- graphql_query_generator(
  "find_groups",
  cursor_fn = function(x) {
    pageInfo <- x$data$keywordSearch$pageInfo
    if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
  },
  total_fn = function(x) x$data$keywordSearch$count %||% Inf,
  extract_fn = function(x) {
    lapply(x$data$keywordSearch$edges, function(item) item$node)
  },
  pb_format = "- :current/?? :elapsed :spin"
)
