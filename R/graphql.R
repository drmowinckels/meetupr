# =============================================================================
# GRAPHQL API CLIENT - HTTP Requests & Query Management
# =============================================================================
# This file handles all GraphQL API communication, authentication, and
# query execution. Data processing utilities are in internals.R

# =============================================================================
# API CONFIGURATION
# =============================================================================

# API URL for GraphQL endpoint
meetup_api_prefix <- function() {
  Sys.getenv("MEETUP_API_URL", "https://api.meetup.com/gql-ext")
}

# =============================================================================
# GRAPHQL REQUEST FUNCTIONS
# =============================================================================

# Capture all output of `str()` and return it as a single string
capture_str <- function(x) {
  paste0(
    utils::capture.output(utils::str(x)),
    collapse = "\n"
  )
}

# Turns a list of consistently shaped lists into a single tibble
# This also turns nested lists like `ITEM$venue$address` into a single value of `venue.address`
data_to_tbl <- function(data) {
  dplyr::bind_rows(
    lapply(data, function(data_item) {
      rlist::list.flatten(data_item)
    })
  )
}

#' Query the Meetup GraphQL API given a file and variables
#'
#' Constructs a single text string and passes the string and `...` variables to [`graphql_query`]
#' @param .file File name (without extension) in `./inst/graphql`
#' @param ... Variables to pass to the query
#' @param .extra_graphql Extra GraphQL code to insert into the query
#' @param .token See [`meetup_token()`] for details
#' @noRd
graphql_file <- function(
  .file,
  ...,
  .extra_graphql = NULL,
  .token = meetup_token()
) {
  # Get GraphQL file path
  file_path <- system.file(
    file.path("graphql", paste0(.file, ".graphql")),
    package = "meetupr"
  )
  query <- readChar(file_path, file.info(file_path)$size)

  # Handle extra GraphQL insertion
  .extra_graphql <- .extra_graphql %||% ""
  if (
    !is.null(.extra_graphql) &&
      (length(.extra_graphql) != 1 || !is.character(.extra_graphql))
  ) {
    stop("`.extra_graphql` must be a single string")
  }

  # Insert extra GraphQL and execute query
  glued_query <- glue::glue_data(
    list(extra_graphql = .extra_graphql),
    query,
    .open = "<<",
    .close = ">>",
    trim = FALSE
  )

  graphql_query(.query = glued_query, ..., .token = .token)
}

#' Query the Meetup GraphQL API
#'
#' @param .query GraphQL query string
#' @param ... Variables to pass to the query
#' @param .token See [`meetup_token()`] for details
#' @return A list like structure directly from the API
#' @noRd
graphql_query <- function(.query, ..., .token = meetup_token()) {
  variables <- purrr::compact(rlang::list2(...))

  # Early return for invalid variables
  if (length(variables) > 0 && !rlang::is_named(variables)) {
    stop(
      "All GraphQL variables must be named. Variables:\n",
      capture_str(variables),
      call. = FALSE
    )
  }

  # Make request using updated endpoint
  response <- make_graphql_request(.query, variables, .token)

  # Early return if errors found
  if (!is.null(response$errors)) {
    stop("Meetup GraphQL API returned errors.\n", capture_str(response$errors))
  }

  response
}

#' Make GraphQL request using httr2
#' @param .query GraphQL query string
#' @param variables Named list of GraphQL variables
#' @param .token httr2_token object
#' @return GraphQL response as list
#' @noRd
make_graphql_request <- function(.query, variables, .token) {
  make_httr2_request(.query, variables, .token)
}

#' Extract authorization header from httr2 token
#' @noRd
extract_auth_header <- function(.token) {
  # Early return for testing environment
  if (nzchar(Sys.getenv("MEETUPR_TESTING"))) {
    return("Bearer test-token")
  }

  # Handle httr2 tokens
  if (inherits(.token, "httr2_token")) {
    return(paste0("Bearer ", .token$access_token))
  }

  # Handle mock tokens for testing
  if (is.list(.token) && !is.null(.token$access_token)) {
    return(paste0("Bearer ", .token$access_token))
  }

  stop("Invalid token type. Expected httr2_token object.")
}

#' Generic method to fetch, extract, and combine results
#'
#' @param file File to send to `graphql_file(.file=)`
#' @param cursor_fn Function that returns next cursor arguments or NULL to stop
#' @param total_fn Function that returns total number of results for progress bar
#' @param extract_fn Function that extracts list of results from API response
#' @param combiner_fn Function to merge results (default: append)
#' @param finalizer_fn Function to process final combined results (default: data_to_tbl)
#' @param pb_format Progress bar format string
#' @return A function that wraps graphql_file with pagination
#' @noRd
graphql_query_generator <- function(
  file,
  cursor_fn,
  total_fn,
  extract_fn,
  combiner_fn = append,
  finalizer_fn = data_to_tbl,
  pb_format = "[:bar] :current/:total :eta"
) {
  # Force evaluation of all closures
  force(file)
  force(cursor_fn)
  force(extract_fn)
  force(combiner_fn)
  force(finalizer_fn)
  force(total_fn)
  force(pb_format)

  function(..., .extra_graphql = NULL, .token = meetup_token()) {
    ret <- NULL
    cursors <- list()
    pb <- NULL

    # Pagination loop with early termination
    repeat {
      # Make GraphQL request
      graphql_res <- graphql_file(
        .file = file,
        ...,
        !!!cursors,
        .extra_graphql = .extra_graphql,
        .token = .token
      )

      # Initialize progress bar on first iteration
      if (is.null(pb)) {
        pb <- create_progress_bar(file, total_fn(graphql_res), pb_format)
        on.exit(pb$terminate(), add = TRUE)
      }

      # Extract and combine results
      graphql_content <- extract_fn(graphql_res)
      ret <- combiner_fn(ret, graphql_content)
      pb$tick(length(graphql_content))

      # Check for more data - early exit if none
      cursors <- cursor_fn(graphql_res)
      if (length(cursors) == 0) break
    }

    finalizer_fn(ret)
  }
}

#' Create progress bar with safe defaults
#' @noRd
create_progress_bar <- function(file, total, pb_format) {
  # Handle infinite totals
  if (is.infinite(total) || is.na(total)) {
    total <- 100 # Use arbitrary total for infinite results
    pb_format <- gsub(":total", "??", pb_format, fixed = TRUE)
  }

  progress::progress_bar$new(
    total = total,
    format = paste0(file, " ", pb_format)
  )
}

# =============================================================================
# GraphQL Query Functions (using early returns)
# =============================================================================

gql_health_check <- graphql_query_generator(
  "health_check",
  cursor_fn = function(response) NULL, # No pagination
  total_fn = function(x) 1,
  extract_fn = function(x) x$data$healthCheck,
  finalizer_fn = unlist,
  pb_format = ":current/:total"
)

gql_events <- graphql_query_generator(
  "find_events",
  cursor_fn = function(x) {
    ret <- list()
    has_cursor <- FALSE
    groupByUrlname <- x$data$groupByUrlname

    # Check each event type for next page
    event_types <- list(
      c("unifiedEvents", "Unified"),
      c("upcomingEvents", "Upcoming"),
      c("pastEvents", "Past"),
      c("draftEvents", "Draft")
    )

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
    sum(c(
      groupByUrlname$unifiedEvents$count %||% 0,
      groupByUrlname$upcomingEvents$count %||% 0,
      groupByUrlname$pastEvents$count %||% 0,
      groupByUrlname$draftEvents$count %||% 0
    ))
  },
  extract_fn = function(x) {
    groupByUrlname <- x$data$groupByUrlname

    # Extract nodes from each event type
    event_types <- c(
      "unifiedEvents",
      "upcomingEvents",
      "pastEvents",
      "draftEvents"
    )
    all_events <- list()

    for (event_type in event_types) {
      edges <- groupByUrlname[[event_type]]$edges
      if (!is.null(edges)) {
        nodes <- lapply(edges, `[[`, "node")
        all_events <- append(all_events, nodes)
      }
    }

    # Add country names and return
    add_country_name(all_events, get_country = function(event) {
      event$venue$country
    })
  },
  pb_format = "[:bar] :current/:total :eta"
)


gql_find_groups <- graphql_query_generator(
  "find_groups",
  cursor_fn = function(x) {
    pageInfo <- x$data$keywordSearch$pageInfo
    if (pageInfo$hasNextPage) list(cursor = pageInfo$endCursor) else NULL
  },
  total_fn = function(x) x$data$keywordSearch$count %||% Inf,
  extract_fn = function(x) {
    groups <- lapply(x$data$keywordSearch$edges, function(item) {
      item$node$result
    })
    add_country_name(groups, get_country = function(group) group$country)
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
  cursor_fn = function(response) NULL, # No pagination for comments
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

# =============================================================================
# Country Code Utilities
# =============================================================================

# Cache the country code to name conversion
country_code_mem <- local({
  cache <- list()
  function(country) {
    # Early return for cached values
    if (!is.null(cache[[country]])) {
      return(cache[[country]])
    }

    # Convert and cache
    val <- countrycode::countrycode(country, "iso2c", "country.name")
    cache[[country]] <<- val
    val
  }
})

# Adds the `country_name` field given the two letter country value
add_country_name <- function(items, get_country) {
  lapply(items, function(item) {
    country <- get_country(item)

    # Early return for missing/empty country
    if (length(country) == 0 || nchar(country) == 0) {
      item$country_name <- ""
      return(item)
    }

    item$country_name <- country_code_mem(country)
    item
  })
}
