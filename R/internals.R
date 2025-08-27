# =============================================================================
# INTERNAL UTILITIES - Data Processing & Legacy Support
# =============================================================================
# This file contains utilities for data processing, validation, and legacy
# function stubs. GraphQL-specific functionality is in graphql.R

# Wrapper for messages, spotted in googlesheets3
spf <- function(...) stop(sprintf(...), call. = FALSE)

# =============================================================================
# DATA PROCESSING UTILITIES
# =============================================================================

# Helper function to convert a vector of milliseconds since epoch into POSIXt
.date_helper <- function(time) {
  if (is.character(time)) {
    time <- tryCatch(expr = as.numeric(time), error = function(e) {
      warning("One or more dates could not be converted properly")
      return(NA)
    })
  }

  if (!is.numeric(time)) {
    warning("One or more dates could not be converted properly")
    return(rep(NA, length(time)))
  }

  # divide milliseconds by 1000 to get seconds; convert to POSIXct
  seconds <- time / 1000
  as.POSIXct(seconds, origin = "1970-01-01")
}

# Helper to check event status (updated for new API)
.check_event_status <- function(event_status) {
  match.arg(
    event_status,
    c("CANCELLED", "DRAFT", "PAST", "UPCOMING"), # GraphQL uses UPPERCASE
    several.ok = TRUE
  )
}

.collapse <- function(x) {
  paste(x, collapse = ",")
}

# =============================================================================
# DATA FRAME MANIPULATION UTILITIES
# =============================================================================

# Utility helpers, so we don't crash on missing fields from the API
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

# =============================================================================
# LEGACY COMPATIBILITY - DEPRECATED FUNCTIONS
# =============================================================================

# Legacy compatibility functions - these will be deprecated
# Mark as deprecated but keep for backwards compatibility
.fetch_results <- function(...) {
  .Deprecated(
    msg = paste(
      ".fetch_results() is deprecated as the REST API is no longer supported.",
      "Use GraphQL functions instead."
    )
  )
  stop(
    "REST API functions are no longer supported. Please use GraphQL equivalents."
  )
}

meetup_call <- function(...) {
  .Deprecated(
    msg = paste(
      "meetup_call() is deprecated as the REST API is no longer supported.",
      "Use GraphQL functions instead."
    )
  )
  stop(
    "REST API functions are no longer supported. Please use GraphQL equivalents."
  )
}
