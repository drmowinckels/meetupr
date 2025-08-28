.check_event_status <- function(event_status) {
  match.arg(
    event_status,
    c("CANCELLED", "DRAFT", "PAST", "UPCOMING"),
    several.ok = TRUE
  )
}

validate_graphql_variables <- function(variables) {
  if (length(variables) > 0 && !rlang::is_named(variables)) {
    stop(
      "All GraphQL variables must be named. Variables:\n",
      capture_str(variables),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
