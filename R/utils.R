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

capture_str <- function(x) {
  paste0(
    capture.output(str(x)),
    collapse = "\n"
  )
}

has_ext <- function(x) {
  .ext_pattern <- "[[:alnum:]]{1,}\\.[[:alpha:]]{1,}$"
  stopifnot(length(x) == 1L)
  x <- basename(x)
  grepl(.ext_pattern, x)
}

only_ext <- function(x) {
  if (has_ext(x)) {
    gsub(".*(?=\\.)", "", x, perl = TRUE)
  } else {
    ""
  }
}

no_ext <- function(x) {
  if (has_ext(x)) {
    gsub("(?<=[[:alnum:]]{1})\\..*(?!=\\.)", "", x, perl = TRUE)
  } else {
    x
  }
}

paste_before_ext <- function(x, p) {
  paste0(no_ext(x), p, only_ext(x))
}

uq_filename <- function(file_name) {
  stopifnot(is.character(file_name) && length(file_name) == 1L)
  if (file.exists(file_name)) {
    files <- list.files(dirname(file_name), all.files = TRUE, full.names = TRUE)
    file_name <- paste_before_ext(file_name, 1:1000)
    file_name <- file_name[!file_name %in% files][1]
  }
  file_name
}
