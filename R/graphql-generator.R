#' Generate a function to handle paginated GraphQL queries
#' This function generates a function that handles paginated GraphQL queries.
#' It takes a GraphQL query template, functions to extract cursor information,
#' total count, and data extraction logic, and returns a function that
#' automatically handles pagination and data aggregation.
#'
#' @param template_name The name of the GraphQL query template to use.
#' @param cursor_fn A function that extracts cursor information from the response.
#' It should return a list with cursor parameters or NULL if no more pages.
#' @param total_fn A function that extracts the total count of items from the response.
#' @param extract_fn A function that extracts the relevant data from the response.
#' It should return a list of data items.
#' @param ... Additional parameters to pass to the GraphQL query template.
#' @param .extra_graphql Optional additional GraphQL fragments or queries to include.
#' @param finalizer_fn A function to process the aggregated data before returning.
#' Defaults to `data_to_tbl`.
#' @return A tibble containing the aggregated data from all pages.
#' @noRd
#' @keywords internal
query_generator <- function(
  template_name,
  cursor_fn,
  total_fn,
  extract_fn,
  ...,
  .extra_graphql = NULL,
  finalizer_fn = data_to_tbl
) {
  all_data <- list()
  cursor <- NULL

  response <- execute_from_template(
    template_name,
    ...,
    cursor = cursor,
    .extra_graphql = .extra_graphql
  )
  total <- total_fn(response)
  repeat {
    current_data <- extract_fn(response)
    if (length(current_data) == 0) {
      break
    }

    all_data <- c(all_data, current_data)

    cursor_info <- cursor_fn(response)
    if (is.null(cursor_info)) {
      break
    }

    response <- execute_from_template(
      template_name,
      ...,
      cursor = cursor_info$cursor,
      .extra_graphql = .extra_graphql
    )
  }

  # Use finalizer_fn instead of hardcoded logic
  finalizer_fn(all_data)
}
