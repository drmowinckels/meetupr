meetup_query_generator <- function(
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

  response <- graphql_file(
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

    response <- graphql_file(
      template_name,
      ...,
      cursor = cursor_info$cursor,
      .extra_graphql = .extra_graphql
    )
  }

  # Use finalizer_fn instead of hardcoded logic
  finalizer_fn(all_data)
}
