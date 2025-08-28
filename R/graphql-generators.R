meetup_query_generator <- function(
  template_name,
  cursor_fn,
  total_fn,
  extract_fn,
  pb_format = ":current/:total :elapsed :spin"
) {
  function(..., .extra_graphql = NULL) {
    all_data <- list()
    cursor <- NULL

    response <- graphql_file(
      template_name,
      ...,
      cursor = cursor,
      .extra_graphql = .extra_graphql
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
        .extra_graphql = .extra_graphql
      )
    }

    pb$terminate()

    if (length(all_data) == 0) {
      return(NULL)
    }

    data_to_tbl(all_data)
  }
}
