graphql_file <- function(
  .file,
  ...,
  .extra_graphql = NULL
) {
  file_path <- get_graphql_file_path(.file)
  query <- read_graphql_file(file_path)

  .extra_graphql <- validate_extra_graphql(.extra_graphql)

  glued_query <- insert_extra_graphql(query, .extra_graphql)
  meetup_query(.query = glued_query, ...)
}

#' Execute GraphQL query
#' @param .query GraphQL query string
#' @param ... Variables to pass to query
#' @export
meetup_query <- function(
  .query,
  ...,
  parent_frame = parent.frame()
) {
  variables <- purrr::compact(rlang::list2(...))

  validate_graphql_variables(variables)

  req <- build_graphql_request(.query, variables)
  result <- httr2::req_perform(req)

  if (!is.null(result$errors)) {
    cli::cli_abort(
      c(
        "Failed to execute GraphQL query.",
        sapply(result$errors, function(e) e$message)
      ),
      .envir = parent_frame
    )
  }

  httr2::resp_body_json(result)
}
