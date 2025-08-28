graphql_file <- function(
  .file,
  ...,
  .extra_graphql = NULL,
  .token = meetup_token()
) {
  file_path <- get_graphql_file_path(.file)
  query <- read_graphql_file(file_path)

  .extra_graphql <- validate_extra_graphql(.extra_graphql)

  glued_query <- insert_extra_graphql(query, .extra_graphql)

  graphql_query(.query = glued_query, ..., .token = .token)
}

graphql_query <- function(.query, ..., .token = meetup_token()) {
  variables <- purrr::compact(rlang::list2(...))

  validate_graphql_variables(variables)

  req <- build_graphql_request(.query, variables, .token)
  result <- execute_graphql_request(req)

  if (!is.null(result$errors)) {
    stop(
      "GraphQL errors: ",
      paste(sapply(result$errors, function(e) e$message), collapse = "; ")
    )
  }

  result
}
