#' Meetup API situation report
#'
#' Get a situation report on your current Meetup API setup. This function
#' checks your authentication, API connectivity, and configuration to help
#' diagnose any issues with your meetupr setup.
#'
#' @param scope Character vector specifying scope of the report. Options are:
#'   - `"config"`: Check package configuration and options
#'   - `"auth"`: Check authentication status and tokens
#'   - `"api"`: Test API connectivity and endpoints
#'   - `"all"`: Report everything (default)
#'
#' @return Invisibly returns a list with diagnostic information for programmatic use.
#'   The function is primarily called for its side effect of printing a diagnostic report.
#'
#' @export
#' @examples
#' \dontrun{
#' # Full diagnostic report
#' meetup_sitrep()
#'
#' # Check only authentication
#' meetup_sitrep("auth")
#'
#' # Check configuration and API connectivity
#' meetup_sitrep(c("config", "api"))
#' }
meetup_sitrep <- function(scope = c("config", "auth", "api")) {
  scope <- match.arg(
    scope,
    c("config", "auth", "api", "all"),
    several.ok = TRUE
  )
  if ("all" %in% scope) {
    scope <- c("config", "auth", "api")
  }

  cli::cli_h1("Meetup API situation report")

  sitrep_data <- list()

  # Configuration check
  if ("config" %in% scope) {
    sitrep_data$config <- check_meetup_config()
  }

  # Authentication check
  if ("auth" %in% scope) {
    sitrep_data$auth <- check_meetup_auth()
  }

  # API connectivity check
  if ("api" %in% scope) {
    sitrep_data$api <- check_meetup_api()
  }

  # Summary and recommendations
  provide_recommendations(sitrep_data)

  invisible(sitrep_data)
}

#' Check meetupr configuration
#' @noRd
check_meetup_config <- function() {
  cli::cli_h2("Configuration")

  config_data <- list()

  # Package version
  pkg_version <- utils::packageVersion("meetupr")
  config_data$package_version <- as.character(pkg_version)
  cli::cli_alert_info("meetupr version: {pkg_version}")

  # API endpoint
  api_endpoint <- meetup_api_prefix()
  config_data$api_endpoint <- api_endpoint
  is_new_endpoint <- grepl("gql-ext", api_endpoint)

  if (is_new_endpoint) {
    cli::cli_alert_success(
      "API endpoint: {.url {api_endpoint}} {cli::col_green('✓ New GraphQL API')}"
    )
  } else {
    cli::cli_alert_warning(
      "API endpoint: {.url {api_endpoint}} {cli::col_yellow('⚠ Old REST API (deprecated)')}"
    )
  }

  # OAuth consumer keys
  consumer_key <- getOption("meetupr.consumer_key")
  consumer_secret <- getOption("meetupr.consumer_secret")

  config_data$has_consumer_key <- !is.null(consumer_key) &&
    nchar(consumer_key) > 0
  config_data$has_consumer_secret <- !is.null(consumer_secret) &&
    nchar(consumer_secret) > 0

  if (config_data$has_consumer_key) {
    masked_key <- paste0(substr(consumer_key, 1, 4), "...")
    cli::cli_alert_success("Consumer key: {masked_key}")
  } else {
    cli::cli_alert_warning("Consumer key: {cli::col_yellow('Not set')}")
  }

  if (config_data$has_consumer_secret) {
    cli::cli_alert_success("Consumer secret: {cli::col_green('Set')}")
  } else {
    cli::cli_alert_warning("Consumer secret: {cli::col_yellow('Not set')}")
  }

  # HTTP client
  config_data$http_client <- "httr2"
  cli::cli_alert_success(
    "HTTP client: {config_data$http_client} {cli::col_green('✓ Modern')}"
  )

  config_data
}

#' Check meetupr authentication
#' @noRd
check_meetup_auth <- function() {
  cli::cli_h2("Authentication")

  auth_data <- list()

  # Token availability
  auth_data$token_available <- token_available(verbose = FALSE)

  if (auth_data$token_available) {
    cli::cli_alert_success("Token status: {cli::col_green('Available')}")

    # Token location
    token_path <- meetup_token_path()
    auth_data$token_path <- token_path

    if (!is.null(token_path)) {
      cli::cli_alert_info("Token cached at: {.path {token_path}}")

      # Token age/validity (if we can check it safely)
      auth_data$token_cached <- file.exists(token_path)
      if (auth_data$token_cached) {
        token_mtime <- file.mtime(token_path)
        days_old <- as.numeric(difftime(
          Sys.time(),
          token_mtime,
          units = "days"
        ))
        auth_data$token_age_days <- days_old

        if (days_old < 30) {
          cli::cli_alert_success(
            "Token age: {round(days_old, 1)} days {cli::col_green('✓ Fresh')}"
          )
        } else if (days_old < 60) {
          cli::cli_alert_warning(
            "Token age: {round(days_old, 1)} days {cli::col_yellow('⚠ Consider refreshing')}"
          )
        } else {
          cli::cli_alert_danger(
            "Token age: {round(days_old, 1)} days {cli::col_red('⚠ May be expired')}"
          )
        }
      }
    } else {
      cli::cli_alert_info("Token: {cli::col_blue('In memory only')}")
      auth_data$token_cached <- FALSE
    }

    # Try to get token type
    tryCatch(
      {
        token <- meetup_token()
        auth_data$token_type <- if (inherits(token, "httr2_token")) {
          "httr2_token"
        } else {
          class(token)[1]
        }
        cli::cli_alert_success(
          "Token type: {auth_data$token_type} {cli::col_green('✓ Compatible')}"
        )
      },
      error = function(e) {
        auth_data$token_error <- conditionMessage(e)
        cli::cli_alert_danger("Token error: {auth_data$token_error}")
      }
    )
  } else {
    cli::cli_alert_warning("Token status: {cli::col_yellow('Not available')}")
    cli::cli_alert_info("Run {.code meetup_auth()} to authenticate")
    auth_data$token_path <- NULL
    auth_data$token_cached <- FALSE
  }

  auth_data
}

#' Check meetupr API connectivity
#' @noRd
check_meetup_api <- function() {
  cli::cli_h2("API Connectivity")

  api_data <- list()

  # Only test if we have a valid token
  if (!token_available(verbose = FALSE)) {
    cli::cli_alert_warning(
      "Skipping API test - no authentication token available"
    )
    api_data$api_test_skipped <- TRUE
    return(api_data)
  }

  # Test basic API connectivity
  cli::cli_alert_info("Testing API connectivity...")

  tryCatch(
    {
      # Try a simple GraphQL health check query
      test_query <- 'query { self { id name } }'

      start_time <- Sys.time()
      result <- meetup_query(test_query)
      end_time <- Sys.time()

      response_time <- as.numeric(difftime(
        end_time,
        start_time,
        units = "secs"
      ))

      api_data$api_accessible <- TRUE
      api_data$response_time_sec <- response_time
      api_data$user_id <- result$self$id
      api_data$user_name <- result$self$name

      cli::cli_alert_success(
        "API Status: {cli::col_green('✓ Connected')} ({round(response_time * 1000)}ms)"
      )
      cli::cli_alert_success(
        "Authenticated as: {result$self$name} (ID: {result$self$id})"
      )

      # Test rate limiting awareness
      if (response_time > 2) {
        cli::cli_alert_warning(
          "Response time: {cli::col_yellow('Slow')} - may indicate rate limiting"
        )
        api_data$slow_response <- TRUE
      } else {
        api_data$slow_response <- FALSE
      }
    },
    error = function(e) {
      api_data$api_accessible <- FALSE
      api_data$api_error <- conditionMessage(e)

      if (grepl("401|403", e$message)) {
        cli::cli_alert_danger(
          "API Status: {cli::col_red('Authentication failed')}"
        )
        cli::cli_alert_info(
          "Your token may have expired - try {.code meetup_auth()}"
        )
      } else if (grepl("429", e$message)) {
        cli::cli_alert_warning("API Status: {cli::col_yellow('Rate limited')}")
        cli::cli_alert_info("Wait a moment and try again")
      } else if (grepl("timeout|connection", e$message, ignore.case = TRUE)) {
        cli::cli_alert_danger("API Status: {cli::col_red('Connection failed')}")
        cli::cli_alert_info("Check your internet connection")
      } else {
        cli::cli_alert_danger("API Status: {cli::col_red('Error')}")
        cli::cli_alert_info("Error: {e$message}")
      }
    }
  )

  api_data
}

#' Provide recommendations based on diagnostic results
#' @noRd
provide_recommendations <- function(sitrep_data) {
  cli::cli_h2("Recommendations")

  issues <- character(0)
  suggestions <- character(0)

  # Configuration issues
  if (!is.null(sitrep_data$config)) {
    config <- sitrep_data$config

    if (!config$has_consumer_key || !config$has_consumer_secret) {
      issues <- c(issues, "Missing OAuth consumer credentials")
      suggestions <- c(
        suggestions,
        "Set up OAuth app at {.url https://secure.meetup.com/meetup_api/oauth_consumers/}"
      )
    }

    if (!grepl("gql-ext", config$api_endpoint)) {
      issues <- c(issues, "Using deprecated REST API endpoint")
      suggestions <- c(
        suggestions,
        "Update to the new GraphQL API - this should be automatic"
      )
    }
  }

  # Authentication issues
  if (!is.null(sitrep_data$auth)) {
    auth <- sitrep_data$auth

    if (!auth$token_available) {
      issues <- c(issues, "No authentication token")
      suggestions <- c(suggestions, "Run {.code meetup_auth()} to authenticate")
    } else if (!is.null(auth$token_age_days) && auth$token_age_days > 60) {
      issues <- c(issues, "Authentication token may be expired")
      suggestions <- c(
        suggestions,
        "Run {.code meetup_auth()} to refresh your token"
      )
    }
  }

  # API issues
  if (!is.null(sitrep_data$api)) {
    api <- sitrep_data$api

    if (!is.null(api$api_accessible) && !api$api_accessible) {
      issues <- c(issues, "Cannot connect to Meetup API")
      if (!is.null(api$api_error)) {
        suggestions <- c(
          suggestions,
          "Check error message above for specific guidance"
        )
      }
    }

    if (!is.null(api$slow_response) && api$slow_response) {
      issues <- c(issues, "Slow API responses")
      suggestions <- c(
        suggestions,
        "You may be hitting rate limits - wait between requests"
      )
    }
  }

  # Display results
  if (length(issues) == 0) {
    cli::cli_alert_success(
      "{cli::col_green('✓ All systems go!')} Your meetupr setup looks good."
    )
    cli::cli_alert_info(
      "You're ready to use functions like {.code get_events()}, {.code find_groups()}, etc."
    )
  } else {
    cli::cli_alert_warning(
      "{cli::qty(length(issues))}Found {length(issues)} potential issue{?s}:"
    )
    cli::cli_ul(issues)

    if (length(suggestions) > 0) {
      cli::cli_alert_info("Suggested fixes:")
      cli::cli_ul(suggestions)
    }
  }

  # Helpful resources
  cli::cli_h2("Helpful resources:")
  cli::cli_ul(c(
    "📖 Package documentation: {.code ?meetupr}",
    "🔧 Authentication help: {.code ?meetup_auth}",
    "🌐 Meetup API docs: {.url https://www.meetup.com/api/}",
    "❓ Issues & help: {.url https://github.com/rladies/meetupr/issues}"
  ))
}
