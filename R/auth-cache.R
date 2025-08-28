appdir_path <- function() {
  file.path(
    rappdirs::user_data_dir("meetupr"),
    "meetupr-token.rds"
  )
}

meetup_token_path <- function() {
  user_appdir <- appdir_path()
  project_cache <- ".httr2-oauth-meetup"

  if (file.exists(user_appdir)) {
    user_appdir
  } else if (file.exists(project_cache)) {
    project_cache
  } else {
    NULL
  }
}

cache_token <- function(token, token_path) {
  if (is.null(token_path)) {
    return(invisible(FALSE))
  }

  parent_dir <- dirname(token_path)
  if (!file.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE)
  }

  tryCatch(
    {
      saveRDS(token, token_path)
      invisible(TRUE)
    },
    error = function(e) {
      warning("Failed to cache token: ", e$message)
      invisible(FALSE)
    }
  )
}

load_cached_token <- function(token_path) {
  if (is.null(token_path) || !file.exists(token_path)) {
    return(NULL)
  }

  tryCatch(
    {
      token <- readRDS(token_path)
      if (inherits(token, "httr2_token")) {
        token
      } else {
        NULL
      }
    },
    error = function(e) {
      warning("Failed to load cached token: ", e$message)
      NULL
    }
  )
}
