#' If there's only one database connection, use it
#' @keywords internal
guess_db_connection <- function (db_con_classes = "PostgreSQLConnection") {
  in_memory <- ls(globalenv())
  is_con <- lapply(in_memory, function(x) class(get(x))) %in% db_con_classes

  good_advice <- ".\nBetter to specify a database connection explicitly."

  if(sum(is_con) < 1)
    stop("There are no active database connections in memory",
         good_advice)

  if(sum(is_con) > 1)
    stop("There is more than one active database connection in memory",
         good_advice)

  return(get(in_memory[is_con]))
}


#' Retreive a database query with SQL, with caching and lazy defaults
#'
#' A convenience function which wraps \code{\link{dbGetQuery}}, by default
#' providing some local caching. Exactly one of \code{sql_file} and
#' \code{sql_string} must be supplied.
#'
#' @param sql_file A path to a SQL text file
#' @param sql_string A string containing SQL
#' @param con A database connection. If unspecified, and there is a single
#'   database connection in the global environment, this will be used
#' @param local_cache Should results be cached locally as \code{.RData} files
#'   via \code{\link{saveRDS}}?
#' @param cache_dir The directory in which to cache files
#' @param gitignore Should the cache directory be \code{.gitignored}?
#'
#' @return The results of passing the SQL statement to \code{\link{dbGetQuery}}
#' @export
get_query <- function (
  sql_file = NULL, sql_string = NULL, con = guess_db_connection(),
  local_cache = TRUE, cache_dir = "./.sql_cache",  gitignore = TRUE
) {

  # You'll need some SQL
  null_sources <- c(is.null(sql_file), is.null(sql_string))
  if (all(null_sources) | all(!null_sources)) {
    stop("Exactly one of sql_file and sql_string must be non-NULL")
  }

  # If the user's chosen to use a file
  if (!is.null(sql_file)) {
    # If the file doesn't exist, error out
    if (!file.exists(sql_file)) {
      stop("sql_file does not resolve to a file")
    }

    # Read in the SQL from the file
    sql <- paste0(readLines(sql_file), collapse = "\n")
  } else {
    # Provide some value for the hashing part
    sql_file <- "None."

    # Use the string provided for the query
    sql <- sql_string
  }

  # If there's no interest in the local cache stuff, hit the db and exit
  if (!local_cache) {
    return(DBI::dbGetQuery(con, sql))
  }

  # If the cache_dir doesn't exist, create it (this won't overwrite anything)
  dir.create(cache_dir, showWarnings = FALSE)

  # Generate a filename for the binary data cache: The hash of the filename, a
  # hyphen, and the hash of the sql itself
  filename <- file.path(
    normalizePath(cache_dir),
    paste0(openssl::md5(sql_file), "-", openssl::md5(sql), ".rds")
  )

  # If there's already a cached file, just use that (much faster!)
  if (file.exists(filename)) {
    return(readRDS(filename))
  } else {
    # Otherwise, hit the db, cache the results locally for next time, and exit
    result <- DBI::dbGetQuery(con, sql)
    saveRDS(result, filename)
    return(result)
  }
}
