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


#' Taken from https://github.com/hadley/httr/blob/1fc659856602f60ff75eb01903513244e3491ec2/R/oauth-cache.R#L52
#' @keywords internal
add_line <- function(path, line) {
  if (file.exists(path)) {
    lines <- readLines(path, warn = FALSE)
    lines <- lines[lines != ""]
  } else {
    lines <- character()
  }

  if (line %in% lines) return(TRUE)
  if (verbose) message("Adding ", line, " to ", path)

  lines <- c(lines, line)
  writeLines(lines, path)
  TRUE
}


#' Retreive a database query with SQL, with caching and lazy defaults
#'
#' A convenience function which wraps \code{\link{dbGetQuery}}, by default
#' providing some local caching. The function is deliberately lazy at the cost
#' of being unpredictable in edge-cases -- if \code{sql} resolves to a valid
#' file, it will be read in as a SQL statement, if it does not, the string will
#' be passed directly to the server as an SQL statement.
#'
#' @param sql \code{\link{character}}. Either a path to an existent text file
#'   which will be read in as a valid SQL statement and submitted to the sever.
#'   If the file does not exist, \code{sql} will be treated as a valid SQL
#'   statement and submitted to the server
#' @param con A database connection. If unspecified, and there is a single
#'   database connection in the global environment, this will be used
#' @param local_cache Should results be cached locally as \code{.RData} files
#'   via \code{\link{saveRDS}}?
#' @param cache_dir The directory in which to cache files
#' @param gitignore Should the cache directory be \code{.gitignored}?
#' @param verbose Should the function tell you how it's processing the strings
#'   passed to \code{sql}, via \code{\link{message}}?
#'
#' @return The results of passing the SQL statement to \code{\link{dbGetQuery}}
#' @export
get_query <- function(
  sql = NULL, con = guess_db_connection(), local_cache = TRUE,
  cache_dir = "./.sql_cache",  gitignore = TRUE, verbose = FALSE
) {

  # Print a message if verbose is TRUE
  vb_message <- function (...) {
    if(verbose) message(...)
  }

  # This is a lazy, slightly unpredicatble function: If `sql` resolves to a file
  # which exists, then read that in as text. If not, assume that it's a sql
  # statement, and use it instead.
  if (is.null(sql)) {
    stop("`sql` cannot be NULL")
  }

  # See if the variable `sql` resolves to a file
  if (file.exists(sql)) {
    vb_message(sql, " resolves to a file. Reading in SQL statement.")
    # Get the filename into an explicit variable
    sql_file   <- sql
    sql_string <- paste0(readLines(sql_file), collapse = "\n")
  } else {
    vb_message("parameter `sql` does not resolve to a file. Treating as ",
               "verbatim SQL statement.")
    sql_file  <- "None."
    sql_string <- sql
  }

  # If there's no interest in the local cache stuff, hit the db and exit
  if (!local_cache) {
    vb_message("Querying database...")
    return(DBI::dbGetQuery(con, sql_string))
  }

  # If the cache_dir doesn't exist, create it (this won't overwrite anything)
  dir.create(cache_dir, showWarnings = FALSE)

  # If they user wants you to, gitignore the cache_dir
  add_line(".gitignore", cache_dir)

  # Generate a filename for the binary data cache: The hash of the filename, a
  # hyphen, and the hash of the sql itself
  filename <- file.path(
    normalizePath(cache_dir),
    paste0(openssl::md5(sql_file), "-", openssl::md5(sql_string), ".rds")
  )

  # If there's already a cached file, just use that (much faster!)
  if (file.exists(filename)) {
    return(readRDS(filename))
  } else {
    # Otherwise, hit the db, cache the results locally for next time, and exit
    vb_message("Querying database...")
    result <- DBI::dbGetQuery(con, sql_string)

    # Note: Error handling in light of
    # https://github.com/rstats-db/DBI/issues/125
    if (is.null(result))
      stop("NULL result from database")

    saveRDS(result, filename)
    return(result)
  }
}
