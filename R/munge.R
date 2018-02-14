#' Change Factor Levels Using List Lookups
#'
#' @param x A \code{\link{factor}} variable
#' @param new_values A list
#' @param throw_error If there are values in \code{x} which are not explicitly
#'   mapped in \code{new_values}, should the function throw an error?
#'
#' @return \code{factor}
#' @export
#' @name refactor
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' @seealso \code{\link{refactor_list}} for generating code to feed to
#'   \code{new_values} in scripts. Other functions which aim to make factor
#'   munging less painful are the \code{\link[car]{recode}} function in the car
#'   package, and \code{\link[plyr]{mapvalues}} in plyr
refactor <- function(x, new_values, throw_error = FALSE){

  valid <- all(
    unlist(lapply(new_values, function(x) dim(x) == 2 & length(x) == 2))
  )

  if(!is.list(new_values) | !valid){
    stop("new_values must be a list of vectors, each of length 2")
  }

  from <- unlist(lapply(new_values, function(x) x[1]))
  to   <- unlist(lapply(new_values, function(x) x[2]))

  if(!all(unique(x) %in% from) & throw_error){
    stop("x contains new levels not found in refactor mapping!")
  }

  mapvalues(x, from, to)
}


#' Bin numeric data into ntiles
#'
#' A function which accepts numeric data, calculates ntiles from it (by
#' default, quartiles are used), and returns a factor variable, corresponding
#' to the ntile within within which each data point in \code{x} resides.
#'
#' @param x A numeric vector to be transformed into a factor variable
#' @param ntiles The number of groups that the data should be divided in
#'   to. For example, the default, 4, will produce a factor level corresponding
#'   to which quartile the value of x is in (denoted as Q1, Q2, Q3, Q4).
#' @return A factor variable, the same length as \code{x}
cut_ntile <- function(x, ntiles = 4) {
  cut(
    x, quantile(x, seq(from = 0, to = 1, length.out = ntiles + 1)),
    include.lowest = TRUE,
    labels = paste0("Q", lz(1:ntiles))
  )
}

#' Produce a lookup for refactor()
#'
#' @description {
#'   The \code{refactor_list} command is a helper function for
#'   \code{\link{refactor}}. It prints the \bold{\code{R}} code requiqred for a
#'   'lookup' to the console, for inclusion in data preparation/cleaning scripts
#'   (perhaps after a bit of editing!).
#'
#'   For vary large lookups, it might make more sense to pass the lookup to
#'   \code{\link{refactor}} using a file. You can write the lookup
#'   to a \code{.csv} file by supplying a path/name to the the \code{file}
#'   argument.
#'
#'   To try and make the process less laborious, \code{refactor_list} also has a
#'   \code{consolidate} parameter. If set to \code{TRUE}, the lookup generated
#'   will pass the 'TO' values through \code{\link{consolidate_values}},
#'   hopefully consoldating factor levels which are different for small
#'   formatting reasons in to one. See the \code{\link{consolidate_values}}
#'   documentation for details.
#'
#'   For a demonstration of how \code{\link{refactor}} and \code{refactor_list}
#'   work together, see the package vignette, with:
#'
#'   \code{vignette('brocks')}
#' }
#'
#' @param x A \code{\link{factor}} (or \code{\link{character}}) variable
#' @param consolidate \code{\link{logical}}. Should the 'TO' values be passed
#'   through \code{\link{consolidate_values}} in an automated attempt to clean
#'   them up?
#' @param file A writable file path. If supplied, the lookup will be written
#'   out to a two column .csv file, as opposed to written to the console. The
#'   file produced can be passed to the file argument in \code{\link{refactor}}
#'
#' @return Nothing. Prints to the console/terminal with \code{\link{cat}}.
#'
#' @export
#' @name refactor_list
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' @seealso \code{\link{refactor}}, the function which \code{rfeactor_list}
#'   supports
#' @examples
#' \dontrun{
#' # Let's tidy up the gender variable in test_data
#' data(test_data)
#' table(test_data$gender)
#'
#' # Passing the gender variable to refactor_list, will generate the R code we
#' # need to create a lookup for it in our data-cleaning script! Setting
#' # consolidate to TRUE will do some of the work for us.
#'
#' refactor_list(test_data$gender, consolidate = TRUE)
#'
#' # At this point you'd take the code generated and itegrate it into your
#' # script. Here's one I made earlier. We can pass it to refactor, and our
#' # factor variable is now tidy!
#'
#' new_vals <- list(
#'   # FROM      TO
#'   c("",        NA     ),
#'   c("<NA>",    NA     ),
#'   c("F",      "female"),
#'   c("Female", "female"),
#'   c("m",      "male"  ),
#'   c("M",      "male"  ),
#'   c("Male",   "male"  ),
#'   c("Man",    "male"  ),
#'   c("Woman",  "female"),
#'   c("n/a",     NA     )
#' )
#'
#' test_data$gender <- refactor(test_data$gender, new_vals)
#'}
refactor_list <- function(x, consolidate = FALSE, file = NULL){
  vals1 <- names(table(x))

  if(consolidate){
    # Run the to values through consolidate_values
    vals2 <- consolidate_values(vals1)
    vals <- c(vals1, vals2)
  } else{
    # If you're not consolidting values, then the from and to values will be the
    # same
    vals <- vals2 <- vals1
  }

  if(!is.null(file)){
    # Write the lookup table out to a file
    if(!assertthat::see_if(assertthat::is.writeable(file)))
      stop("'file' is not a writable path")

    if(!grepl("\\.csv$", file))
      warning("Heads up: Lookup table written to .csv but file name supplied '",
              file, "' does not end in '.csv'")

    utils::write.csv(data.frame(from = vals1, to = vals2), row.names = FALSE)

  } else {
    # Print the lookup table out to a .csv file

    start <- '  c("'
    mid1  <- '", '
    end   <- '),\n'

    # If the 'TO' value is NA, don't put it in quotes
    vals_2_paren <- ifelse(is.na(vals2), ' ', '"')

    spaces1 <- rep_char(" ", max(nchar(vals1)) - nchar(vals1))
    spaces2 <- rep_char(" ", max(nchar(vals2)) - nchar(vals2))

    strings <- paste0(start, vals1, mid1, spaces1, vals_2_paren, vals2,
                      vals_2_paren, spaces2, end)

    # Lose the comma from the last one
    strings[length(strings)] <- gsub("\\),", ")", strings[length(strings)])

    cat(
      '\n# Copy this code into your text editor, and tidy up the values in the',
      'TO column\n',
      '\nnew_vals <- list(\n',
      paste0('  # FROM', rep_char(' ', max(nchar(vals))), 'TO\n'),
      strings,
      ')\n',
      '# You can then pass the new_vals object to refactor() '
    )
  }
}


# Taken from plyr::mapvalues (1.8.2)
# Copyright 2014 Hadley Wickham
# Didn't seem worth making the whole package a dependency for 20 LOC!
#' @keywords internal
mapvalues <- function (x, from, to, warn_missing = TRUE)
{
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  if (is.factor(x)) {
    levels(x) <- mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ",
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
  }
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]
  x
}


#' Convert all columns in a data.frame to character
#'
#' \bold{\code{R}}'s tendency to convert strings to \code{factor}s is well
#' meaning, but occasionally annoying when munging data (e.g. when using
#' \code{\link{rbind}}, or \code{dplyr}'s \code{\link[dplyr]{bind_rows}}).
#' \code{char_cols} turns all variables in a \code{\link{data.frame}} into
#' \code{\link{character}} variables. Optionally, if \code{all} is set to
#' \code{FALSE}, only \code{factor} variables will be converted.
#'
#' @param x A \code{data.frame}
#' @param all Should all the columns (not just those of \code{factor}s) be
#'   converted to \code{character}?
#'
#' @export
#' @name char_cols
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
char_cols <- function(x, all = TRUE){

  f <- function(x){
    if(is.factor(x)){
      as.character(x)
    } else{
      x
    }
  }

  if(all)
    f <- as.character

  x[] <- lapply(x, f)
  x
}


#' Attempt to Rationalise a Factor Variable (using simple rules)
#'
#' @description {
#'   \code{consolidate_values} aims to do some of the work of the grunt work of
#'   rationalising factor levels for you. Using some very simple rules, it will
#'   attempt to turn a vector containing what appear to be gramatical variations
#'   on common patterns, into one with these rationalised to a smaller set of
#'   values.
#'
#'   \itemize{
#'     \item All strings are converted to the same case (lower, by default)
#'     \item Spaces around slashes, before commas, or after common currency
#'       symbols are removed
#'     \item All underscores and full stops are replaced with spaces
#'     \item All continuous whitespace is converted to a single space
#'     \item All trailing and leading whitespace is removed
#'     \item Blank values ("") are converted to \code{NA}
#'     \item Any strings which match \code{na_regex} are converted to \code{NA}.
#'   }
#' }
#'
#' @param x The \code{\link{factor}} variable which you'd like to consolidate
#'   the levels of
#' @param case The case which the levels of \code{x} will be converted to. Can
#'   be one of \code{"lower"}, \code{"upper"}.
#' @param na_regex A regular expression which will be used to set values of
#'   \code{x} to \code{NA}.
#'
#' @return \code{\link{factor}}, hopefully with consolidated levels
#' @export
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
consolidate_values <- function(
  x, case = "lower",
  na_regex = "no info*|don't know|<na>|#na|n/a|^[[:space:]]*$"
){
  case_fun <- switch(case, "lower" = tolower, "upper" = toupper)
  # You should change the encoding here!
  # Change the case
  x <- case_fun(x)
  # Remove any space around slashes, and turn them all to /
  x <- gsub("[[:space:]]*/[[:space:]]*", "/", x)
  # Remove spaces before commas, colons, and semicolons
  # Pretty sure there's a nicer way of doing this with clever regex!
  x <- gsub("[[:space:]]*,[[:space:]]*", ", ", x)
  x <- gsub("[[:space:]]*;[[:space:]]*", "; ", x)
  x <- gsub("[[:space:]]*:[[:space:]]*", ": ", x)
  x <- gsub("[[:space:]]*-[[:space:]]*", " - ", x)
  # Remove spaces after currency symbols (just dollars for now)
  x <- gsub("\\$[[:space:]]*", "$", x)
  # Replace all underscores and full stops with spaces
  x <- gsub("\\.|_", " ", x)
  # Change all whitespace to a single space
  x <- gsub("[[:space:]]+", " ", x)
  # Strip all leading and trailing whitespace
  x <- gsub("^[[:space:]]|[[:space:]]$", "", x)
  # Remove blanks
  x[x == ""] <- NA
  # Remove with the NA regex
  x[grepl(na_regex, x)] <- NA
  x
}


#' Strip Scale Labels from Likert Data
#'
#' Survey systems commonly export data files with values measured by Likert
#' scales coded as \code{\link{character}} as opposed to \code{\link{numeric}},
#' (e.g. \code{"10 - Very Likely"} as opposed to \code{10}). The
#' \code{scale_strip} function pulls the first sequence of digits from a string
#' and returns it as numeric.
#'
#' @param x a \code{\link{vector}}, \code{\link{matrix}}, or
#'   \code{\link{data.frame}} containg \code{\link{character}} data
#'
#' @return \code{\link{numeric}} values stripped from the
#'   \code{\link{character}} values in \code{x}.
#' @export
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
scale_strip <- function(x){
  ss <- function(x)
    as.numeric(stringr::str_extract(x, "[[:digit:]]+"))

  if (is.data.frame(x)){
    # The tbl_df class seems to break the looping process, which is really bad!
    # convert to a standard data.frame
    x <- data.frame(x, check.names = FALSE)
    for(i in 1:ncol(x))
      x[,i] <- ss(x[,i])
  } else if (is.vector(x) | is.factor(x)) {
    x <- ss(x)
  } else {
    stop("x must be a data.frame, factor or vector")
  }

  return(x)
}


#' Turn numbers to strings, with leading zeros
#'
#' Sometimes useful for creating composite primary keys, which order nicely.
#'
#' @name lz
#' @param x A \code{\link{numeric}} \code{\link{vector}} (non numeric values
#'   will be coerced)
#' @param n The character length of the returned strings. Defaults to the length
#'   of the maximum value of x
#' @return A \code{\link{character}} \code{\link{vector}} of those numbers, with
#'   leading zeros
#' @export
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' @examples
#' lz(c(1, 10, 100))
lz <- function(x, n = nchar(max(as.numeric(x)))){
  formatC(as.numeric(x), width = n, format = "d", flag = "0")
}


#' Reverse Factor Levels
#'
#' Reverse the ordering of levels in a factor variable. Sometimes useful when
#' plotting data.
#'
#' @param x A \code{\link{factor}} \code{\link{vector}}. If not, it will be
#'   coerced.
#' @return A A \code{\link{factor}} \code{\link{vector}}, with the ordering of
#'   the levels reversed
#' @export
#' @name rev_levs
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' @examples
#' levels(rev_levs(1:10))
rev_levs <- function(x){
  if(!is.factor(x))
    x <- factor(x)

  factor(x, levels = rev(levels(x)))
}


#' Kill off non-informative columns in a data.frame
#'
#' \code{kill_cols} accepts a \code{\link{data.frame}} or \code{\link{matrix}},
#' and returns another, containg only the columns with more than one unique
#' non-missing value. By default, empty strings ("") are converted to \code{NA},
#' and not considered informative.
#'
#' @name kill_cols
#' @aliases killcols
#' @param x A \code{data.frame}
#' @param empty_strings \code{logical}. Should empty strings ("") be converted
#'   to \code{NA}?
#' @return A \code{data.frame}, less non-varying variables.
#' @export
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' @examples
#'
#' # If we filter test_data down to just those in a particular income group,
#' # income groups no longer vary
#'
#' data(test_data)
#' test_data2 <- test_data[test_data$income == '$0 - $40k',]
#'
#' # Now kill_cols will remove the income column, as it no longer varies
#'
#' kill_cols(test_data2)
#'
kill_cols <- function(x, empty_strings = TRUE){
  n_levs <- function(x){
    if(empty_strings)
      x[grepl("^[[:space:]]*$", x)] <- NA
    length(stats::na.omit(unique(x)))
  }

  x[,as.vector(apply(x, 2, n_levs)) > 1]
}


#' @name kill_cols
#' @export
killcols <- kill_cols


#' Convert Ages or Dates, to a Factor Variable of Standard Age Groups
#'
#' Convert integer/numeric ages, or dates (of birth) to a factor of standard age
#' groups, with presentable labels, ordered from youngest to oldest. The default
#' provides the 'standard' age groups used by much of the market research
#' industry. Custom age breaks can also be used, by passing a vector to the
#' \code{breaks} argument.
#'
#' @note This function does not round ages, and so the common
#'   cultural/numerical interpretation of age works with decimal numbers. For
#'   example someone who has existed for 17.999 years is said to be 17 years
#'   old. The function (with default settings) will process 17 and 17.9999 in
#'   the same fashion.
#'
#' @name age_breaks
#' @param x A \code{\link{vector}} of \code{\link{numeric}} data (ages), or
#'   one of the \code{\link{DateTimeClasses}} for a date of birth
#' @param breaks a A \code{\link{numeric}} \code{\link{vector}} of cutpoints
#' @param right Passed internally to \code{\link{cut}}
#'   Internally passed to \code{\link{cut}}
#' @param ... Additional arguments passed to \code{\link{cut}}
#' @return \code{\link{factor}} age groups
#' @export
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' @examples
#' data(test_data)
#'
#' # A 'rough and ready' way of calculating ages
#' test_data$age <- as.numeric(Sys.Date() - test_data$dob) /365
#'
#' # Let's put those ages into groups
#' test_data$age_group <- age_breaks(test_data$age)
#'
#' # Hooray!
#' table(test_data$age_group)
#'
#' # We could do the above by simply passing in the dates of birth
#' test_data$age_group <- age_breaks(test_data$dob)
#'
age_breaks <- function(x, breaks = c(-Inf, 18, 25, 35, 45, 55, 65, +Inf),
                       right = FALSE, ...){

  # A function for reformatting the break labels which 'cut' comes up with
  # Note: It currently assumes that right = FALSE - you should make it so that
  # it works either way
  age_lab <- function(x){
    levs <- levels(x)
    # The bounds of the range
    bounds <- stringr::str_extract_all(levs, "[[:digit:]]+|-Inf| Inf")

    labs <- unlist(lapply(bounds, function(x){
      if(x[1] == "-Inf")
        return(paste("<", x[2]))
      if(x[2] == " Inf")
        return(paste(x[1], "+"))

      paste(x[1], "-", as.numeric(x[2]) - 1)
    }))

    factor(x, levels = levs, labels = labs)
  }

  if(class(x) %in% c("Date", "POSIXct", "POSIXt")){
    x <- date_to_age(x)
  }

  age_lab(cut(x, breaks, right = right, ...))
}


#' Convert Dates to Ages
#'
#' Convert dates to ages.
#'
#' @name date_to_age
#' @param dob 'Date of Birth'
#' @param refdate Reference date to compare \code{x} with. Defaults to the
#'   current date
#' @return Numeric, 'whole number' ages
#' @export
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' @examples
#' # The argument can take a single age
#' date_to_age("1952-04-29")
#'
#' # Or a whole vector!
#' data(test_data)
#' test_data$age <- date_to_age(test_data$dob)
#'
#' # Historical ages, too. How old were these people yesterday?
#'
#' test_data$age <- date_to_age(test_data$dob, Sys.Date() - 1)
#'
date_to_age <- function (dob, refdate = Sys.time()) {
  lubridate::year(lubridate::as.period(
      lubridate::interval(dob, refdate)
  ))
}
