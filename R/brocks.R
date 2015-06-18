#' Turn numbers to strings, with leading zeros
#'
#' Often useful for creating composite primary keys which order nicely.
#'
#' @name lz
#' @aliases lz
#' @param x A \code{\link{numeric}} \code{\link{vector}} (non numeric values will be coerced)
#' @param n The character length of the returned strings. Defaults to the length of the maximum value of x
#' @return A \code{\link{character}} \code{\link{vector}} of those numbers, with leading zeros
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
#' \code{kill_cols} accepts a \code{\link{data.frame}} or \code{\link{matrix}}, and returns another, containg only the columns with more than one unique non-missing value. By default, empty strings ("") are converted to \code{NA}, and not considered informative.
#'
#' @name kill_cols
#' @aliases killcols
#' @param x A \code{data.frame}
#' @param empty_strings \code{logical}. Should empty strings ("") be converted
#'   to \code{NA}?
#' @return A \code{data.frame}, less non-varying variables.
#' @export
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
kill_cols <- function(x, empty_strings = TRUE){
  n.lev <- function(x){
    if(empty_strings)
      x[x == ""] <- NA
    length(table(x)[table(x) > 0])
  }

  x[,as.vector(apply(x, 2, n.lev)) > 1]
}

#' @name kill_cols
#' @export
killcols <- kill_cols


#' Convert Integer/Numeric Ages to a Factor Variable of Standard Age Groups
#'
#' Convert integer/numeric ages to a factor, of standard age groups, with
#' presentable labels, ordered from youngest to oldest. The default provides the
#' 'standard' age groups used by much of the market research industry. Custom
#' age breaks can also be used, by passing a vector to the \code{breaks}
#' argument.
#'
#' @note This function does not round up ages, and so the common
#'   cultural/numerical interpretation of age works with decimal numbers. For
#'   example someone who has existed for 17.99 years is said to be an 17 year
#'   old. The function (with default settings) will process 17 and 17.9999 in
#'   the same fashion.
#'
#' @name age_breaks
#' @param x A \code{\link{numeric}} \code{\link{vector}}
#' @param breaks a A \code{\link{numeric}} \code{\link{vector}} of cutpoints.
#' @param right Passed internally to \code{\link{cut}}
#'   Internally passed to \code{\link{cut}}.
#' @param ... Additional arguments passed to \code{\link{cut}}
#' @return A \code{\link{character}} \code{\link{vector}} of those numbers, with leading zeros
#' @export
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
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

  age_lab(cut(x, breaks, right = right, ...))
}


#' Standard errors for proportions using Agresti-Coull intervals
#'
#' Agresti-Coull (1998) intervals are a great way to get a quick and
#' non-terrible estimate of a proportion. They work by using a 'Wald' interval,
#' after the addition of 2 successes and 2 failures to the sample (other numbers
#' can be specified, via the \code{wt} argument).
#'
#'
#' @name ac_se
#' @param logical_var A \code{\link{logical}} \code{\link{vector}}
#' @param wt The number of successes and failures to add to the sample before
#'   construction of a Wald interval
#' @return \code{\link{numeric}}. An estimate of the sample's standard error.
#' @export
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
ac_se <- function(logical_var, wt = 2){
  x <- sum(logical_var)
  n <- sum(!logical_var)

  x_hat <- x + wt
  n_hat <- n + wt * 2
  p_hat <- x_hat / n_hat

  sqrt((p_hat * (1 - p_hat))/ n_hat)
}

#' Repeat a character a variable number of times
#'
#' Effectively a version of \code{\link{rep}}, where only once value can be
#' repeated (by default, a space; " "), but it can be repeated a variable number
#' of times. Useful for creating even spacing for print and summary methods.
#'
#' @name rep_char
#' @param x A value to repeat. Will be coerced to \code{\link{character}}.
#' @param times A \code{\link{numeric}} \code{\link{vector}}; the number of
#'   times that \code{x} should be repeated.
#' @return A \code{\link{character}} \code{\link{vector}} of x repated various
#'   times
#' @export
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' @examples
#'
#' # Strings repeating 'a' a variable number of times!
#' rep_char("a", 1:5)
#'
#' # Slightly more useful. Some text strings which we'd like to present:
#' desc <- c("\n", "first : 1st\n", "second : 2nd\n", "third : 3rd\n",
#' "fourth : 4th\n", "umpteenth : ...\n")
#'
#' # However, the varying lengths make them look a little awkward
#' cat(desc)
#'
#' # We can use rep_char to add extra spaces to the strings which are shorter than
#' # the longest
#' desc_spaced <- paste0(rep_char(times = max(nchar(desc)) - nchar(desc)), desc)
#'
#' # Much better
#' cat(desc_spaced)
#'
rep_char <- function(x = " ", times){
  unlist(lapply(times, function(y){paste(rep(x, y), collapse = "")}))
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
  x, case = "lower", na_regex = "no info*|don't know|<na>|#na|n/a"
){
  case_fun <- switch(case, "lower" = tolower, "upper" = toupper)
  # You should change the encoding here!
  # Change the case
  x <- case_fun(x)
  # Remove any space around slashes, and turn them all to /
  x <- gsub("[[:space:]]*/[[:space:]]*", "/", x)
  # Remove spaces before commas, colons, and semicolons
  # Pretty sure there's a nicer way of doing this with clever regex!
  x <- gsub("[[:space:]]*,", ",", x)
  x <- gsub("[[:space:]]*;", ";", x)
  x <- gsub("[[:space:]]*:", ":", x)
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
#' \code{\link{data.frame}} containg \code{\link{character}} data
#'
#' @return \code{\link{numeric}} values stripped from the \code{\link{character}}
#'   values in \code{x}.
#' @export
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
scale_strip <- function(x){
  ss <- function(x)
    as.numeric(stringr::str_extract(x, "[[:digit:]]+"))

  if (is.data.frame(x)){
    for(i in 1:ncol(x))
      x[,i] <- ss(x[,i])
  } else if (is.vector(x)) {
    x <- ss(x)
  } else {
    stop("x must be a data.frame, or vecotr")
  }

  return(x)
}

#' Format Numeric Data with HTML Arrows
#'
#' @description {
#' When producing numbers in R markdown documents, it can be nice to try and
#' draw readers' attention to increases and decreases. The \code{html_tri}
#' function takes a numeric vector, and returns a \code{\link{character}}
#' vector of HTML strings, which will render in an (R) markdown document as
#' numbers accompanied with a little green 'upward' triangle
#' for positive numbers, and a little red 'downward' triangle for
#' negative ones, and a little black square for numbers which are exactly 0 by
#' default. The colours can be altered by passing valid CSS colour values to the
#' \code{colours} argument, and the symbols by passing valid HTML character
#' values to the \code{symbols} argument. The default values are in HTML decimal
#' character codes.
#'
#' If you'd only like to green/red triangles for some non-zero numbers, you can
#' use the subset argument to pass a \code{\link{logical}} vector (the same)
#' length as \code{x} to \code{html_tri}. This will mean that only elements of
#' \code{x} will get a traingle when they are non-negative \emph{and}
#' \code{subset} is \code{TRUE}.
#' }
#'
#' @param x A \code{\link{numeric}} \code{\link{vector}}
#' @param format A function used to format the numbers before the HTML for the
#'   triangles is added.
#' @param subset A \code{logical} vector. Should elements of \code{x} get
#'   coloured arrows (as opposed to the symbol for 'nochange')?
#' @param symbols The symbols to use for increases, decreases, and things
#'   not chaning respectively. Must a a vector of length 3, the entries having
#'   the names \code{"up"}, \code{"down"}, and \code{"nochange"}
#' @param colours As above, but for the colours of the symbols
#'
#' @return A vector of \code{\link{character}} values, containing HTML so that
#'   they should render with green/red triangles in an HTML document.
#'   values in \code{x}.
#' @export
#' @name html_tri
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
html_tri <- function(
  x, format = round, subset = TRUE,
  symbols = c(up = "&#9650;", down = "&#9660;", nochange = "&#9632;"),
  colours = c(up = "green", down = "red", ncochange = "black")
){

  arrow_fun <- function(x, dir){
    paste0("<a style='color:", colours[dir], "'>", symbols[dir], "</a><a>",
           format(x), "</a>")
  }

  dir_fun <- function(x){
    ifelse(!sign(x) | !subset, "nochange", ifelse(x > 0, "up", "down"))
  }

  mapply(arrow_fun, x, dir_fun(x))
}



# plus_minus <- function(x, ...){
#   paste0(ifelse(x > 0, "+", ""), round(x, ...))
# }
#
# plus_minus_percent <- function(x, ...){
#   paste0(ifelse(x > 0, "+", ""), round(x * 100, ...), "%")
# }
#
# plus_minus_nps <- function(x, ...){
#   paste0(ifelse(x > 0, "+", ""), round(x * 100, ...))
# }
#
# # A vectorized version of switch
# vswitch <- function(EXPR, ...){
#   unlist(lapply(EXPR, function(x) switch(x, ...)))
# }
