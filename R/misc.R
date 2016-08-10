#' 'Agresti-Coull'ish Standard Errors
#'
#' Agresti-Coull (1998) intervals are a great way to get a quick and
#' non-terrible estimate of a proportion. They work by using a 'Wald' interval,
#' after the addition of 2 successes and 2 failures to the sample (other numbers
#' can be specified, via the \code{wt} argument). This function creates a
#' Wald-style standard-error, after adding psuedo-responses.
#'
#' @name ac_se
#' @param logical_var A \code{\link{logical}} \code{\link{vector}}
#' @param wt The number of successes and failures to add to the sample before
#'   construction of a Wald interval
#' @return \code{\link{numeric}}. An estimate of the sample's standard error.
#' @export
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' @references {Agresti, A., & Coull, B. A. (1998). Approximate is better than
#'   "exact" for interval estimation of binomial proportions. \emph{The American
#'   Statistician}, 52(2), 119-126.}
#' @examples
#' ac_se(as.logical(round(runif(10))))
ac_se <- function(logical_var, wt = 2){
  x <- sum(logical_var)
  n <- sum(!logical_var)

  x_hat <- x + wt
  n_hat <- n + wt * 2
  p_hat <- x_hat / n_hat

  sqrt((p_hat * (1 - p_hat))/ n_hat)
}


#' A wrapper for the googlesheets package: A lazy way to read a googlesheet
#'
#' This auths (via \code{\link{gs_auth}}), finds a sheet, and reads it (via
#' \code{\link{gs_read}}). It assumes that you've already set-up your computing
#' evironment to use the the \code{\link{googlesheets}} pacakge; see it's
#' documentation for more details.
#'
#' @param key   Passed to \code{\link{gs_key}}
#' @param title Passed to \code{\link{gs_title}}
#' @param url   Passed to \code{\link{gs_url}}
#'
#' @return The results of \code{\link{gs_read}}
#' @export
read_gs <- function(key = NULL, title = NULL, url = NULL){

  if (length(c(key, title, url)) != 1L) {
    stop("Only one sheet parameter may be supplied.")
  }

  # Auth
  googlesheets::gs_auth()

  # Use the right fun for the param
  if(!is.null(key))
    gs_obj <- googlesheets::gs_key(key)

  if(!is.null(title))
    gs_obj <- googlesheets::gs_title(title)

  if(!is.null(url))
    gs_obj <- googlesheets::gs_url(url)

  out <- googlesheets::gs_read(gs_obj)

  # Should be exported by the next version:
  # https://github.com/jennybc/googlesheets/commit/61042d
  # googlesheets::gs_deauth()

  return(out)
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
#' # We can use rep_char to add extra spaces to the strings which are shorter
#' # than the longest
#' desc_spaced <- paste0(rep_char(times = max(nchar(desc)) - nchar(desc)), desc)
#'
#' # Much better
#' cat(desc_spaced)
#'
rep_char <- function(x = " ", times){
  unlist(lapply(times, function(y){paste(rep(x, y), collapse = "")}))
}


#' Format Numeric Data with HTML Arrows
#'
#' @description {
#'   When producing numbers in R markdown documents, it can be nice to try and
#'   draw readers' attention to increases and decreases. The \code{html_tri}
#'   function takes a numeric vector, and returns a \code{\link{character}}
#'   vector of HTML strings, which will render in an (R) markdown document as
#'   numbers accompanied with a green 'upward' triangle for positive numbers, a
#'   red 'downward' triangle for negative ones, and a black square for numbers
#'   which are exactly 0 by default. The colours can be altered by passing valid
#'   CSS colour values to the \code{colours} argument, and the symbols by
#'   passing valid HTML character values to the \code{symbols} argument. The
#'   default values are in HTML decimal character codes.
#'
#'   If you'd only like to green/red triangles for some non-zero numbers, you
#'   can use the subset argument to pass a \code{\link{logical}} vector (the
#'   same) length as \code{x} to \code{html_tri}. This will mean that only
#'   elements of \code{x} will get a traingle when they are non-negative
#'   \emph{and} \code{subset} is \code{TRUE}.
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
#' @examples
#' # This will output 'raw' HTML. To see the final result in an HTML markdown
#' # document, see the package vignette; vignette("brocks")
#'
#' html_tri(runif(10))
#'
#' # You could use other HTML symbols, even emojis if you like!
#' # These are HTML decimal codes (only unicode allowed in R packages), but
#' # you could use any valid characters (e.g. copy and paste)
#'
#' html_tri(runif(10), symbols = c("up" = "&#128522;", "down" = "&#128542;",
#'   "nochange" = "&#128528;"))
#'
html_tri <- function(
  x, format = round, subset = TRUE,
  symbols = c(up = "&#9650;", down = "&#9660;", nochange = "&#9632;"),
  colours = c(up = "green", down = "red", nochange = "black")
){

  arrow_fun <- function(x, dir){
    paste0("<a style='color:", colours[dir], "'>", symbols[dir], "</a><a>",
           format(x), "</a>")
  }

  dir_fun <- function(x){
    ifelse(!sign(x) | !subset, "nochange", ifelse(x > 0, "up", "down"))
  }

  arrow_fun(x, dir_fun(x))
}


#' Miscellaneous Number Formatting Functions
#'
#' @description {
#'   Sometimes (for example when illustrating differences), it can be useful for
#'   positive numbers to be prefixed by a + sign, just as negative numbers are
#'   with a - sign. The following are a few (very simple) wrapper functions
#'   which do this.
#'
#'   \describe{
#'     \item{\bold{\code{fmt_pm}}}{ Is a wrapper for \code{\link{round}},
#'       which also \code{\link{paste}}s a + sign before positive numbers
#'     }
#'     \item{\bold{\code{fmt_pc}}}{ A simple formatting function for
#'       percentages. Defaults to 0 decimal places
#'     }
#'     \item{\bold{\code{fmt_pc_pm}}}{ As above, but with a + prefix for
#'       positive numbers
#'     }
#'     \item{\bold{\code{format_nps}}}{ A very simple formatter for the Net
#'       Promoter Score
#'     }
#'     \item{\bold{\code{fmt_nps_pm}}}{ As above, but without the percentage
#'       sign
#'     }
#'     \item{\bold{\code{unsci}}}{ Unscientific notation: Short colloquial
#'       number formatting. For example, 1e+04 becomes "100k", 1.454e+09 becomes
#'       "1.5B", etc.
#'     }
#'     \item{\bold{\code{unsci_dollars}}}{ A convenience function for the above,
#'       with \code{currency = TRUE} as the default.
#'     }
#'   }
#' }
#'
#' @param x \code{\link{numeric}} data to format
#' @param currency Should numbers be prefixed with \code{symbol}?
#' @param symbol if \code{currency = TRUE}, a string to prefix numbers with
#' @param ... Passed to \code{\link{round}}
#' @param digits Parameter passed to \code{\link{round}}
#' @param type The truncation function for the number, in the context of the
#'   text in which its likely to be formatted. One of \code{round} (the
#'   \code{round} function is used), \code{greater} (the \code{floor} function
#'   is used) or \code{less} (the \code{ceiling} function is used).
#'
#' @return \code{\link{character}}.
#'
#' @export
#' @name misc_br_num_formats
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#'
fmt_pm <- function(x, ...){
  paste0(ifelse(x > 0, "+", ""), round(x, ...))
}

#' @name misc_br_num_formats
#' @export
fmt_pc <- function(x, type = c("round", "greater", "less"), digits = 0) {
  f <- switch(type[1], round = round, greater = floor, less = ceiling)
  paste0(f(x * 100 * 10^(digits)) / 10^(digits), "%")
}

#' @name misc_br_num_formats
#' @export
fmt_pc_pm <- function(x, ...){
  paste0(ifelse(x > 0, "+", ""), fmt_pc(x, ...))
}

#' @name misc_br_num_formats
#' @export
fmt_nps_pm <- function(x, ...){
  paste0(ifelse(x > 0, "+", ""), round(x * 100, ...))
}

#' @name misc_br_num_formats
#' @export
fmt_nps <- function(x, ...){
  paste0(round(x * 100, ...))
}

#' @name misc_br_num_formats
#' @export
unsci <- function(x, digits = 1, currency = FALSE, symbol = "$") {
  r <- function(x) round(x, digits)
  k <- function(x) paste0(r(x / 1e+03), "k")
  M <- function(x) paste0(r(x / 1e+06), "MM")
  B <- function(x) paste0(r(x / 1e+09), "B")

  # Based on the size of the number, add the prefix. The `paste0("", ...` part
  # is to coerce NAs to character, follwing the behaviour of the scales package
  prefixed <- paste0("", ifelse(
    x >= 1e+03 & x < 1e+06, k(x),
    ifelse(
      x >= 1e+06 & x < 1e+09, M(x), ifelse(x >= 1e+09, B(x), r(x))
    )
  ))

  # Append dollars
  if (currency) {
    prefixed <- paste0(symbol, prefixed)
  }

  # Add spaces to make uniform widths, and return
  paste0(rep_char(times = max(nchar(prefixed)) - nchar(prefixed)), prefixed)
}

#' @name misc_br_num_formats
#' @export
unsci_dollars <- function(x, ...) unsci(x, currency = TRUE, ...)



#' A vectorized version of switch
#'
#' A vectorized version of \code{\link{switch}}.
#'
#' @param EXPR As in \code{switch}, an expression which evaluated to a number or
#'   character string. However, in \code{vswitch}, there can be more than one.
#'
#' @param ... Passed to \code{switch}
#'
#' @export
#' @name vswitch
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' @examples
#'
#' # The usual version of 'switch' works perfectly with one value
#' x <- "a"
#' switch(x, a = 1, b = 2, c = 3)
#'
#' # But not with more than one
#' x <- letters[1:3]
#' \dontrun{switch(x, a = 1, b = 2, c = 3)}
#'
#' # vswitch works well where you'd like to 'switch' a vector
#' x <- letters[1:3]
#' vswitch(x, a = 1, b = 2, c = 3)
#'
#'
vswitch <- function(EXPR, ...){
  unlist(lapply(EXPR, function(x) switch(x, ...)))
}


#' Extract package dependencies from an R script
#'
#' @param file A file containing R code to parse
#'
#' @return A character vector containing the names of packages used in
#'   \code{file}
#' @export
extract_package_deps <- function(file) {
  # Read file, strip comments and empty lines
  txt <- readLines(file) %>% gsub("#.*$", "", .) %>%
    subset(!grepl("^$|^[[:space:]]$", .))

  # Find inline references to packages, like package::function or
  # package:::function
  inline  <- txt %>% stringr::str_extract_all("[[:alnum:]_\\.]*:{2,3}") %>%
    unlist() %>% gsub(":{2,3}", "", .)

  # Find references to packages via library(package) or require(package)
  lib_reqs <- txt %>% stringr::str_extract_all(
      "library\\([[:alnum:]_\\.]*\\)|require\\([[:alnum:]_\\.]*\\)"
    ) %>% unlist() %>% gsub("library\\(|require\\(|\\)", "", .)

  # Find some special operators which are commonly associated with certain
  # packages

  txt <- paste(txt, collapse = "\n")

  magrittr   <- if (grepl("%$%|%>%|%<>%|%T>%", txt))
    "magrittr"

  data.table <- if (grepl("%like%|%between%|%inrange%|%chin%", txt))
    "data.table"

  future     <- if (grepl("%<-%|%->%|%<=%|%=>%|%plan%|%tweak%", txt))
    "future"

  ops_packages <- c(magrittr, data.table, future)

  out <- c(inline, lib_reqs, ops_packages) %>% stats::na.omit() %>% unique()
  out[out != ""]
}


#' Return a CRAN repo: The user-default, or RStudio's
#'
#' Package installation on remote machines depends on a CRAN repo, which can be
#' tricky to set non-interactively. This simple wrapper function looks to see if
#' a default CRAN mirror has already been set. If it has, it is returned. If
#' not, \code{fallback} is returned.
#'
#' @return Either \code{fallback}, or the result of
#'   \code{getOption("repos")["CRAN"]}
#'
#' @keywords internal
cran_repo <- function(fallback = "https://cran.rstudio.com/") {
  default_repo <- getOption("repos")["CRAN"]
  # Is there a default set that can be contacted over http(s)? (The default if
  # unset seems to be "@CRAN@", hence the http check)
  if (!grepl("^http", default_repo)) {
    return(fallback)
  } else {
    return(default_repo)
  }
}

#' Recursively Parse R Files in a Directory, and Install Packages Used
#'
#' \code{install_deps} (recursively) finds R code files in a directory, and uses
#' regular expressions to find code that looks like it refers to an R package
#' (via \code{\link{extract_package_deps}}). It then extracts the names of all
#' of these packages, checks that they're not already installed, and that they
#' are on CRAN, and then installs them (via \code{\link{install.packages}}).
#'
#' @param dir The directory to search for R files to parse
#' @param file_pattern A regular expression used to determine whether a file
#'   should be parsed or not. The default will parse only \code{.R} and
#'   \code{.Rmd} files
#' @param cran_mirror The CRAN mirror to use. The default calls a small function
#'   which returns the Rstudio mirror, if no current default exists
#' @param ... Passed to \code{\link{install.packages}}
#'
#' @return Used for it's side effects (the installation of packages)
#' @export
install_deps <- function(dir = getwd(), file_pattern = "\\.R$|\\.Rmd$",
                         cran_mirror = cran_repo(), ...) {
  file_list <- list.files(dir, recursive = TRUE) %>% .[grepl(file_pattern, .)]

  package_list <- file_list %>% lapply(extract_package_deps) %>% unlist() %>%
    unique

  # Let the user know which files you've scanned
  message("Searching...\n    ", paste(file_list, collapse = "\n    "), "\n")

  # Vector of installed packages
  installed <- utils::installed.packages()[,1]

  to_install        <- package_list[!package_list %in% installed]
  already_installed <- package_list[package_list %in% installed]

  if (length(already_installed) > 0) {
    message("The following packages are already installed -- no action taken:",
            "\n", paste(already_installed, collapse = ", "))
  }

  # Get a list of everything on CRAN. Surprisingly fast!
  cran_packages <- utils::available.packages(contrib.url(cran_mirror))

  on_cran     <- to_install[ to_install %in% cran_packages]
  not_on_cran <- to_install[!to_install %in% cran_packages]

  if (length(not_on_cran) > 0) {
    warning("The following packages are not available on CRAN, and have not ",
            "been installed:\n", paste(not_on_cran, collapse = ", "))
  }

  # If there's nothing to do, end
  if (!length(on_cran) > 0) {
    message("\n\nUp to date.\n")
    return(invisible())
  }

  # Otherwise, install stuff
  if (length(on_cran) > 0) {
    message("Installing the following packages:\n\n",
            paste(on_cran, collapse = ", "))
    utils::install.packages(on_cran, repos = cran_mirror, ...)
  }
}





#' An idealised test data set, for demonstrating some of the functions
#'
#' An idealised test data set, for demonstrating some of the functions in the
#' package
#'
#' @name test_data
#' @docType data
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' @keywords data
NULL
