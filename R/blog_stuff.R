#' Open a file in the Operating System
#'
#' Open a file in the operating system, using it's default program.
#' \code{sys_open} should work on Ubuntu (and other Linux variants), OSX and
#' Windows.
#'
#' @param f The path of the file to open
#' @return Nothing. Used for it's side effect.
#'
#' @export
sys_open <- function (f){
  # Taken from:
  #   Package: pander
  #   Maintainer: Gergely Daróczi <daroczig@rapporter.net>
  #     Title: An R Pandoc Writer
  #   Version: 0.5.2

  if (missing(f))
    stop("No file to open!")
  f <- path.expand(f)
  if (!file.exists(f))
    stop("File not found!")
  if (grepl("w|W", .Platform$OS.type)) {
    shell.exec(f)
  }
  else {
    if (grepl("darwin", version$os))
      system(paste(shQuote("open"), shQuote(f)), wait = FALSE,
             ignore.stderr = TRUE)
    else system(paste(shQuote("/usr/bin/xdg-open"), shQuote(f)),
                wait = FALSE, ignore.stdout = TRUE)
  }
}


#' Sanitise a String (URL/filename safe)
#'
#' Sanitise a string (removing puctuation and spaces), so that it can safely be
#' used in a URL or file path.
#'
#' @param x The string to be santised
#' @param sep_char The character to use in place of spaces/punctuation found in
#'   \code{x}
#' @param ext A file extenstion to be appended to the end of the result
#'
#' @return \code{character}
#'
#' @export
#' @aliases filenameize
filenameise <- function(x, sep_char = "_", ext = ""){
  paste0(
    gsub(
      paste0(sep_char, "$|^", sep_char), "",
      gsub(
        paste0(sep_char, "+"), sep_char,
        gsub("[[:space:]]|[[:punct:]]", sep_char, tolower(x))
      )
    ),
    ext
  )
}

#' @export
#' @name filenameise
filenameize <- filenameise


#' File Structure for a Jekyll Blog Post
#'
#' @param title The title of the blog post
#' @param dir The directory the post (or subdirectory) should reside in
#' @param subdir Should the post live in a subdirectory? Defaults to \code{TRUE}
#' @param skeleton_file The filepath of a skeleton blog post which will be used
#'   as the basis for the basis for the newly created file
#'
#' @details {
#'   \code{new_post} will create a .R file, and a .Rmd file (by default in a
#'   subdirectory), with names created by running \code{title} through
#'   \code{\link{filenameise}}. The .R file will contain a short note mentioning
#'   that it accompanies the .Rmd file, which will contain the same text as the
#'   file supplied by \code{skeleton_post} paramter. Both files will be opened
#'   using \code{\link{sys_open}}.
#' }
#'
#' @export
new_post <- function(title = "new post", dir = "_source", subdir = TRUE,
                     skeleton_file = "skeleton_post"){

  # Sanitise the post title
  fname <- filenameise(title, sep_char = "-")

  if(subdir){
    fpath <- file.path(dir, fname)
    dir.create(fpath)
  } else {
    fpath <- dir
  }

  rmd_name <- file.path(fpath, paste0(Sys.Date(), "-", fname, ".Rmd"))
  r_name   <- file.path(fpath, paste0(fname, ".R"))

  # Read in the skeleton post
  post <- readLines(skeleton_file)
  post[grepl("title: ", post)] <- paste0("title:  ", title)
  writeLines(post, rmd_name)

  # Write out an empty R file as well, in case that's useful
  writeLines(
    c("# This R file accomanies the .Rmd blog post",
      paste("#", rmd_name),
      "", ""
    ),
    r_name
  )

  sys_open(r_name)
  sys_open(rmd_name)
}



#' Serve or Compile a Jekyll Blog
#'
#' Serve or Compile a Jekyll Blog. A small wrapper around
#' servr::\code{\link{jekyll}}, which be default looks for subdirectories witin
#' the main source directory.
#'
#' @param input passed to servr::\code{\link{jekyll}}
#' @param output passed to servr::\code{\link{jekyll}}
#' @param ... passed to servr::\code{\link{jekyll}}
#'
#' @export
blog_run <- function(
  input  = c(".", list.dirs("_source")),
  output = c(".", rep("_posts", length(list.dirs("_source")) - 1)),
  ...
){
  servr::jekyll(input = input, output = output, ...)
}
