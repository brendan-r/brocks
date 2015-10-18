#' Open a file in the Operating System
#'
#' Open a file in the operating system, using it's default program.
#' \code{sys_open} should work on Ubuntu (and other Linux variants), OSX and
#' Windows.
#'
#' @param f The path of the file to open
#' @return Nothing. Used for it's side effect.
#'
#' @author Based very heavily on the function \code{openFileInOS} from the
#'   package \code{pander} (v0.5.2), written by Gergely Daroczi
#'   (\email{daroczig@@rapporter.net}), itself based on the \code{convert}
#'   function in the package \code{ascii}, written by David Hajage
#'   (\email{dhajage@@gmail.com}).
#'
#' @export
sys_open <- function (f){
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
#' @aliases filenamize
filenamise <- function(x, sep_char = "_", ext = ""){
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
#' @name filenamise
filenamize <- filenamise


#' File Structure for a Jekyll Blog Post
#'
#' @param title The title of the blog post
#' @param serve Should \code{\link{blog_serve}} be run once the files have
#'   been set-up? Defatuls to \code{TRUE}.
#' @param dir The directory the post (or subdirectory) should reside in
#' @param subdir Should the post live in a subdirectory? Defaults to \code{TRUE}
#' @param skeleton_file The filepath of a skeleton blog post which will be used
#'   as the basis for the basis for the newly created file
#'
#' @details {
#'   \code{new_post} will create a .R file, and a .Rmd file (by default in a
#'   subdirectory), with names created by running \code{title} through
#'   \code{\link{filenamise}}. The .R file will contain a short note mentioning
#'   that it accompanies the .Rmd file, which will contain the same text as the
#'   file supplied by \code{skeleton_post} paramter. Both files will be opened
#'   using \code{\link{sys_open}}.
#' }
#'
#' @export
new_post <- function(title = "new post", serve = TRUE, dir = "_source",
                     subdir = TRUE, skeleton_file = ".skeleton_post"){

  if(!dir.exists(dir)){
    stop("The directory '", dir, "' doesn't exist. Are you running R in
         the right directory?")
  }

  # Sanitise the post title
  fname <- filenamise(title, sep_char = "-")

  if(subdir){
    fpath <- file.path(dir, fname)
    dir.create(fpath)
  } else {
    fpath <- dir
  }

  rmd_name <- file.path(fpath, paste0(Sys.Date(), "-", fname, ".Rmd"))
  r_name   <- file.path(fpath, paste0(fname, ".R"))

  # Read in the skeleton post
  # If it doesn't exist, emit a warning and use the package default
  if(!file.exists(skeleton_file)){
    message("File .skeleton_post does not exist. Using package default")
    skeleton_file <- system.file("skeleton_post.Rmd", package = "brocks")
  }

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

  if(serve)
    blog_serve()
}


#' Serve or Compile a Jekyll Blog
#'
#' Serve or Compile a Jekyll Blog. A small wrapper around
#' servr::\code{\link{jekyll}}, which by default also looks for subdirectories
#' witin the main source directory.
#'
#' @param input passed to servr::\code{\link{jekyll}}
#' @param output passed to servr::\code{\link{jekyll}}
#' @param ... passed to servr::\code{\link{jekyll}}
#'
#' @export
blog_serve <- function(
  input  = c(".", list.dirs("_source")),
  output = c(".", rep("_posts", length(list.dirs("_source")))),
  ...
){
  servr::jekyll(input = input, output = output, serve = TRUE, ...)
}


#' @rdname blog_serve
#' @export
blog_gen <- function(
  input  = c(".", list.dirs("_source")),
  output = c(".", rep("_posts", length(list.dirs("_source")))),
  ...
){
  servr::jekyll(input = input, output = output, serve = FALSE, ...)
}


#' Push a blog post live (possibly)
#'
#' I use this function to push blog posts live, as on my machine I have a bash
#' alias \code{blog_push} which does this. This is an incredibyly lazy wrapper
#' for \code{system(command)}, where \code{command} is by default the bash alias
#' I use.
#'
#' @param command Something which will be executed by system
#'
#' @details {
#'   If it's of any interest, my \code{push_blog} bash alias is:
#'
#'   \code{'aws s3 sync /home/br/projects/brendanrocks.com/_site \
#'          s3://brendanrocks.com --exclude "cache/*|README.md" --delete'}
#'}
#'
#' @return Used for its side effects.
#' @export
blog_push <- function(command = "blog_push") {
  system(command)
}

#' Set some knitr chunk options which may work well for blogging
#'
#' A small wrapper around knitr's \code{\link[knitr]{opts_chunk}}$set, with some
#' defaults which I've found work well for blog posts. All messages from R are
#' surpressed, and the quality of the plots is increased to 6" X 6" 300 dpi
#' \code{\link{png}}s.
#'
#' @param ... passed to knitr::\code{\link[knitr]{opts_chunk}}$set
#'
#' @export
blog_opts <- function(...){
  library(knitr)
  knitr::opts_chunk$set(
    echo    = FALSE,
    warning = FALSE,
    error   = FALSE,
    message = FALSE,
    device  = 'png',
    fig.height = 6,
    fig.width  = 6,
    dpi = 300,
    ...
  )
}
