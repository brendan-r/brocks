#
#
#
#
# #' Reasonably sensible date conversion, from various formats common to Satmetrix systems and Excel
# #'
# #' R's standard \code{\link{Date}} format is the ISO 8601 international standard (e.g. \code{'2012-12-25'}), which is also used by Xperience
# #' (and works very well for a transatlantic company like ours). 11x and Excel have varying defaults, and in addition, odd formats are often in sample files.
# #' This function takes care of common date formats which I've come accross, though if you run
# #' into something unusual - please do sense check results before performing analyses! If data are already in \code{'2012-12-25'} or \code{'2012/12/25'}
# #' format, you can simply use \code{\link{as.Date}}. For odd combinations you may want to use a more flexible function, such as \code{\link{strptime}}.
# #'
# #' @name Date.conv
# #' @aliases Date.conv
# #' @param x a \code{\link{vector}}, containing date or time information
# #' @note Date formats currently converted:
# #' Standard US windows, e.g. mm/dd/yyyy
# #' Standard UK windows, e.g. dd/mm/yyyy
# #' ISO UK windows, e.g. dd/mm/yyyy
# #' All of the above with times on (though these will be killed off), and with or without decimal numbers (e.g. June as '6' or '06').
# #' @return Hopefully, a vector of \code{\link{Date}} data.
# #' @author Brendan Rocks \email{brendan.rocks@@satmetrix.com}
# #' @export
# #' @examples
# #' # A vector of dates formatted with times, and in am ambigious (US) format
# #' x <- c("3/8/2011 2:10:35 AM", "1/25/2010 4:08:52 AM", "9/15/2009 6:13:59 AM", "6/29/2010 6:10:40 AM", "11/15/2011 2:04:19 AM")
# #'
# #' #Success
# #' Date.conv(x)
# #'
# #' # A vector of dates formatted with times, and in am ambigious (UK) format
# #' x <- c("8/3/2011 2:10:35 AM", "25/1/2010 4:08:52 AM", "15/9/2009 6:13:59 AM", "29/6/2010 6:10:40 AM", "15/11/2011 2:04:19 AM")
# #'
# #' #Success
# #' Date.conv(x)
# Date.conv <- function(x){
#   #Differnt Date formats this will handle:
#   # Standard US windows, e.g. mm/dd/yyyy
#   # Standard UK windows, e.g. dd/mm/yyyy
#   # ISO UK windows, e.g. dd/mm/yyyy
#   #All of the above with times on (though these will be killed off)
#   Pc.na <- function(x) mean(is.na(x))
#   dates <- list()
#   #Standard US format without decimal numbers fo days or months
#   dates[[1]] <- as.Date(gsub(" ...+","",x), format="%m/%u/%Y")
#   #Standard US format with slashes, without decimal numbers fo days or months
#   dates[[2]] <- as.Date(gsub(" ...+","",x), format="%m/%d/%Y")
#   #Standard UK format
#   dates[[3]] <- as.Date(gsub(" ...+","",x), format="%d/%m/%Y")
#   #Standard UK format with slashes, without decimal numbers fo days or months
#   dates[[4]] <- as.Date(gsub(" ...+","",x), format="%u/%m/%Y")
#   #Standard ISO format
#   dates[[5]] <- as.Date(gsub(" ...+","",x), format="%F")
#
#   #Return the one with the lowest number of NAs generated
#   return(dates[rank(unlist(lapply(dates, FUN=Pc.na))) == 1][[1]])
# }
#
#
#
#
#
#
#
# # Untested, but would be useful!
# # example usage ggplot() + smx_watermark() + geom_line() ...
# smx_watermark <- function(){
#   library(png)
#   library(grid)
#   # Insert something to write the rasterGrob to a variable within the package
#   # and just use that, if it exists (otherwise, load from disk)
#   img <- readPNG("~/projects/smx_logos/greyscale_top.png")
#   g   <- rasterGrob(img, interpolate=TRUE)
#   annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
# }
#
# # You should put id.na/id_na in here!
# # Import a version of gdata::format_bytes
#
#
# # Convert all columns in a data.frame to character
# char_cols <- function(x){
#   x[] <- lapply(x, as.character)
#   x
# }
#
#
#
#
# # Your ggplot2 theme. You should spend some time on this!
#
# # theme_bw() +
# #   facet_wrap(~title) +
# #   theme(
# #     legend.position    = c(0.85, 0.15),
# #     axis.title.y       = element_text(family = "Ubuntu", size=12),
# #     axis.title.x       = element_text(family = "Ubuntu", size=12),
# #     axis.text.y        = element_text(family = "Ubuntu", size=9),
# #     axis.text.x        = element_text(family = "Ubuntu", size=9),
# #     panel.grid.minor.x = element_line(colour="grey90", size=0.2),
# #     strip.text.x       = element_text(family = "Ubuntu", colour = 'white'),
# #     legend.background  = element_rect(fill=alpha('white', 0)),
# #     strip.background   =
# #       element_rect(
# #         colour = primary_cols[2], fill = "#424142"
# #       )
# #   )
#
#
#
#
#
# # x <- c("a", "b", "c")
# #
# # new_values <- list(
# #   c("a", "A"),
# #   c("c", "C")
# # )
#
#
#
# refactor <- function(x, new_values){
#
#   valid <- all(
#     unlist(lapply(new_values, function(x) dim(x) == 2 & length(x) == 2))
#   )
#
#   if(!is.list(new_values) | !valid){
#     stop("new_values must be a list of vectors, each of length 2")
#   }
#
#   from <- unlist(lapply(new_values, function(x) x[1]))
#   to   <- unlist(lapply(new_values, function(x) x[2]))
#
#   mapvalues(x, from, to)
# }
#
# # x <- c("a", "b", "c")
# #
# # new_values <- list(
# #   c("a", "A"),
# #   c("c", "C")
# # )
# #
# # [1] "A" "b" "C"
#
#
# # You should file export to a csv
# refactor_list <- function(x, filename = NULL){
#   vals <- names(table(x))
#
#   start <- '  c("'
#   mid1  <- '", '
#   mid2  <- '"'
#   end   <- '),\n'
#
#   spaces <- char_rep(" ", max(nchar(vals)) - nchar(vals))
#
#   strings <- paste0(start, vals, mid1, spaces, mid2, vals, '"', spaces, end)
#
#   # Lose the comma from the last one
#   strings[length(strings)] <- gsub("\\),", ")", strings[length(strings)])
#
#   cat(
#     '\n# Copy this code into your text editor, and tidy up the values in the',
#     'TO column\n',
#     '\nnew_vals <- list(\n',
#     paste0('  # FROM', char_rep(' ', max(nchar(vals))), 'TO\n'),
#     strings,
#     ')\n',
#     '# You can then pass the new_vals object to refactor() '
#   )
# }
#
# # Taken from plyr::mapvalues (1.8.2)
# mapvalues <- function (x, from, to, warn_missing = TRUE)
# {
#   if (length(from) != length(to)) {
#     stop("`from` and `to` vectors are not the same length.")
#   }
#   if (!is.atomic(x)) {
#     stop("`x` must be an atomic vector.")
#   }
#   if (is.factor(x)) {
#     levels(x) <- mapvalues(levels(x), from, to, warn_missing)
#     return(x)
#   }
#   mapidx <- match(x, from)
#   mapidxNA <- is.na(mapidx)
#   from_found <- sort(unique(mapidx))
#   if (warn_missing && length(from_found) != length(from)) {
#     message("The following `from` values were not present in `x`: ",
#             paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
#   }
#   x[!mapidxNA] <- to[mapidx[!mapidxNA]]
#   x
# }
