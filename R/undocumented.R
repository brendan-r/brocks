

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
#
# publish_post <- function(filename = NULL, yaml = TRUE, drafts_dir = "_drafts",
#                          posts_dir = "_posts"){
#
#   drafts <- list.files(drafts_dir)
#
#   if(length(drafts > 1)){
#     stop("No files in the '_drafts' folder")
#   }
#
#   cat("Which draft post would you like moved to posts?")
#   selection <- menu(drafts)
#
#   file.rename(
#     file.path(drafts_dir, drafts[selection]),
#     file.path(posts_dir, drafts[selection])
#   )
# }
