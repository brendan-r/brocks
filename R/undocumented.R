





# You should put id.na/id_na in here!
# Import a version of gdata::format_bytes





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
