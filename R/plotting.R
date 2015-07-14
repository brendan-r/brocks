#' Add a footnote to a ggplot chart
#'
#' Currently, the text wrapping is based on the plot's default font size (which
#' is usually 12pts), meaning that using something smaller (as is the default in
#' this function), is likely to result in text which is slightly more narrow
#' than the available horizontal space. I don't mind it. Someone smarter than I
#' might be able to fix it with splitTextGrob!
#'
#' @param p A ggplot2 plot object
#' @param text The text used for the footnote
#' @param detect_font for ggplot2 objects, should the function attempt to
#'   extract the default font used?
#' @param wrap Should line breaks be added to the text to (attempt) to prevent
#'   it overflowing?
#' @param newlines Should a new line be appended to the start and end of the
#'   string, for spacing?
#' @param fontface Passed to \code{\link{gpar}}
#' @param fontsize Passed to \code{\link{gpar}}
#' @param col Passed to \code{\link{gpar}}
#' @param fontfamily Passed to \code{\link{gpar}}
#' @param ... Passed to \code{\link{gpar}}
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}, but heavily inspired
#'   by  Wendi (Alan) Wang: \url{http://statmodeling.com/best-way-to-add-a-footnote-to-a-plot-created-with-ggplot2.html}
#' @export
#' @examples
#' \dontrun{
#'   # Let's make a simple plot and add a footnote to it.
#'
#'   # Some footnote text, taken from the 'fortunes' package
#'   foot_note <- "R is Open Source and so you can modify it to emulate the bugs
#'   in other software: that is not one of the aims of its developers so please
#'   don't expect us to do so for you.\n  -- Brian D. Ripley (answering a request
#'   for a change to Excel-like non IEC 60559 standard conform rounding)
#'   \n     R-help (March 2009)"
#'
#'   # A ggplot2 chart
#'   library(ggplot2)
#'   d <- data.frame(mtcars)
#'   p <- ggplot(d, aes(cyl, mpg)) +
#'   geom_point() +
#'   theme(text = element_text(family = "Garamond"))
#'
#'   # Let's add our footnote!
#'   g <- gg_footnote(p, foot_note)
#'   g
#'
#'   # Note that the text is horizontally truncated. It will take up the full
#'   # width if we use the default ggplot2 font size of 12pts
#'   g <- gg_footnote(p, foot_note, fontsize = 12)
#'   g
#' }
gg_footnote <- function(p, text = "This is a footnote", detect_font = TRUE,
                        wrap = TRUE, newlines = TRUE, fontface = "italic",
                        fontsize = 8, col = "grey50", fontfamily = NULL, ...){

  if(detect_font)
    fontfamily <- p$theme$text$family

  if(newlines)
    text <- paste0("\n", text, "\n")

  if(wrap)
    text <- RGraphics::splitString(text)

  gridExtra::arrangeGrob(
    p,
    sub = grid::textGrob(
      text, x = 0.05, hjust = 0, vjust = 0.5,
      gp = grid::gpar(fontface = fontface, fontsize = fontsize, col = col,
                      fontfamily = fontfamily, ...))
  )
}


#' Add a watermark to a plot
#'
#' @param png_path A path to a valid png file to use for the watermark
#'
#' @export
smx_watermark <- function(png_path = "~/projects/smx_logos/greyscale_top.png"){
  # Insert something to write the rasterGrob to a variable within the package
  # and just use that, if it exists (otherwise, load from disk)

  img <- png::readPNG(png_path)
  g   <- grid::rasterGrob(img, interpolate=TRUE)
  ggplot2::annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
}

#' Brendan's ggplot2 theme
#'
#' @export
theme_br <- function(){
  # You should :: this up once you're happy with it
  requireNamespace("ggplot2")
  theme_bw() +
    theme(
      legend.position    = c(0.85, 0.15),
      text = element_text(family = "Ubuntu"),
      strip.text.x       = element_text(family = "Ubuntu", colour = 'white'),
      legend.background  = element_rect(fill=scales::alpha('white', 0))#,
      #     strip.background   =
      #       element_rect(
      #         colour = primary_cols[2], fill = "#424142"
      #       )
    )
}

#' ggplot2 categorical colour hues
#'
#' Generate the colours which \code{ggplot2} would, given a certain number of
#' categories.
#'
#' @param n The number of colours to return
#'
#' @author John Colby (taken from: \url{https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette})
#' @export
#' @examples
#' barplot(1:10, col = gg_colour_hue(10))
gg_colour_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
