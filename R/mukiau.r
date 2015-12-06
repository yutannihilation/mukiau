#' mukiau
#'
#' @param spell text to overlay
#' @param wrap_width width of paragraph
#' @param force_wrap if TRUE, break lines with strictly fixed length of characters
#' @param ... text style passed to \code{ggplot2::annotate()}
#' @examples
#' mukiau()
#'
#' # wrap
#' mukiau(wrap_width = 10)
#'
#' # break words
#' mukiau(wrap_width = 10, force_wrap = TRUE)
#'
#' mukiau("\x82\xc8\x82\xba\x82\xa0\x82\xc8\x82\xbd\x82\xcd\x43\x50\x39\x33\x32\x82\xc8\x82\xc7\x82\xf0\x8e\x67\x82\xa4\x82\xcc\x82\xc5\x82\xb7\x82\xa9\x81\x48", wrap_width = 10, force_wrap = TRUE)
#'
#' @export
mukiau <- function(spell = "Why are you using S-JIS?",
                   wrap_width = 25, force_wrap = FALSE,
                   size = 12, fontface = "bold", family = "sans",
                   colour = "white",
                   x = 0.1, y = 0.7, hjust = 0) {

  spell <- if(force_wrap) {
    len <- stringr::str_length(spell)
    idx <- seq(0, len, wrap_width)
    paste(stringr::str_sub(spell, start = idx, end = c(idx[-1] - 1, len)),
          collapse = "\n")
  } else {
    stringr::str_wrap(spell, width = wrap_width)
  }

  ggplot2::ggplot(data.frame(x=0,y=0),aes(x,y)) +

    ggplot2::annotation_raster(.yami, -Inf, Inf, -Inf, Inf) +

    ggplot2::annotate("text", x = x, y = y, hjust = hjust,
                      size = size, fontface = fontface, family = family,
                      colour = colour,
                      label = spell) +

    ggplot2::xlim(0,1) + ggplot2::ylim(0,1) +

    ggplot2::theme_void()
}

.yami <- jpeg::readJPEG(system.file("yami.jpg", package = "mukiau"))
