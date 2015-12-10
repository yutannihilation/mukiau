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
#' mukiau("\\u306a\\u305c\\u3042\\u306a\\u305f\\u306fCP932\\u306a\\u3069\\u3092\\u4f7f\\u3046\\u306e\\u3067\\u3059\\u304b\\uff1f", wrap_width = 10, force_wrap = TRUE)
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
