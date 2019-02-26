# palette.R - DESC
# ggplotFL/R/palette.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# flpalette {{{

#' A highly-contrasted palette to use in ggplotFL
#'
#' 
#' Plot methods defined in the ggplotFL package make use by default of a palette
#' with a high contrast, useful to separate time series or categories. The palette
#' consist of seven colours: red, blue, green, violet, orange, yellow and brown.
#' This palette can be inspected at the [colorbrewe2.org site](http://colorbrewer2.org/?type=qualitative&scheme=Set1&n=7#type=qualitative&scheme=Set1&n=7).
#'
#' The palette is accessible as a named vector, *flpalette*, with the corresponding
#' HEX codes. Two functions are also available to manipualte the palette. One to
#' extract a subset of the palette, *flpalette_colours*, and another to create a
#' gradation of colours between two or more of the palette colours, *flpalette_grads*.
#'
#' @param ... Elements to subset from palette, by name or position.
#' @param palette Palette subset to create a gradual scale from, defaults to *flpalette*.
#' @param reverse Should the palette be reversed, FALSE.
#'
#' @return A named vector of colors and HEX codes, or a function to obtain a gradient of colors fo a given length.
#'
#' @name flpalette
#' @rdname flpalette
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
#' @examples
#' # CHECK flpalette
#'  flpalette

flpalette <- c(red='#e41a1c', blue='#377eb8', green='#4daf4a', violet='#984ea3',
  orange='#ff7f00', yellow='#ffff33', brown='#a65628')

#' @rdname flpalette
#' @examples
#' flpalette_colours()
#' flpalette_colours(5)

flpalette_colours <- function(n=length(flpalette)) {

  unname(flpalette[seq(n)])
}

#' @rdname flpalette
#' @examples
#' flpalette_grads(flpalette_colours(3))(20)
flpalette_grads <- function(palette = flpalette, reverse = FALSE, ...) {

  if (reverse)
    palette <- rev(palette)

  return(colorRampPalette(palette, ...))
}
# }}}


scale_fill_flr <- function(palette = flpalette, discrete = TRUE, reverse = FALSE, ...) {

  if (discrete) {
    discrete_scale("fill", "flpalette", palette = flpalette_colours, ...)
  } else {
    pal <- flpalette_grads(palette = palette, reverse = reverse)
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

scale_colour_flr <- function(palette = flpalette, discrete = TRUE, reverse = FALSE, ...) {

  if (discrete) {
    discrete_scale("colour", "flpalette", palette = flpalette_colours, ...)
  } else {
    pal <- flpalette_grads(palette = palette, reverse = reverse)
    scale_colour_gradientn(colours = pal(256), ...)
  }
}
