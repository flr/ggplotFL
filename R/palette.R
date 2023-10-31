# palette.R - DESC
# ggplotFL/R/palette.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# flpalette {{{

#' A high-contrast palette to use in ggplotFL
#' 
#' Plot methods defined in the ggplotFL package make use by default of a palette
#' with a high contrast, useful to separate time series or categories. The palette
#' consist of seven colours: red, blue, green, violet, orange, yellow and brown.
#' This palette can be inspected at the [colorbrewe2.org site](http://colorbrewer2.org/?type=qualitative&scheme=Set1&n=7#type=qualitative&scheme=Set1&n=7).
#'
#' The palette is accessible as a named vector, *flpalette*. Two functions are
#' also available to manipualte the palette. One to extract a subset of the
#' palette, *flpalette_colours*, and another to create a gradation of colours
#' between two or more of the palette colours, *flpalette_grads*.
#'
#' @param ... Elements to subset from palette, by name or position.
#' @param palette Palette subset to create a gradual scale from, defaults to *flpalette*.
#' @param reverse Should the palette be reversed, FALSE.
#' @param discrete Is the palette to be applied to a discrete variable, TRUE.
#' @param n Number of colours or individual colours to return, or number of colours to interpolate.
#'
#' @return A named vector of colors and HEX codes, or a function to obtain a gradient of colors fo a given length.
#'
#' @name flpalette
#' @rdname flpalette
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords color
#' @examples
#' # CHECK flpalette
#'  flpalette
#'  scales::show_col(flpalette)

flpalette <- c("#cc79a7", "#009e73", "#e69f00", "#56b4e9",
  "#999999", "#f0e442", "#0072b2", "#d55e00")


#' @rdname flpalette
#' @examples
#' flpalette_colours()
#' flpalette_colours(5)
#' flpalette_colours(2:3)

flpalette_colours <- function(n=length(flpalette)) {

  return(unname(flpalette[seq(n)]))
}

#' @rdname flpalette
#' @examples
#' flpalette_grads(flpalette_colours(3))(20)
flpalette_grads <- function(palette = flpalette, reverse = FALSE,
  ...) {

  if (reverse)
    palette <- rev(palette)

  return(colorRampPalette(palette, ...))
}
# }}}

# scale_fill/colour_flr {{{

#' High contrast discrete and continuous palettes
#' 
#' Discrete and continuous versions of the standard ggplotFL palette.
#' See \link{flpalette} fo further details.
#'
#' @param palette Palette subset to create a gradual scale from, defaults to *flpalette*.
#' @param discrete Is the palette to be applied to a discrete variable, TRUE.
#' @param reverse Should the palette be reversed, FALSE.
#' @param ... Other arguments to be passed to the scale functions
#'
#' @return A function.
#'
#' @name scale_fill_flr
#' @rdname scale_fill_flr
#'
#' @author The FLR Team
#' @seealso \link{flpalette}
#' @keywords color

scale_fill_flr <- function(palette = flpalette, discrete = TRUE, reverse = FALSE, ...) {

  if (discrete) {
    discrete_scale("fill", "flpalette", palette = flpalette_colours, ...)
  } else {
    pal <- flpalette_grads(palette = palette, reverse = reverse)
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' @rdname flpalette

scale_colour_flr <- function(palette = flpalette, discrete = TRUE, reverse = FALSE, ...) {

  if (discrete) {
    discrete_scale("colour", "flpalette", palette = flpalette_colours, ...)
  } else {
    pal <- flpalette_grads(palette = palette, reverse = reverse)
    scale_colour_gradientn(colours = pal(256), ...)
  }
}

# }}}
