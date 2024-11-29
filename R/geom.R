# geom.R - DESC
# /geom.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# geom_flquantiles {{{

#' Sampling quantiles
#'
#' This `geom` calculates sampling quantiles and draws a ribbon for the quantile
#' range plus a line for the median (50\% quantile).
#'
#' As this `geom` outputs two layers, although based on different `geoms`,
#' interactions between common parameters need to be considered. The `fill` parameter
#' will only affect the quantile range `ribbon`, but `colour` will be passed to
#' both the `ribbon` and median `line` layers. The defaults are no lines on the quantiles
#' and "black" for the median line. The `alpha` value has been hard coded to 1
#' for the median line, so only affects the quantile `ribbon`. To change this,
#' call `stat_flquantiles` directly, as in the examples below.
#'
#' @name geom_flquantiles
#' @section Aesthetics:
#' `geom_flquantiles` understands the following aesthetics (required aesthetics are in bold):
#' - `*x*`
#' - `*y*`
#' - `alpha`
#' - `colour`
#' - `fill`
#' - `group`
#' - `linetype`
#' - `linewidth`
#' where some of them apply to the ribbons and some of them to the lines.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_line
#' @inheritParams ggplot2::geom_ribbon
#' @param probs Quantiles to compute and draw, defaults to c(0.10, 0.90).
#' @param alpha Transparency for quantile ribbon.
#' @examples
#' data(ple4)
#' flq <- rnorm(250, catch(ple4), 200000)
#' ggplot(flq, aes(x=date, y=data)) +
#'   geom_flquantiles(probs=c(0.25, 0.50, 0.75), fill="red", alpha=0.25)
#' # Draw two quantiles with two calls to geom_flquantiles
#' ggplot(flq, aes(x=date, y=data)) +
#'   geom_flquantiles(probs=c(0.25, 0.50, 0.75), alpha=0.25, fill="red") +
#'   geom_flquantiles(probs=c(0.10, 0.90), alpha=0.15, fill="red")
#' # Use it on an FLQuants, colouring by their name
#' flqs <- FLQuants(A=rnorm(250, catch(ple4), 200000),
#'   B=rnorm(250, stock(ple4), 200000))
#' ggplot(flqs, aes(x=date, y=data, colour=qname)) +
#'   geom_flquantiles(probs=c(0.10, 0.50, 0.90), aes(fill=qname), alpha=c(0.30))
#' # Or facet them
#' ggplot(flqs, aes(x=date, y=data)) +
#'   geom_flquantiles(probs=c(0.10, 0.50, 0.90), fill="red", alpha=c(0.30)) +
#'   facet_grid(qname~.)

geom_flquantiles <- function(mapping = NULL, data = NULL, stat = "FLQuantiles",
  position = "identity", show.legend = NA, inherit.aes = TRUE, na.rm = FALSE,
  probs=c(0.10, 0.50, 0.90), alpha=0.5, ...) {

  # Quantile ribbon, IF length(probs) > 1
  if(length(probs) > 1) {

  out <- list(

  layer(
    geom = GeomRibbon,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params = list(probs=probs[c(1, length(probs))], alpha=alpha, 
      linewidth=0, ...)))
  } else {
    out <- list()
  }

  # ADD line?

  if((length(probs) %% 2) != 0) {

  out <- c(out, list(
       
  # Median line
  layer(
    geom = GeomLine,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    check.param = FALSE,
    params = list(probs=probs[ceiling(length(probs)/2)], ...)
  )))
  }

  return(out)
} # }}}

# stat_flquantiles {{{

StatFLQuantiles <- ggproto("StatFLQuantiles", Stat, 
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales, probs=c(0.10, 0.50, 0.90), na.rm=TRUE) {
    
    grid <- as.data.frame(do.call("rbind",
      as.list(tapply(data$y, data$x, quantile, na.rm=TRUE, names=FALSE,
      probs=probs))))
    
    grid$x <- as.numeric(rownames(grid))
    grid$PANEL <- unique(data$PANEL)
    grid$group <- unique(data$group)
    grid$colour <- unique(data$colour)
    grid$fill <- unique(data$fill)

    # TODO Output to vary with stat_flquantiles$geom, not probs
    if(length(probs) == 1) {
      grid$y <- grid[, 1]
    } else if(length(probs) == 2) {
      grid$ymin <- grid[, 1]
      grid$y <- NA
      grid$ymax <- grid[, 2]
    } else if(length(probs) == 3) {
      grid$ymin <- grid[, 1]
      grid$y <- grid[, 2]
      grid$ymax <- grid[, 3]
    } else { 
      stop("stat_flquantiles can only return a maximum of 3 quantiles")
    }
  grid
  }
)
 
#' @section Computed variables:
#' \describe{
#'   \item{y}{quantile, if only one requested or central one when if three}
#'   \item{ymin}{lower quantile, if two or three requested}
#'   \item{ymax}{upper quantile, if two or three requested}
#' }
#' @rdname geom_flquantiles
#' @details
#' `stat_flquantiles` will return between one and three `y` values depending on
#' the number of quantiles requested. If two quantiles are to be calculated, it
#' will return the corresponding `ymin` and `ymax`, to be used with, for example,
#' `geom_ribbon`. If only one quantile is to be calculated, it will be returned
#' as `y`, to be used typically by `geom_line`. Finally, if three values are passed
#' in the `probs` argument, all of the above will be returned, in the right order.
#' @examples
#' # For greater control, call stat_flquantiles directly with a geom
#' ggplot(flq, aes(x=year, y=data)) +
#'  stat_flquantiles(probs=c(0.10, 0.90), geom = "ribbon",
#'    fill="yellowgreen", alpha=0.30) +
#'  stat_flquantiles(probs=c(0.01), geom = "line",
#'    colour = "green4", linetype=3) +
#'  stat_flquantiles(probs=c(0.99), geom = "line",
#'    colour = "green4", linetype=3) +
#'  stat_flquantiles(probs=c(0.25, 0.75), geom = "ribbon",
#'    fill="green4", alpha=0.30) +
#'  stat_flquantiles(probs=c(0.50), geom = "line", linewidth=1.5,
#'    colour = "lightgreen") +
#'  stat_flquantiles(probs=c(0.50), geom = "line",
#'    colour = "darkgreen")
#' ggplot(flq, aes(x=year, y=data)) +
#'  stat_flquantiles(probs=c(0.50), geom = "line",
#'    colour = "darkgreen")
#' plot(flq, probs=0.50)


stat_flquantiles <- function(mapping = NULL, data = NULL, geom = "line",
  position = "identity", na.rm = TRUE, show.legend = NA, 
  inherit.aes = TRUE, ...) {

  layer(
    stat = StatFLQuantiles, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = TRUE, ...)
  )
} # }}}

# geom_flpar {{{

#' Horizontal lines for FLPar objects
#'
#' This `geom` shows a horizontal line for each `param` in a `FLPar` object and
#' labels it using the `param` dimnames, by calling `geom_hline` and
#' `ggrepel::geom_label_repel`. Separate labels and lines can be specified for
#' different facets by providing the `data` argument with an object of class
#' `FLPars` in which each element is named as the values of the facetting
#' variable.
#'
#' @name geom_flpar
#' @section Aesthetics:
#' `geom_flpar` understands the following aesthetics (required aesthetics are in bold). Some aesthetics apply to only one of the two elements, in parenthesis:
#' - `*x*`
#' - `y`, defaults to 90% of params value (text).
#' - `yintercept`, defaults to params value (line).
#' - `label`, defaults to params names.
#' - `alpha`
#' - `colour`
#' - `linetype` (line)
#' - `linewidth` (line)
#' - `fill` (label)
#' - `angle` (label)
#' - `family` (label)
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_hline
#' @inheritParams ggplot2::geom_label_repel
#' @param x Position for params labels on the x axis
#' @examples
#' data(ple4)
#' plot(ssb(ple4)) + geom_flpar(data=FLPar(Blim=300000), x=1960)
#' plot(ssb(ple4)) + geom_flpar(data=FLPar(Blim=300000), x=2015)
#' plot(ssb(ple4)) + geom_flpar(data=FLPar(Blim=300000, Bpa=230000), x=1960)
#' # geom works for multiple facets, separate params using name-matching FLPars()
#' plot(ple4, metrics=list(SSB=ssb, F=fbar)) +
#'   geom_flpar(data=FLPars(SSB=FLPar(Blim=300000, Bpa=230000),
#'   F=FLPar(FMSY=0.21)), x=c(1964))
#' # x and y positions can be altered by param
#' plot(ple4, metrics=list(SSB=ssb, F=fbar)) +
#'   geom_flpar(data=FLPars(SSB=FLPar(Blim=300000, Bpa=230000),
#'   F=FLPar(FMSY=0.21)), x=c(2015, 2015, 1960), y=c(340000, 180000, 0.18))

geom_flpar <- function(mapping = NULL, data, ..., x, na.rm=FALSE) {

  args <- list(...)

  if(is.null(mapping))
    mapping <- aes(x=x)
 
  # DATA
  data <- as(data, "data.frame")
  data$yintercept <- data$data
  data$y <- data$data 
  data$linetype <- letters[as.numeric(row.names(data)) + 1]
  data$label <- data$params

  # MAPPINGS from data: y, yintercept, label, linetype
  mapping$y <- aes_string(y="y")$y
  mapping$label <- aes(label=params)$label
  mapping$yintercept <- aes_string(yintercept="yintercept")$yintercept
  mapping$linetype <- aes_string(linetype="linetype")$linetype

  # ACCEPTED aesthetics by geom
  ahline <- c("alpha", "colour", "linetype", "linewidth", "yintercept")
  atext <- c("x", "y", "label", "alpha", "angle", "colour", "family",
    "fontface", "group", "hjust", "lineheight", "size", "vjust", "fill")
 
  list(

  # geom_hline
  layer(
    data = data,
    mapping = mapping[names(mapping) %in% ahline],
    stat = StatIdentity,
    geom = GeomHline,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = c(list(na.rm=na.rm), args[names(args) %in% ahline])
  ),

  # geom_label
  layer(
    data = do.call(cbind, c(list(data),
      args[!names(args) %in% c(ahline, atext)])),
    mapping = mapping[names(mapping) %in% atext],
    stat = StatIdentity,
    geom = GeomLabelRepel,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = c(list(na.rm=na.rm, label.size = NA ), args[names(args) %in% atext])
  )
  )
} # }}}

# geom_worm {{{

#' A geom for adding worms to probability intervals from geom_flquantiles
#'
#' @section Aesthetics:
#' `geom_worm` understands the following aesthetics (required aesthetics are in bold):
#' - `colour`
#' - `linetype`
#' - `linewidth`
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_line
#' @param data Subset of data, select from full object using iter().
#'
#' @examples
#' data(ple4)
#' x <- rlnorm(200, log(catch(ple4)), 0.3)
#' plot(x) + geom_worm(data=iter(x, 1:4))
#'
#' x <- FLQuants(C=rlnorm(200, log(catch(ple4)), 0.3),
#'   F=rlnorm(200, fbar(ple4), 0.2))
#' plot(x) + geom_worm(data=iter(x, 1:4))

geom_worm <- function(data, mapping = aes(colour=iter), ...,
  stat="identity", position="identity", na.rm=FALSE) {
  
  data <- as.data.frame(data)

  layer(
    geom = GeomLine, mapping = mapping,  data = data, stat = stat, 
    position = position, inherit.aes = TRUE,
    params = list(na.rm = na.rm, ...)
  )
}
# }}}

# geom_fwd

# geom_vline(FLArray/FLQuant/FLPar)

# geom_hline(FLArray/FLQuant/FLPar)
