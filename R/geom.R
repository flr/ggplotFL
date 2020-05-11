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
#' - `size`
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_line
#' @inheritParams ggplot2::geom_ribbon
#' @param probs Quantiles to compute and draw, defaults to c(0.10, 0.90).
#' @param alpha Transparency for quantile ribbon.
#' @examples
#' data(ple4)
#' flq <- rnorm(250, catch(ple4), 200000)
#' ggplot(flq, aes(x=date, y=data)) +
#'   geom_flquantiles(probs=c(0.25, 0.75), fill="red", alpha=0.25)
#' # Draw two quantiles with two calls to geom_flquantiles
#' ggplot(flq, aes(x=date, y=data)) +
#'   geom_flquantiles(probs=c(0.25, 0.75), alpha=0.25, fill="red") +
#'   geom_flquantiles(probs=c(0.10, 0.90), alpha=0.15, fill="red")
#' # Use it on an FLQuants, colouring by their name
#' flqs <- FLQuants(A=rnorm(250, catch(ple4), 200000),
#'   B=rnorm(250, stock(ple4), 200000))
#' ggplot(flqs, aes(x=date, y=data, group=qname)) +
#'   geom_flquantiles(probs=c(0.10, 0.90), aes(fill=qname), alpha=c(0.30))
#' # Or facet them
#' ggplot(flqs, aes(x=date, y=data)) +
#'   geom_flquantiles(probs=c(0.10, 0.90), fill="red", alpha=c(0.30)) +
#'   facet_grid(qname~.)

geom_flquantiles <- function(mapping = NULL, data = NULL, stat = "FLQuantiles",
  position = "identity", show.legend = NA, inherit.aes = TRUE, na.rm = FALSE,
  probs=c(0.10, 0.50, 0.90), alpha=0.5, ...) {

  list(

  # Quantile ribbon
  layer(
    geom = GeomRibbon,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params = list(probs=probs[c(1, length(probs))], alpha=alpha, ...)
  ),
       
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
    params = list(probs=probs[ceiling(length(probs)/2)], alpha=alpha, ...)
  )
  )
} # }}}

# stat_flquantiles {{{

StatFLQuantiles <- ggproto("StatFLQuantiles", Stat, 
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales, probs=c(0.10, 0.50, 0.90), na.rm=TRUE) {
    
    grid <- as.data.frame(do.call("rbind",
      as.list(tapply(data$y, data$x, quantile, na.rm=TRUE, probs=probs))))

    grid$x <- as.numeric(rownames(grid))
    grid$PANEL <- unique(data$PANEL)
    grid$group <- unique(data$group)
    grid$colour <- unique(data$colour)
    grid$fill <- unique(data$fill)

    # TODO Output to vary with stat_flquantiles$geom, not probs
    if(length(probs) == 1) {
      grid$y <- grid[,1]
    } else if(length(probs) == 2) {
      grid$ymin <- grid[,1]
      grid$ymax <- grid[,2]
    } else if(length(probs) == 3) {
      grid$ymin <- grid[,1]
      grid$y <- grid[,2]
      grid$ymax <- grid[,3]
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
#'  stat_flquantiles(probs=c(0.50), geom = "line", size=1.5,
#'    colour = "lightgreen") +
#'  stat_flquantiles(probs=c(0.50), geom = "line",
#'    colour = "darkgreen")

stat_flquantiles <- function(mapping = NULL, data = NULL, geom = "line",
  position = "identity", na.rm = TRUE, show.legend = NA, 
  inherit.aes = TRUE, ...) {

  layer(
    stat = StatFLQuantiles, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = TRUE, ...)
  )
} # }}}

# TODO geom_flpar(data=FLPar(s))

# geom_flpar {{{

#' @examples
#' plot(ssb(ple4)) + geom_flpar(data=FLPar(Blim=300000), aes(x=1960))
#' plot(ssb(ple4)) + geom_flpar(data=FLPar(Blim=300000), x=1960)
#' plot(ssb(ple4)) + geom_flpar(data=FLPar(Blim=300000), x=2015)
#' plot(ssb(ple4)) + geom_flpar(data=FLPar(Blim=300000, Bpa=230000), aes(x=1960))

geom_flpar <- function(mapping = NULL, data = NULL, ..., x, na.rm=FALSE) {
  
  # DATA
  data <- as.data.frame(data, drop=FALSE)

  data$yintercept <- data$data
  data$linetype <- letters[as.numeric(row.names(data)) + 1]
  
  data$y <- data$data * 0.95
  data$label <- data$params
  
  # MAPPINGS
  if(!missing(x)) {
    mapping <- aes(x=x)
  }
  mapping$y <- aes(y=y)$y
  mapping$label <- aes(label=params)$label

  list(

  # geom_hline
  layer(
    geom = GeomHline,
    mapping = aes(yintercept=yintercept, linetype=linetype),
    data = data,
    stat = StatIdentity,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(na.rm=na.rm, ...)

  ),

  # geom_text
  layer(
    geom = GeomText,
    mapping = mapping,
    data = data,
    stat = StatIdentity,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(na.rm=na.rm, hjust="outward")
  )
  )
} # }}}

