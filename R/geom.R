# geom.R - DESC
# /geom.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# stat_flquantiles {{{

#' @examples
#' data(ple4)
#' ggplot(rnorm(250, catch(ple4), 200000), aes(x=year, y=data)) +
#'  stat_flquantiles(probs=c(0.10, 0.90), geom = "ribbon",
#'    fill="red", alpha=0.10) +
#'  stat_flquantiles(probs=c(0.10, 0.90), geom = "line",
#'    colour = "red", linetype=3) +
#'  stat_flquantiles(probs=c(0.25, 0.75), geom = "ribbon",
#'    fill="red", alpha=0.30) +
#'  stat_flquantiles(probs=c(0.25, 0.50, 0.75), geom = "line",
#'    colour = "black") 
#' # FLQuants
#' ggplot(FLQuants(A=rnorm(250, catch(ple4), 200000),
#'   B=rnorm(250, stock(ple4), 200000)),
#'   aes(x=year, y=data, group=qname, colour=qname, fill=qname)) +
#'  stat_flquantiles(probs=c(0.10, 0.90), geom = "ribbon",
#'    alpha=0.25, linetype=0) +
#'  stat_flquantiles(probs=c(0.25, 0.50, 0.75), geom = "line") 

StatFLQuantiles <- ggproto("StatFLQuantiles", Stat, 
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales, probs=c(0.10, 0.90)) {

    # ADD 50% 
    probs <- sort(unique(c(0.50, probs)))
    
    grid <- as.data.frame(do.call("rbind",
      tapply(data$y, list(data$x, data$group), quantile, probs=probs)))

    grid$PANEL <- unique(data$PANEL)
    grid$group <- unique(data$group)
    grid$colour <- unique(data$colour)
    grid$fill <- unique(data$fill)
    grid$x <- unique(data$x)

    grid$ymax <- grid[,1]
    grid$ymin <- grid[,3]
    grid$y <- grid[,2]

  grid
  }
)
    
stat_flquantiles <- function(mapping = NULL, data = NULL, geom = "line",
  position = "identity", na.rm = TRUE, show.legend = NA, 
  inherit.aes = TRUE, ...) {

  layer(
    stat = StatFLQuantiles, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
} # }}}

# geom_flquantiles {{{

geom_flquantiles <- function(mapping = NULL, data = NULL, stat = "FLQuantiles",
  position = "identity", show.legend = NA, inherit.aes = TRUE, na.rm = TRUE,
  ...) {
  
  list(

  # Quantile ribbon
  layer(
    geom = GeomRibbon,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
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
    params = list(...)
  )
  )
}

#' @examples
#' data(ple4)
#' ggplot(rnorm(250, catch(ple4), 200000), aes(x=year, y=data)) +
#'   geom_flquantiles(probs=c(0.25, 0.75), fill="cyan", alpha=0.40)
#' ggplot(rnorm(250, catch(ple4), 200000), aes(x=year, y=data)) +
#'   geom_flquantiles(probs=c(0.25, 0.75), alpha=0.25, fill="red") +
#'   geom_flquantiles(probs=c(0.10, 0.90), alpha=c(0.15), fill="red")
#' ggplot(rnorm(250, catch.n(ple4), 200000), aes(x=year, y=data)) +
#'   geom_flquantiles(probs=c(0.25, 0.75), alpha=0.25, fill="red") +
#'   geom_flquantiles(probs=c(0.10, 0.90), alpha=c(0.15), fill="red") +
#'   facet_wrap(~age)
#' ggplot(FLQuants(A=rnorm(250, catch(ple4), 200000),
#'    B=rnorm(250, catch(ple4), 200000)), aes(x=year, y=data)) +
#'   geom_flquantiles(probs=c(0.25, 0.75), fill="red", alpha=0.25) +
#'   geom_flquantiles(probs=c(0.10, 0.90), fill="red", alpha=0.10)
#' ggplot(FLQuants(A=rnorm(250, catch(ple4), 200000),
#'   B=rnorm(250, stock(ple4), 200000)), aes(x=year, y=data, group=qname)) +
#'  geom_flquantiles(probs=c(0.10, 0.90), aes(fill=qname), alpha=c(0.30))
