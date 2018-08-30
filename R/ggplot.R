# ggplot.R - Overloaded S4 ggplot methods for FLR classes
# ggplotFL/R/ggplot.R

# Copyright 2012-2017 FLR Team. Distributed under the GPL 2
# Maintainer: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu

# FLQuant {{{

#' ggplot method for various FLR classes
#'
#' The \code{\link[ggplot2]{ggplot}()} method has been conveniently overloaded for various
#' FLR classes. A call to \code{\link{as.data.frame}} takes place on \code{data}
#' before passing all arguments to the original \code{\link[ggplot2]{ggplot}} function.
#'
#' Please look at the relevant \code{as.data.frame} method for each class to
#' understand the naming conventions used in the resulting \code{data.frame}
#'
#' @param data An \code{FLQuant} object
#' @param mapping An aesthetic mapping, from a call to \code{\link[ggplot2]{aes}}
#' @param ... Other arguments to be passod on to \code{\link[ggplot2]{ggplot}}
#' @param environment Where to look for an undefined plot variable.
#' @rdname ggplot
#' @aliases ggplot,FLQuant-method
#' @docType methods
#' @examples
#'    dat <- rnorm(1, FLQuant(1, dim=c(5,10)), 0.5)
#'    ggplot(data=dat, aes(data, year)) + geom_point() 
#' @seealso \link[ggplot2]{ggplot}, \url{https://github.com/flr/ggplotFL/}

setMethod("ggplot", signature(data="FLQuant"),
  function(data, mapping=aes(), ..., environment=parent.frame()) {
    data <- as.data.frame(data, cohort=TRUE, timestep=TRUE, date=TRUE)
	return(ggplot(data, mapping, ...))
	}
) # }}}

# FLQuants {{{

# FLQuants

#' @rdname ggplot
#' @aliases ggplot,FLQuants-method
#' @examples
#'    data(ple4)
#'    dat <- FLQuants(catch=catch(ple4), ssb=ssb(ple4))
#'    ggplot(data=dat, aes(data, year)) + geom_point() + facet_wrap(~qname)

setMethod("ggplot", signature(data="FLQuants"),
  function(data, mapping=aes(), ..., environment=parent.frame()) {
    data <- as.data.frame(data, cohort=TRUE, timestep=TRUE, date=TRUE)
		ggplot(na.omit(data), mapping, ...)
	}) # }}}

# FLComp {{{
#' @rdname ggplot
#' @aliases ggplot,FLComp-method
setMethod("ggplot", signature(data="FLComp"),
  function(data, mapping=aes(), ..., environment=parent.frame()) {
    data <- as.data.frame(data, cohort=TRUE)
    ggplot(data, mapping, ...)
	}) # }}}

# FLComps {{{
#' @rdname ggplot
#' @aliases ggplot,FLComps-method
setMethod("ggplot", signature(data="FLComps"),
  function(data, mapping=aes(), ..., environment=parent.frame()) {
    data <- as.data.frame(data, date=TRUE)
    try(data$cohort <- data$year - data$age)
    ggplot(data, mapping, ...)
	}) # }}}
