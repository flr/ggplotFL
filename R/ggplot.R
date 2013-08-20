# ggplot.R - Overloaded S4 ggplot methods for FLR classes
# ggplotFL/R/ggplot.R

# Copyright 2012-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurie Kell, ICCAT & Iago Mosqueira, JRC
# Notes:

# FLQuant

#' ggplot method for various FLR classes
#'
#' The \\code{\\link{ggplot()}} method has been conviniently overloaded for various
#' FLR classes. A call to \\code{{\\link{{as.data.frame}}}} takes place on \\code{data}
#' before passing all arguments to the original \\code{ggplot} method.
#'
#' Please look at the relevant \\code{as.data.frame} method for each class to
#' understand the naming conventions used in the resulting \\code{data.frame}
#'
#' @param data An \\code{FLQuant} object
#' @rdname ggplot
#' @aliases ggplot,FLQuant-method
#' @docType methods
#' @examples
#'    dat <- rnorm(1, FLQuant(1, dim=c(5,10)), 0.5)
#'    ggplot(data=dat, aes(data, year)) + geom_point() 
#' @seealso \\link{ggplot}, \url{http://github.com/flr/ggplotFL/}

setMethod("ggplot", signature(data="FLQuant"),
  function(data, ...) {
    data <- as.data.frame(data, cohort=TRUE)
		ggplot(data, ...)
	})

# FLQuants
#' @rdname ggplot
#' @aliases ggplot,FLQuants-method
setMethod("ggplot", signature(data="FLQuants"),
  function(data, ...) {
    data <- as.data.frame(data, cohort=TRUE)
		ggplot(data, ...)
	})

# FLComp
#' @rdname ggplot
#' @aliases ggplot,FLComp-method
setMethod("ggplot", signature(data="FLComp"),
  function(data, ...) {
    data <- as.data.frame(data, cohort=TRUE)
    ggplot(data, ...)
	})

# FLComps
#' @rdname ggplot
#' @aliases ggplot,FLComps-method
setMethod("ggplot", signature(data="FLComps"),
  function(data, ...) {
    dat <- as.data.frame(data)

    try(dat$cohort <- dat$year - dat$age)
    ggplot(dat, ...)
	})
