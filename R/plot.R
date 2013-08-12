# plot.R - 
# ggplotFL/R/plot.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT
# $Id:  $

# plot(FLStock) {{{
#' ggplot versions of FLR class plot() methods
#'
#' New basic plot for some FLR classes are defined in ggplotFL.
#'
#' @aliases plot,FLStock,missing-method
#' @docType methods
#' @rdname plot
#' @examples
#'   data(ple4)
#'   plot(ple4)

setMethod("plot", signature(x="FLStock", y="missing"),
	function(x, main="", xlab="", ylab="", ...) {

		# extract info to plot: rec, ssb, catch and fbar
		out <- FLQuants(Rec=rec(x), SSB=ssb(x), Catch=catch(x), Harvest=fbar(x))

		# object w/ iters?
		if(dims(x)$iter > 1) {
			res <- FLQ2df(out, quantiles=c(0.10, 0.25, 0.50, 0.75, 0.90))
		} else {
			res <- FLQ2df(out)
		}
		
		# plot q50 vs. year + facet on qname +
		p <- ggplot(data=res, aes(x=year, y=q50)) + facet_grid(qname~., scales="free") +
			# line + xlab + ylab + limits to include 0 +
			geom_line() + xlab(xlab) + ylab(ylab) + expand_limits(y=0) +
			# no legend
			theme(legend.title = element_blank())
		
		# object w/ iters?
		if(dims(x)$iter > 1) {
			p <- p +
			# 75% quantile ribbon in red, alpha=0.25
			geom_ribbon(aes(x=year, ymin = q25, ymax = q75), fill="red", alpha = .25) +
			# 90% quantile ribbon in red, aplha=0.10
			geom_ribbon(aes(x=year, ymin = q10, ymax = q90),  fill="red", alpha = .10)
		}
		
		p
	}
) # }}}

# plot(FLStocks) {{{
#' ggplot versions of FLR class plot() methods
#'
#' New basic plot for some FLR classes are defined in ggplotFL.
#'
#' @aliases plots,FLStocks,missing-method
#' @rdname plot
#' @examples
#'   data(ple4)
#'   fls <- FLStocks(runA=ple4, runB=ple4)
#'   plot(fls)

setMethod("plot", signature(x="FLStocks", y="missing"),
	function(x, main="", xlab="", ylab="", ...) {

		return(TRUE)

	}
) # }}}
