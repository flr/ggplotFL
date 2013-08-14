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
		fqs <- FLQuants(Rec=rec(x), SSB=ssb(x), Catch=catch(x), Harvest=fbar(x))
		
		# object w/ iters? compute quantiles
		if(dims(x)$iter > 1) {

			# compute quantiles on FLQs, then convert to df
			df <- as.data.frame(lapply(fqs, quantile, c(0.10, 0.25, 0.50, 0.75, 0.90)))
		
			# cast with quantiles in columns
			df <- cast(df, age+year+unit+season+area+qname~iter, value="data")
			names(df)[7:11] <- paste("q", c(10, 25, 50, 75, 90), sep="")
			
		# otherwise, rename 'data' as 'q50'
		} else {
			df <- as.data.frame(fqs)
			names(df)[7] <- "q50"
		}
	
		# plot data vs. year + facet on qname +
		p <- ggplot(data=df, aes(x=year, y=q50)) + facet_grid(qname~., scales="free") +
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
		
		return(p)
	}
) # }}}

# plot(FLStocks) {{{
#' ggplot versions of FLR class plot() methods
#'
#' New basic plot for some FLR classes are defined in ggplotFL.
#'
#' @aliases plot,FLStocks,missing-method
#' @rdname plot
#' @examples
#'   data(ple4)
#'   fls <- FLStocks(runA=ple4, runB=ple4)
#'   plot(fls)

setMethod("plot", signature(x="FLStocks", y="missing"),
	function(x, main="", xlab="", ylab="", ...) {

		# extract slots
		out <- lapply(x, function(x) FLQuants(Rec=rec(x), SSB=ssb(x),
			Catch=catch(x), Harvest=fbar(x)))

		res <- lapply(out, FLQ2df)
		res <- melt(res, id.vars=c('age', 'year', 'unit', 'season', 'area', 'qname'))
		# TODO Review
		res <- cast(res, age+year+unit+season+area+qname+L1~variable)
	
		# plot q50 vs. year +
		p <- ggplot(data=res, aes(x=year, y=q50, colour=L1)) +
		# facet on qname +
			facet_grid(qname~., scales="free") +
			# line + xlab + ylab + limits to include 0 +
			geom_line() + xlab(xlab) + ylab(ylab) + expand_limits(y=0) +
			# no legend
			theme(legend.position="bottom", legend.title=element_blank())

		return(p)

	}
) # }}}
