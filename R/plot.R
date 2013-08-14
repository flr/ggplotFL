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
#'   pls <- FLStocks(runA=ple4, runB=qapply(ple4, function(x) x*1.10))
#'   plot(pls)

setMethod("plot", signature(x="FLStocks", y="missing"),
	function(x, main="", xlab="", ylab="", ...) {
		
		# extract slots by stock
		fqs <- lapply(x, function(y) FLQuants(Rec=rec(y), SSB=ssb(y),
			Catch=catch(y), Harvest=fbar(y)))

		# get median if iters
		fqs <- lapply(fqs, function(y) as.data.frame(lapply(y, quantile, 0.50)))
		# stock names
		stk <- rep.int(names(fqs), unlist(lapply(fqs, nrow)))
		# rbind dfs
		fqs <- do.call(rbind, fqs)
		rownames(fqs) <- NULL
		# add stock names
		fqs <- transform(fqs, stock=stk)

		# plot q50 vs. year +
		p <- ggplot(data=fqs, aes(x=year, y=data, colour=stock)) +
		# facet on qname +
			facet_grid(qname~., scales="free") +
			# line + xlab + ylab + limits to include 0 +
			geom_line() + xlab(xlab) + ylab(ylab) + expand_limits(y=0) +
			# no legend
			theme(legend.position="bottom", legend.title=element_blank())

		return(p)

	}
) # }}}
