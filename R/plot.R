# plot.R - 
# ggplotFL/R/plot.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT
# $Id:  $

# plot(FLQuants) {{{
#' ggplot versions of FLR class plot() methods
#'
#' New basic plot for some FLR classes are defined in ggplotFL.
#'
#' @aliases plot,FLQuants,missing-method
#' @docType methods
#' @rdname plot
#' @examples
#'   # Plot anf FLQuants created from ple4 FLStock
#'   data(ple4)
#'   plot(FLQuants(SSB=ssb(ple4), rec=rec(ple4)))
#'

setMethod("plot", signature(x="FLQuants", y="missing"),
	function(x, main="", xlab="", ylab="", ...) {
		
		# object w/ iters? compute quantiles
		if(any(unlist(lapply(x, function(y) dims(y)$iter)) > 1)) {

			# compute quantiles on FLQs, then convert to df
			df <- as.data.frame(lapply(x, quantile, c(0.10, 0.25, 0.50, 0.75, 0.90)))
		
			# cast with quantiles in columns
			df <- dcast(df, as.formula(paste(paste(names(df)[c(1:5,8)], collapse='+'),
				'iter', sep='~')), value.var="data")
			
		# otherwise, rename 'data' as 'q50'
		} else {
			df <- as.data.frame(x)
			names(df)[7] <- "50%"
		}
		
		# plot data vs. year + facet on qname +
		p <- ggplot(data=df, aes(x=year, y=`50%`)) + facet_grid(qname~., scales="free") +
			# line + xlab + ylab + limits to include 0 +
			geom_line() + xlab(xlab) + ylab(ylab) + expand_limits(y=0) +
			# no legend
			theme(legend.title = element_blank())
		
		# object w/ iters?
		if(any(unlist(lapply(x, function(y) dims(y)$iter)) > 1)) {
			p <- p +
			# 75% quantile ribbon in red, alpha=0.25
			geom_ribbon(aes(x=year, ymin = `25%`, ymax = `75%`), fill="red", alpha = .25) +
			# 90% quantile ribbon in red, aplha=0.10
			geom_ribbon(aes(x=year, ymin = `10%`, ymax = `90%`),  fill="red", alpha = .10)
		}
		
		return(p)
	}
) # }}}

# plot(FLQuant) {{{
#' ggplot versions of FLR class plot() methods
#'
#' New basic plot for some FLR classes are defined in ggplotFL.
#'
#' @aliases plot,FLQuants,missing-method
#' @docType methods
#' @rdname plot
#' @examples
#'   data(ple4)
#'   plot(catch.n(ple4))

setMethod("plot", signature(x="FLQuant", y="missing"),
	function(x, main="", xlab="", ylab="", ...) {
		
		# object w/ iters? compute quantiles
		if(dims(x)$iter > 1) {

			# compute quantiles on FLQs, then convert to df
			df <- as.data.frame(quantile(x, c(0.10, 0.25, 0.50, 0.75, 0.90)))
		
			# cast with quantiles in columns
			df <- dcast(df, as.formula(paste(paste(names(df)[1:5], collapse='+'),
				'iter', sep='~')), value.var="data")
			
		# otherwise, rename 'data' as 'q50'
		} else {
			df <- as.data.frame(x)
			names(df)[7] <- "50%"
		}

		# dims on facet or groups
		dx <- dim(x)
		ldi <- names(x)[-c(2,6)][dx[-c(2,6)] > 1]

		# basic plot data vs. year
		p <- ggplot(data=df, aes(x=year, y=`50%`)) +
			# line + xlab + ylab + limits to include 0 +
			geom_line() + xlab(xlab) + ylab(ylab) + expand_limits(y=0) +
			# no legend
			theme(legend.title = element_blank())

		# build formula
		if(length(ldi) == 1) {
			p <- p + facet_grid(as.formula(paste0(ldi, "~.")), scales="free", 
				labeller=label_both)
		}
		else if (length(ldi) > 1) {
			p <- p + facet_grid(as.formula(paste0(ldi[1], "~", paste(ldi[-1], sep= "+"))),
				scales="free", labeller=label_both)
		}

		# object w/ iters?
		if(dims(x)$iter > 1) {
			p <- p +
			# 75% quantile ribbon in red, alpha=0.25
			geom_ribbon(aes(x=year, ymin = `25%`, ymax = `75%`), fill="red", alpha = .25) +
			# 90% quantile ribbon in red, aplha=0.10
			geom_ribbon(aes(x=year, ymin = `10%`, ymax = `90%`),  fill="red", alpha = .10)
		}
		
		return(p)
	}
) # }}}

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

		p <- plot(fqs)

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
		
		cos <- c('red', 'blue')
		
		# extract slots by stock
		fqs <- lapply(x, function(y) FLQuants(Rec=rec(y), SSB=ssb(y),
			Catch=catch(y), Harvest=fbar(y)))

		# get median & 85% quantiles if iters
		its <- unlist(lapply(x, function(x) dims(x)$iter))
		if(any(its > 1))
		{
			# quantiles
			fqs <- lapply(fqs, function(y) as.data.frame(lapply(y, quantile,
				c(0.10, 0.50, 0.90))))
		} else {
			fqs <- lapply(fqs, as.data.frame)
			fqs <- lapply(fqs, function(x) {x$iter <- "50%"; return(x)})
		}

		# stock names
		stk <- rep.int(names(fqs), unlist(lapply(fqs, nrow)))
		# rbind dfs
		fqs <- do.call(rbind, fqs)
		rownames(fqs) <- NULL
		# add stock names
		fqs <- transform(fqs, stock=stk)

		# cast with quantiles in columns
		df <- dcast(fqs, age+year+unit+season+area+qname+stock~iter, value.var="data")

		# plot data vs. year + facet on qname +
		p <- ggplot(data=df, aes(x=year, y=`50%`, group=stock)) +
			facet_grid(qname~., scales="free") +
			# line + xlab + ylab + limits to include 0 +
			geom_line(aes(colour=stock)) + xlab(xlab) + ylab(ylab) + expand_limits(y=0) +
			# no legend
			theme(legend.title = element_blank())
		
		# object w/ iters?
		if(any(unlist(lapply(x, function(y) dims(y)$iter)) > 1)) {
				p <- p +
			# 75% quantile ribbon in red, alpha=0.25
			geom_ribbon(aes(x=year, ymin = `10%`, ymax = `90%`, group=stock),
			fill='red', alpha = .10)
			# 90% quantile ribbon in red, aplha=0.10
		}
		return(p)
	}
) # }}}

# plot(FLStock, FLPar) {{{
#' ggplot versions of FLR class plot() methods
#'
#' New basic plot for some FLR classes are defined in ggplotFL.
#'
#' @aliases plot,FLStock,FLPar-method
#' @docType methods
#' @rdname plot
#' @examples
#'   data(ple4)
#'   rps <- FLPar(Harvest=0.14, Catch=1.29e5, Rec=9.38e5, SSB=1.25e6)
#'   plot(ple4, rps)

setMethod("plot", signature(x="FLStock", y="FLPar"),
	function(x, y, ...) {

		p <- plot(x, ...)
		p <- plot(x)

		rpa <- as.data.frame(y)
		names(rpa)[1] <- 'qname'

		# BUG BUT Catch is Yield in plot(FLStock)
		qnames <- c("Rec", "SSB", "Catch", "Harvest")
		idx <- pmatch(as.character(rpa$qname), qnames, duplicates.ok=TRUE)
		rpa <- rpa[idx,c('qname', 'data')]
		
		p <- p + geom_hline(data=rpa, aes(yintercept=data), colour="blue", linetype=2)
		
		return(p)
	}
) # }}}

# plot(FLSR) {{{
#' @aliases plot,FLSR,missing-method
#' @docType methods
#' @rdname plot
#' @examples
#'   data(nsher)
#'   plot(nsher)

setMethod('plot', signature(x='FLSR', y='missing'),
	function(x, ...) {

	dat <- model.frame(FLQuants(SSB=ssb(x), Rec=rec(x), Residuals=residuals(x), 	 
		RecHat=fitted(x)))

	uns <- units(x)

	# SSB vs. REC
	p1 <- ggplot(data=dat, aes(x=SSB, y=Rec)) + geom_point() +
		geom_smooth(method='loess') + xlab(paste0('SSB (', uns$ssb, ')')) +
		ylab(paste0('Recruits (', uns$rec, ')'))

	# model fit line
	form <- as.list(model(x))[[3]]
	pars <- as(params(x), 'list')

	fmo <- function(x)
		eval(form, c(list(ssb=x), pars))
	
	p1 <- p1 + geom_smooth(method="lm", formula=y~fmo(x), colour='red',
		size=0.5, se=FALSE)

	#
	p2 <- ggplot(data=dat, aes(x=year, y=Residuals)) + geom_point() + 	
		geom_smooth(method='loess')

	#
	p3 <- ggplot(data=data.frame(res1=dat$Residuals[-length(dat$Residuals)],
		res2=dat$Residuals[-1]), aes(x=res1, y=res2)) + geom_point()

	#
	p4 <- ggplot(data=dat, aes(x=SSB, y=Residuals)) + geom_point() + 	
		geom_smooth(method='loess')

	#
	p5 <- ggplot(data=dat, aes(sample = Residuals)) + stat_qq(color="red",
		alpha=1) + geom_abline(aes(intercept = mean(Residuals), slope = sd(Residuals)))

	#
	p6 <- ggplot(data=dat, aes(x=RecHat, y=Residuals)) + geom_point() + 	
		geom_smooth(method='loess')
	
	p <- gridExtra::arrangeGrob(p1, p2, p3, p4, p5, p6, ncol=2)
	
	return(p)
	}
) # }}}

# plot(FLComps, list)
