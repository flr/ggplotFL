# plot.R - ggplot2-based plot methods for FLCore classes
# ggplotFL/R/plot.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT

# plot(FLQuant) {{{
#' ggplot versions of FLR class plot() methods
#'
#' New basic plot methods for some FLR classes are defined in ggplotFL.
#'
#' @aliases plot,FLQuants,missing-method
#' @docType methods
#' @rdname plot
#' @examples
#'
#'  # Plot a single FLQuant
#'  data(ple4)
#'  plot(catch.n(ple4))
#'
#'  # Plot an FLQuant with iters, shows quantiles
#'  flq <- rnorm(100, catch(ple4), 60000)
#'  plot(flq)
#'
#'  # Specify quantiles, default is c(0.10, 0.25, 0.50, 0.75, 0.90)
#'  plot(flq, probs=c(0.05, 0.40, 0.50, 0.60, 0.95))
#'  

setMethod("plot", signature(x="FLQuant", y="missing"),
	function(x, main="", xlab="", ylab="", na.rm=FALSE,
    probs=c(0.10, 0.25, 0.50, 0.75, 0.90), type=7) {

		
		# object w/ iters? compute quantiles
		if(dims(x)$iter > 1) {
		
			# check probs length is odd
			if(is.integer(length(probs)/2))
				stop("quantile probs can only be a vector of odd length")
		
			quans <- paste0(probs * 100, "%")
			mid <- ceiling(length(quans)/2)
			mquan <- quans[mid]

			# compute quantiles on FLQs, then convert to df
			df <- as.data.frame(quantile(x, probs=probs, na.rm=na.rm, type=type))
		
			# cast with quantiles in columns
			df <- dcast(df, as.formula(paste(paste(names(df)[1:5], collapse='+'),
				'iter', sep='~')), value.var="data")
			
		# otherwise, rename 'data' as 'q50'
		} else {
			df <- as.data.frame(x)
			names(df)[7] <- "50%"
			mquan <- "50%"
		}

		# dims on facet or groups
		dx <- dim(x)
		ldi <- names(x)[-c(2,6)][dx[-c(2,6)] > 1]

		# basic plot data vs. year
		p <- ggplot(data=df, aes_q(x=quote(year), y=as.name(mquan))) +
			# line + xlab + ylab + limits to include 0 +
			geom_line(colour="black") + xlab(xlab) + ylab(ylab) + expand_limits(y=0) +
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
				# extreme probs as dotted line
				geom_line(aes_q(x=quote(year), y = as.name(quans[1])),
					colour="red", alpha = .50, linetype=3) +
				geom_line(aes_q(x=quote(year), y = as.name(quans[length(quans)])),
					colour="red", alpha = .50, linetype=3)

			# all others as ribbons of changing alpha
			if(length(quans) > 3) {

				ids <- seq(2, mid-1)
				for(i in ids)
					p <- p + geom_ribbon(aes_q(x=quote(year), ymin = as.name(quans[i]),
						ymax = as.name(quans[length(quans)-i+1])), fill="red", alpha = probs[i])
			}
		}
		return(p)
	}
) # }}}

# plot(FLQuants) {{{
#' @aliases plot,FLQuants,missing-method
#' @rdname plot
#' @examples
#'
#'  # Plot an FLQuants created from ple4 FLStock
#'  data(ple4)
#'  plot(FLQuants(SSB=ssb(ple4), rec=rec(ple4)))
#'  

setMethod("plot", signature(x="FLQuants", y="missing"),
	function(x, main="", xlab="", ylab="", probs=c(0.10, 0.25, 0.50, 0.75, 0.90),
	         na.rm=FALSE, type=7) {
		
		# object w/ iters? compute quantiles
		if(any(unlist(lapply(x, function(y) dims(y)$iter)) > 1)) {

			# compute quantiles on FLQs, then convert to df
			df <- as.data.frame(lapply(x, quantile, probs=probs,na.rm=na.rm,type=type))
		
			# cast with quantiles in columns
			df <- dcast(df, as.formula(paste(paste(names(df)[c(1:5,8)], collapse='+'),
				'iter', sep='~')), value.var="data")
			
		# otherwise, rename 'data' as 'q50'
		} else {
			df <- as.data.frame(x)
			names(df)[7] <- "50%"
		}
		
		# plot data vs. year + facet on qname +
		p <- ggplot(data=df, aes(x=year, y=`50%`)) +
			facet_grid(qname~., scales="free") +
			# line + xlab + ylab + limits to include 0 +
			geom_line() + xlab(xlab) + ylab(ylab) + expand_limits(y=0) +
			# no legend
			theme(legend.title = element_blank())
		
		# object w/ iters?
		if(any(unlist(lapply(x, function(y) dims(y)$iter)) > 1)) {
			p <- p +
			# 75% quantile ribbon in red, alpha=0.25
			geom_ribbon(aes(x=year, ymin = `25%`, ymax = `75%`),
				fill="red", alpha = .25) +
			# 90% quantile ribbon in red, aplha=0.10
			geom_ribbon(aes(x=year, ymin = `10%`, ymax = `90%`),
				fill="red", alpha = .10) +
			# .. and dotted lines
			geom_line(aes(x=year, y = `10%`),
				colour="red", alpha = .50, linetype=3) +
			geom_line(aes(x=year, y = `90%`),
				colour="red", alpha = .50, linetype=3)
		}
		
		return(p)
	}
) # }}}

# plot(FLStock) {{{
#' @aliases plot,FLStock,missing-method
#' @rdname plot
#' @examples
#'
#'  # plot of an FLStock
#'  data(ple4)
#'  plot(ple4)
#'

setMethod("plot", signature(x="FLStock", y="missing"),
	function(x, main="", xlab="", ylab="", ...) {
		
		# extract info to plot: rec, ssb, catch and fbar
		fqs <- FLQuants(Rec=rec(x), SSB=ssb(x), Catch=catch(x), Harvest=fbar(x))

		p <- plot(fqs)

		return(p)
	}
) # }}}

# plot(FLStocks) {{{
#' @aliases plot,FLStocks,missing-method
#' @rdname plot
#' @examples
#'
#'  # plot for FLStocks
#'  data(ple4)
#'  pls <- FLStocks(runA=ple4, runB=qapply(ple4, function(x) x*1.10))
#'  plot(pls)
#'  

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
			geom_ribbon(aes(x=year, ymin = `10%`, ymax = `90%`, group=stock,
				colour=stock, fill=stock), alpha = .20, linetype = 0)
			# 90% quantile ribbon in red, aplha=0.10
		}
		return(p)
	}
) # }}}

# plot(FLStock, FLPar) {{{
#' @aliases plot,FLStock,FLPar-method
#' @rdname plot
#' @examples
#'
#'  # plot for FLStock, FLPar
#'  data(ple4)
#'  rps <- FLPar(Harvest=0.14, Catch=1.29e5, Rec=9.38e5, SSB=1.25e6)
#'  plot(ple4, rps)
#'  

setMethod("plot", signature(x="FLStock", y="FLPar"),
					function(x, y, ...) {

						p <- plot(x)

						rpa <- y@.Data[1,,1]
						rpa <- data.frame(data=rpa, qname=names(rpa), stringsAsFactors=FALSE)

						rpa$qname[rpa$qname == 'yield'] <- 'catch'

						qnames <- c("Rec", "SSB", "Catch", "Harvest")
						idx <- pmatch(tolower(as.character(rpa$qname)), tolower(qnames),
													duplicates.ok=TRUE)[1:4]
						rpa <- rpa[idx,]
						idx <- pmatch(tolower(as.character(rpa$qname)), tolower(qnames),
													duplicates.ok=TRUE)[1:4]
						rpa[,'qname'] <- qnames[idx]
						rownames(rpa) <- rpa[,'qname']

						p <- p + geom_hline(data=rpa, aes(yintercept=data), colour="blue", linetype=2)

						return(p)
					}
					) # }}}

# plot(FLSR) {{{
#' @aliases plot,FLSR,missing-method
#' @docType methods
#' @rdname plot
#' @examples
#'
#'  # plot for FLSR
#'  data(nsher)
#'  plot(nsher)
#'  

setMethod('plot', signature(x='FLSR', y='missing'),
	function(x, ...) {

	dat <- model.frame(FLQuants(SSB=ssb(x), Rec=rec(x), Residuals=residuals(x),
		RecHat=fitted(x)))

	uns <- units(x)
	unr <- ifelse(uns$rec == 'NA', 'Recruits', parse(text=paste0('Recruits (',
		sub('*', 'A', uns$rec, fixed=TRUE), ')')))
	uns <- ifelse(uns$ssb == 'NA', 'SSB', parse(text=paste0('SSB (', sub('*',
		'%*%', uns$ssb, fixed=TRUE), ')')))

	# SSB vs. REC
	p1 <- ggplot(data=dat, aes(x=SSB, y=Rec)) + geom_point() +
		geom_smooth(method='loess') + xlab(uns) +
		ylab(unr)

	# model fit line
	form <- as.list(model(x))[[3]]
	pars <- as(params(x), 'list')

	fmo <- function(x)
		eval(form, c(list(ssb=x), pars))
	
	p1 <- p1 + stat_function(fun=fmo,  colour='red', size=0.5)
	
	# P2
	p2 <- ggplot(data=dat, aes(x=year, y=Residuals)) + geom_point() + 	
		geom_smooth(method='loess') + xlab("Year")

	# P3
	p3 <- ggplot(data=data.frame(res1=dat$Residuals[-length(dat$Residuals)],
		res2=dat$Residuals[-1]), aes(x=res1, y=res2)) + geom_point() +
		xlab(expression(Residuals[t])) + ylab(expression(Residuals[t + 1])) +
	  geom_smooth(method='lm')

	# P4
	p4 <- ggplot(data=dat, aes(x=SSB, y=Residuals)) + geom_point() + 	
		geom_smooth(method='loess')

	# P5
	p5 <- ggplot(data=dat, aes(sample = Residuals)) + stat_qq(color="red",
		alpha=1) + geom_abline(aes(intercept = mean(Residuals),
		slope = sd(Residuals))) + xlab("Theoretical") + ylab("Sample")

	# P6
	p6 <- ggplot(data=dat, aes(x=RecHat, y=Residuals)) + geom_point() + 	
		geom_smooth(method='loess') + xlab(expression(hat(Recruits)))
	
	p <- gridExtra::arrangeGrob(p1, p2, p3, p4, p5, p6, ncol=2)
	
	return(p)
	}
) # }}}# plot(FLComps, list)
