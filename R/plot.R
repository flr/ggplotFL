# plot.R - ggplot2-based plot methods for FLCore classes
# ggplotFL/R/plot.R

# Copyright 2012-2018 FLR Team. Distributed under the GPL 2
# Maintainer: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu

globalVariables("density")

# plot(FLQuant) {{{

#' ggplot versions of FLR class plot() methods
#'
#' New basic plot methods for some FLR classes are defined in ggplotFL.
#'
#' The coertion to *data.frame* that is carried out in the plot methods sets
#' the argument `date=TRUE`. This generates a new column of class `POSIXct` for
#' the first day of the first month of each season. If the `season` dimension of
#' the object being plotted is of length greater than one, `date` will be used
#' as variable on the x axis of the plot. Otherwise, it will be `year`. Keep this
#' in mind when adding extra elements to the plot (see examples below).
#' 
#' A similar mechanism is used for the *y* axis, depending on the length of the
#' `iter` dimension. For objects with no *iters*, a single line is plotted for
#' each *FLQuant*, and the *y* axis is mapped to the `data` column of the
#' *data.frame*. For objects with iterations, i.e. with length greater than 1 on
#' the `iter` dimension, the default plots show the quantiles of the distribution
#' and the *y* axis is mapped to the middle quantile, by default `50%`. See the
#' examples below on how to refer to these variables when adding elements to the
#' plot.
#'
#' @param x FLR object to plot
#' @param y FLR object to plot
#' @param na.rm Should NAs be deleted in quantile calculations?, defaults to TRUE.
#' @param probs Quantiles to be plotted if object has iters, defaults to c(0.10, 0.33, 0.50, 0.66, 0.90).
#' @param ... Other arguments to be passed to the corresponding ggplot call.
#' @param metrics FlQuants computed from complex objects (e.g. FLStock)
#' @param iter Individual iterations to show as worm plots over the quantiles.
#' @param worm Individual iterations to show as worm plots over the quantiles.
#'
#' @aliases plot,FLQuant,missing-method
#' @seealso \code{\link{ISOdate}} \code{\link{ggplot}} 
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
#'  # Specify quantiles, default is c(0.10, 0.33, 0.50, 0.66, 0.90)
#'  plot(flq, probs=c(0.05, 0.25, 0.50, 0.75, 0.95))
#'
#'  # Adding extra elements to an FLQuant plot, with seasons
#'  flq <- FLQuant(runif(200), dim=c(1,15,1,4))
#'  plot(flq) + geom_point(aes(x=date, y=data, colour=season))
#'
#' # or without them
#'  flq <- FLQuant(runif(200), dim=c(1,15))
#'  plot(flq) + geom_point(aes(x=year, y=data))
#'
#' # For an object with iter
#'  flq <- rlnorm(100, flq, 0.4)
#'  plot(flq) + geom_point(aes(x=year, y=data))
#'
#' # To plot(FLQuant) as in previous versions of ggplotFL
#' plot(rnorm(300, catch(ple4), catch(ple4)/2), probs=c(0.10, 0.5, 0.90)) +
#'   geom_flquantiles(probs=c(0.01), linetype=3, colour="red", alpha=0.1) +
#'   geom_flquantiles(probs=c(0.99), linetype=3, colour="red", alpha=0.1)

setMethod("plot", signature(x="FLQuant", y="missing"),
	function(x, probs=c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm=FALSE, ...) {

    # GET base plot from plot(FLQuants)
    p <- plot(FLQuants(x), probs=probs, na.rm=na.rm, ...)
    
    # PARSE dimensions > 1 for new facets
    ldi <- c(names(x)[-c(2,3,4,6)][dim(x)[-c(2,3,4,6)] > 1])

    # PLOT(FLQuants(x)) & reset facets
		if(length(ldi) == 0) {
      p <- p + theme(strip.text.y = element_blank())
    }
		else if(length(ldi) == 1) {
			p <- p + facet_grid(as.formula(paste0(ldi, "~.")), scales="free",
        labeller=label_both)
		}
		else if (length(ldi) > 1) {
			p <- p + facet_grid(as.formula(paste0(ldi[1], "~", paste(ldi[-1],
        collapse= "+"))), scales="free", labeller=label_both)
		}

    return(p)
  })
# }}}

# plot(FLQuant, FLQuant) {{{

#' @rdname plot
#' @examples
#' # plot(FLQuant, FLQuant, ...) to place in one facet
#' plot(catch(ple4), landings(ple4))
#' # Add legend by hand
#' plot(rnorm(200, landings(ple4), 8000), discards(ple4)) +
#'   scale_colour_discrete(name="Yield (t)", labels=c("Landings", "Discards")) +
#'   theme(legend.position="bottom")

setMethod("plot", signature(x="FLQuant", y="FLQuant"),
  function(x, y, ..., probs=c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm=FALSE, iter=NULL) {

    # ASSEMBLE FLQuants
    fqs <- FLQuants(c(list(x, y), list(...)))

    # PLOT as 
    ggplot(fqs, aes(x=year, y=data, fill=qname, colour=qname)) +
      geom_flquantiles(alpha=0.3, probs=probs, na.rm=na.rm) +
      theme(legend.position="none") +
      xlab("") + ylab("")
  }
)
# }}}

# plot(FLQuants) {{{

#' @aliases plot,FLQuants,missing-method
#' @rdname plot
#' @examples
#'  # Plot an FLQuants created from ple4 FLStock
#'  data(ple4)
#'  plot(FLQuants(SSB=ssb(ple4), rec=rec(ple4)))
#'  plot(FLQuants(SSB=ssb(ple4), rec=rec(ple4)), probs = NULL)

setMethod("plot", signature(x="FLQuants", y="missing"),
	function(x, probs=c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm=FALSE, worm=iter,
    iter=NULL) {

    # SWITCH off ribbons
    if(is.null(probs)) {
      probs <- rep(0, 3)
      if(missing(worm))
        worm <- TRUE
    } else if(length(probs) == 1) {
      probs <- rep(probs, 3)
    }
    
    # CHECK probs length is odd
		if(is.integer(length(probs)/2))
		  stop("quantile probs can only be a vector of odd length")
    
    # FIND center of probs
    idx <- ceiling(length(probs)/2)

    # GET max dimensions
    mds <- apply(do.call(rbind, lapply(x, dim)), 2, max)

    # USE year or date for x axis
    xvar <- sym(ifelse(mds[4] == 1, "year", "date"))

    if(mds[6] == 1) {
      # NO ITERS? PLOT central line by unit
      p <- if(mds[3] == 1) {
        ggplot(x, aes(x=!!xvar, y=data)) +
          geom_line(na.rm=na.rm)
      } else {
        ggplot(x, aes(x=!!xvar, y=data, fill=unit, colour=unit)) +
          geom_line(na.rm=na.rm)
      }
    } else {
      # worm=TRUE? PLOT central ribbon and line by unit
      if(isTRUE(worm)) {
  		p <- if(mds[3] == 1) {
        ggplot(x, aes(x=!!xvar, y=data, fill=flpalette_colours(1))) +
          geom_line(aes(group=iter), alpha=0.2, linewidth=1, colour="#adadad") +
          geom_flquantiles(alpha=0.5,
            probs=probs[seq(idx - 1, idx + 1)], na.rm=na.rm)
      } else {
        ggplot(x, aes(x=!!xvar, y=data, fill=unit, colour=unit)) +
          geom_line(aes(group=iter), alpha=0.1, linewidth=1) +
          geom_flquantiles(alpha=0.5,
            probs=probs[seq(idx - 1, idx + 1)], na.rm=na.rm)
      }
      } else {
  		p <- if(mds[3] == 1) {
        ggplot(x, aes(x=!!xvar, y=data, fill=flpalette_colours(1))) +
          geom_flquantiles(alpha=0.5,
            probs=probs[seq(idx - 1, idx + 1)], na.rm=na.rm)
      } else {
        ggplot(x, aes(x=!!xvar, y=data, fill=unit, colour=unit)) +
          geom_flquantiles(alpha=0.5,
            probs=probs[seq(idx - 1, idx + 1)], na.rm=na.rm)
      }
      }
    }
    
    # PLOT other ribbons, if any
    if(length(probs) > 3 & mds[6] > 1) {
      geoms <- lapply(seq((length(probs)-3)/2), function(x) {
        geom_flquantiles(probs=probs[seq(idx - x - 1, idx + x + 1)],
          alpha=0.3)
      })
      p <- p + geoms
    }
     
    # SHOW NAs in x axis, only if no iters
		if(mds[6] == 1 & na.rm == FALSE) {
        if(any(is.na(p$data)))
          p <- p + geom_point(aes(y=0), cex=0.6, colour='darkgrey',
            data=subset(p$data, is.na(data)))
    }
    
    # PLOT iter worms
    if(is.numeric(worm) | is.character(worm)) {

      idata <- subset(p$data, iter %in% worm)
      
      # SELECT by position if numbers
      if(is.numeric(worm)) {

        # FIND iter dimnames for given positions
        ma <- data.table(p$data)[, .(iter=unique(iter)[worm]), by=qname]
 
        # SUBSET for combinations
        idata <- ma[data.table(p$data), , nomatch=0L, on=c("qname", "iter")]
        # RENAME iters so colours (factor) match
        idata[, iter:=as.character(rep(rep(seq(length(worm)),
          each=uniqueN(idata$year)), uniqueN(idata$qname)))]
      }
      p <- p + geom_line(data=idata, aes(x=!!xvar, y=data, colour=iter),
        na.rm=TRUE) 
    }

    # BUILD facet formula
    ldi <- c("qname", names(x[[1]])[-c(2, 3, 4, 6)][mds[-c(2, 3, 4, 6)] > 1])
    
    if(length(ldi) == 1) {
  			p <- p + facet_grid(as.formula(paste0(ldi, "~.")), scales="free", 
	  			labeller=label_flqs(x))
		}
		else if (length(ldi) > 1) {
			  p <- p + facet_grid(as.formula(paste0(ldi[1], "~", paste(ldi[-1],
          collapse= "+"))), scales="free", labeller=label_flqs(x))
		}

    # ASSEMBLE plot 
    p <- p + xlab("") + ylab("") + theme(legend.position="none")

    return(p)
  })
# }}}

# plot(FLQuants, FLPar) {{{

#' @aliases plot,FLQuants,FLPar-method
#' @rdname plot
#' @examples
#'  # plot for FLQuants, FLPar
#'  data(ple4)
#'  rps <- FLPar(F=0.14, Catch=1.29e5, Rec=9.38e5, SSB=1.8e5)
#'  fqs <- metrics(ple4)
#'  plot(fqs, rps)
#'  # Works also if reptsa are given for some panels
#'  rps <- FLPar(F=0.14, Catch=1.29e5, SSB=1.8e5)
#'  plot(fqs, rps)

# TODO REFORMAT
setMethod("plot", signature(x="FLQuants", y="FLPar"),
	function(x, y, ...) {
	
		p <- plot(x)

    # GET name variable mapped to y axis
    dat <- quo_name(p$mapping$y)
    
    # CREATE df with right name
		rpa <- data.frame(dat=c(y), qname=dimnames(y)$params, stringsAsFactors=FALSE)
    colnames(rpa)[1] <- dat

		# FIX mixmatch between refpts and FLStock slots naming
		if('yield' %in% rpa$qname)
			rpa$qname[rpa$qname == 'yield'] <- 'catch'

		p <- p + geom_hline(data=rpa, aes(yintercept=data), colour="blue", linetype=2)

		return(p)
	}
) # }}}

# plot (FLQuants, FLPars) {{{

#' @aliases plot,FLQuants,FLPars-method
#' @rdname plot
#' @examples
#'  # plot for FLQuants, FLPars
#'  data(ple4)
#'  rps <- FLPars(F=FLPar(Fmsy=0.14, Fpa=0.35), SSB=FLPar(SBmsy=1.8e5, SBlim=1.1e5))
#'  fqs <- metrics(ple4, list(SSB=ssb, F=fbar))
#'  plot(fqs, rps) + ylim(c(0, NA))

setMethod("plot", signature(x="FLQuants", y="FLPars"),
	function(x, y, ...) {
	
		p <- plot(x)

    # GET name variable mapped to y axis
    dnm <- quo_name(p$mapping$y)
    
    # CREATE df with right names
    dat <- do.call(rbind, c(mapply(function(i, j)
      cbind(as.data.frame(i, drop=TRUE), qname=j), y, names(y),
      SIMPLIFY=FALSE), make.row.names = FALSE))
    colnames(dat)[2] <- dnm
    # DEBUG factors to character
    dat[,"params"] <- as.character(dat[,"params"])

		# FIX common mixmatch between refpts and FLStock slots naming
		if('yield' %in% dat$qname)
			dat$qname[dat$qname == 'yield'] <- 'catch'

    # MERGE refpts with same value
    counts <- table(dat$data)
    dups <- counts[counts > 1]
    if(length(dups) > 0) {
      idx <- lapply(names(dups), function(x) which(dat$data == x))
      dat[unlist(lapply(idx, '[', 1)), "params"] <- 
        unlist(lapply(idx, function(x) paste(as.character(dat[x, "params"]),
          collapse=" - ")))
      # DROP merged params
      dat <- dat[-unlist(lapply(idx, '[', -1)),]
    }

    # SET y nudge up
    lim <- do.call(rbind, mapply(function(i, j) data.frame(max=max(i),
      min=min(i), qname=as.character(j)),  x, names(x), SIMPLIFY=FALSE))

    dat <- merge(dat, lim)

		p <- p + geom_hline(data=dat, aes(yintercept=data), linetype=2,
        linewidth=0.25) +
      geom_text(data=dat, aes(y=data + ((max-min) * 0.05), label=params),
        x=dims(x[[1]])$minyear - 1, size=3, hjust="inward")

		return(p)
	}
) # }}}

# plot(FLQuantPoint) {{{

#' @aliases plot,FLQuantPoint,missing-method
#' @rdname plot
#' @examples
#' # plot for FLQuantPoint
#' fqp <- FLQuantPoint(rlnorm(300, log(catch(ple4)), 0.20))
#' plot(fqp)

setMethod("plot", signature(x="FLQuantPoint", y="missing"),
	function(x, mean=TRUE, median=TRUE) {
    
    # BASE plot 
    p <- ggplot(x, aes(x=date))
   
    # ELEMENTS to add
    if(!all(is.na(p$data$median)) & mean)
       p <- p + geom_line(aes(y=median), linetype=1)

    if(!all(is.na(p$data$mean)) & median)
       p <- p + geom_line(aes(y=mean), linetype=2)

    if(!all(is.na(p$data[, c("lowq", "uppq")])))
      p <- p + geom_ribbon(aes(ymin=lowq, ymax=uppq), colour="gray",
        alpha=0.20, linetype=3)

    # PARSE dimensions > 1 for new facets
    ldi <- c(names(x)[-c(2,3,4,6)][dim(x)[-c(2,3,4,6)] > 1])

    # PLOT(FLQuants(x)) & reset facets
		if(length(ldi) == 0) {
      p <- p + theme(strip.text.y = element_blank())
    }
		else if(length(ldi) == 1) {
			p <- p + facet_grid(as.formula(paste0(ldi, "~.")), scales="free",
        labeller=label_both)
		}
		else if (length(ldi) > 1) {
			p <- p + facet_grid(as.formula(paste0(ldi[1], "~", paste(ldi[-1],
        collapse= "+"))), scales="free", labeller=label_both)
		}

    # ASSEMBLE plot 
    p <- p + xlab("") + ylab("")

    return(p)
  }
) # }}}

# plot(FLQuantPoint, FLQuant) {{{

#' @aliases plot,FLQuantPoint,FLQuant-method
#' @rdname plot
#' @examples
#' # plot for FLQuantPoint, FLQuant
#' plot(fqp, rlnorm(3, log(catch(ple4)), 0.20))

setMethod("plot", signature(x="FLQuantPoint", y="FLQuant"),
	function(x, y, na.rm=FALSE, ...) {
    if(dim(y)[6] > 1)
      plot(x, divide(y), ...)
    else
      plot(x, FLQuants(y=y), ...)
  }
) # }}}

# plot(FLQuantPoint, FLQuants) {{{

#' @aliases plot,FLQuantPoint,FLQuants-method
#' @rdname plot
#' @examples
#' # plot for FLQuantPoint, FLQuants
#' fqp <- FLQuantPoint(rlnorm(300, log(catch(ple4)), 0.20))
#' fqs <- divide(rlnorm(3, log(catch(ple4)), 0.20))
#' plot(fqp, fqs)

setMethod("plot", signature(x="FLQuantPoint", y="FLQuants"),
	function(x, y, na.rm=FALSE, mean=TRUE, median=TRUE, ...) {

    # PLOT FLQuantPoint
    p <- plot(x, mean=mean, median=median)

    # EXTRACT FLQuants
    dat <- as.data.frame(y, drop=TRUE, date=TRUE)

    # ADD FLQuants as lines
    p <- p + geom_flquantiles(data=dat, aes(x=date, y=data, colour=qname))

    # PARSE dimensions > 1 for new facets
    ldi <- c(names(x)[-c(2,3,4,6)][dim(x)[-c(2,3,4,6)] > 1])

    # PLOT(FLQuants(x)) & reset facets
		if(length(ldi) == 0) {
      p <- p + theme(strip.text.y = element_blank())
    }
		else if(length(ldi) == 1) {
			p <- p + facet_grid(as.formula(paste0(ldi, "~.")), scales="free",
        labeller=label_both)
		}
		else if (length(ldi) > 1) {
			p <- p + facet_grid(as.formula(paste0(ldi[1], "~", paste(ldi[-1],
        collapse= "+"))), scales="free", labeller=label_both)
		}

    # ASSEMBLE plot 
    p <- p + xlab("") + ylab("") + theme(legend.position="none")

    return(p)
  })
# }}}

# plot(FLPar, missing) {{{

#' @rdname plot
#' @examples
#' par <- FLPar(alpha=rnorm(200, 0.6, 0.2), beta=rlnorm(200, 0.8, 0.3))
#' plot(par)

setMethod("plot", signature(x="FLPar", y="missing"),
  function(x, names=NULL) {

    # EXTRACT units
    ups <- units(x)

  ggplot(as.data.frame(x), aes(x=data)) + 
    geom_density(alpha=.2, aes(fill=params)) +
    geom_histogram(aes(y=after_stat(density)), bins=20,
      colour="black", fill=NA) +
    facet_wrap(~params, scales="free", labeller=
    format_label_flqs(ups, dimnames(x)$params)) +
    xlab("") + ylab("") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_x_continuous(n.breaks = 7) +
    guides(fill="none")

  }
)
# }}}

# plot(FLStock) {{{

#' @aliases plot,FLStock,missing-method
#' @rdname plot
#' @examples
#' # plot of an FLStock
#'  data(ple4)
#'  plot(ple4)

setMethod("plot", signature(x="FLStock", y="missing"),
	function(x, metrics=list(Rec=rec, SSB=ssb, Catch=catch, F=fbar), na.rm=TRUE,
    ...) {

    # PLOT unfitted
    if(all(is.na(harvest(x))) & missing(metrics))
      metrics <- list(Catch=catch, Landings=landings, Discards=discards)
    
    metrics <- metrics(x, metrics=metrics)

    # HACK for F units
    if("F" %in% names(metrics))
      units(metrics$F) <- paste0(range(x, c("minfbar", "maxfbar")),
        collapse="-")
  
    # ADAPT for 2-sex model
    if("SSB" %in% names(metrics)) {

      if(all(dimnames(metrics$SSB)$unit %in% c("F", "M"))) {

        # FIND spawning season, if it exists
        #if(dim(metrics$SSB)[4] > 1) {
        #  metrics$SSB[is.na(metrics$SSB)] <- 0
        #}
      
        # DROP M ssb if missing
        metrics$SSB <- metrics$SSB[,,'F'] + metrics$SSB[,,'M']

        # SUM rec across units
        if("Rec" %in% names(metrics))
        metrics$Rec <- unitSums(metrics$Rec)
      }
    }

    # ADAPT for seasonal recruitment
#    if("Rec" %in% names(metrics)) {
#      if(dim(metrics$Rec)[4] > 1) {
#        metrics$Rec[metrics$Rec == 0] <- NA 
#      }
#    }

    p <- plot(metrics, na.rm=na.rm, ...) + ylim(c(0, NA))
  
    # ADD legend if 2 sexes  
    if("SSB" %in% names(metrics))
    if(all(dimnames(metrics$SSB)$unit %in% c("F", "M"))) {
      return(p +
        theme(legend.position="bottom", legend.key=element_blank()) +
        labs(color="Sex") +
        scale_color_manual(name="", 
          labels=c("Both", "F", "M"),
          values=flpalette_colours(3))
      )
    }
		return(p)
	}
) # }}}

# plot(FLStock, FLStock) {{{

#' @aliases plot,FLStock,FLStock-method
#' @rdname plot

setMethod("plot", signature(x="FLStock", y="FLStock"),
	function(x, y, metrics=list(Rec=rec, SSB=ssb, Catch=catch, F=fbar),
    probs=c(0.10, 0.33, 0.50, 0.66, 0.90), na.rm=TRUE, iter=NULL, ...) {

    args <- list(...)

    sts <- do.call("FLStocks", c(list(x, y), args))

    names(sts) <- unlist(lapply(sts, name))

    p <- plot(sts, probs=probs, na.rm=na.rm, iter=iter, metrics=metrics)

		return(p)
	}
) # }}}

# plot(FLStock, FLPar) {{{

#' @aliases plot,FLStock,FLPar-method
#' @rdname plot
#' @examples
#' # plot for FLStock, FLPar
#' data(ple4)
#' rps <- FLPar(F=0.14, Catch=1.29e5, Rec=9.38e5, SSB=1.8e5)
#' plot(ple4, rps)

setMethod("plot", signature(x="FLStock", y="FLPar"),
	function(x, y, metrics=list(Rec=rec, SSB=ssb, Catch=catch, F=fbar), ...) {
	
    p <- plot(metrics(x, metrics=metrics), y, ...)
    return(p)
	}
) # }}}

# plot(FLStocks) {{{

#' @aliases plot,FLStocks,missing-method
#' @rdname plot
#' @param metrics function returning an FLQuants for each FLStock
#' @param probs Quantiles to calculate along the iter dimension. A vector of length 5, for the lower outer, lower inner, central, upper inner and upper outer quantiles. Defaults to the 66 and 80 percent quantiles, plus median line.
#' @param alpha alpha values for the quantile ribbons, defaults to 0.10 and 0.40.
#' @examples
#' # plot for FLStocks
#' data(ple4)
#' pls <- FLStocks(runA=ple4, runB=qapply(ple4, function(x) x*1.10))
#' plot(pls)
#' # geom_flpar can be used draw refpts lines and labels
#' plot(pls, metrics=list(SSB=ssb, F=fbar)) +
#'   facet_grid(qname~stock, scales='free') +
#'   geom_flpar(data=FLPars(SSB=FLPar(Blim=300000, Bpa=230000),
#'   F=FLPar(FMSY=0.21)), x=c(1960), stock='runA', fill=alpha('white', 0.4))
  
setMethod("plot", signature(x="FLStocks", y="missing"),
	function(x, metrics=list(Rec=rec, SSB=ssb, Catch=catch, F=fbar),
    probs=c(0.10, 0.33, 0.50, 0.66, 0.90), alpha=c(0.10, 0.40), worm=iter,
    iter=NULL, ...) {
	
		# CHECK names not repeated
		dup <- duplicated(names(x))
		if(any(dup)) {
			names(x)[dup] <- paste(names(x)[dup], LETTERS[seq(sum(dup))], sep='_')
			# warning('Duplicated names in object, changed to differentiate')
		}
		
		# EXTRACT slots by stock
		fqs <- lapply(x, "metrics", metrics)

    # HACK for F units
    if("F" %in% names(fqs[[1]]))
      fqs <- lapply(fqs, function(fq) {
        units(fq$F) <- paste0(range(x[[1]],
          c("minfbar", "maxfbar")), collapse="-")
        return(fq)
    })

    # GET labels
    labeller <- label_flqs(fqs[[1]])

    # ASSEMBLE data
    data <- lapply(fqs, as.data.frame, date=TRUE, drop=FALSE)

    # SET stock names
		stk <- rep.int(names(fqs), unlist(lapply(data, nrow)))
		
    # RBIND dfs
		data <- do.call(rbind, data)
		rownames(data) <- NULL

    # USE year or date for x axis
    xvar <- sym(ifelse(length(unique(data$season)) == 1, "year", "date"))
		
    # ADD stock names
		data <- transform(data, stock=factor(stk, levels=names(x)))
    
    # PLOT using geom_flquantiles
    p <- ggplot(data, aes(x=!!xvar, y=data, fill=stock, colour=stock)) + 
      facet_grid(qname~., labeller=labeller, scales="free_y") +
      # outer quantile
      geom_flquantiles(probs=probs[c(1, 5)], alpha=alpha[1],
        colour="white") +
      # inner quantile
      geom_flquantiles(probs=probs[c(2, 4)], alpha=alpha[2],
        colour="white") +
      # median
      geom_flquantiles(probs=probs[3], alpha=1) +
      xlab("") + ylab("") +
			# SET limits to include 0
			expand_limits(y=0) +
      # SET legend with no title
      theme(legend.title = element_blank()) +
      # and only with lines and no title
      guides(fill = "none")
	
    # PLOT iter worms
    if(is.numeric(worm)) {
      idata <- p$data[p$data$iter %in% worm,]
      p <- p + geom_line(data=idata, aes(x=!!xvar, y=data, colour=iter))
    }

		return(p)
	}
) # }}}

# plot(FLStocks) {{{

#' @aliases plot,FLStocks,missing-method
#' @rdname plot
#' @param metrics function returning an FLQuants for each FLStock
#' @param probs Quantiles to calculate along the iter dimension. A vector of length 5, for the lower outer, lower inner, central, upper inner and upper outer quantiles. Defaults to the 66 and 80 percent quantiles, plus median line.
#' @param alpha alpha values for the quantile ribbons, defaults to 0.10 and 0.40.
#' @examples
#' # plot for FLStocks
#' data(ple4)
#' pls <- FLStocks(runA=ple4, runB=qapply(ple4, function(x) x*1.10))
#' plot(pls)
#' # geom_flpar can then be used draw refpts lines and labels
#' plot(pls, metrics=list(SSB=ssb, F=fbar)) +
#'   facet_grid(qname~stock, scales='free') +
#'   geom_flpar(data=FLPars(SSB=FLPar(Blim=300000, Bpa=230000),
#'   F=FLPar(FMSY=0.21)), x=c(1960), stock='runA', fill=alpha('white', 0.4))
  
setMethod("plot", signature(x="FLStocks", y="missing"),
	function(x, metrics=list(Rec=function(x) unitSums(rec(x)),
    SB=function(x) unitSums(ssb(x)), C=function(x) unitSums(catch(x)), 
    F=function(x) unitMeans(fbar(x))),
    probs=c(0.10, 0.33, 0.50, 0.66, 0.90), alpha=c(0.10, 0.40), worm=iter,
    iter=NULL, ...) {
	
		# EXTRACT slots by stock
		fqs <- lapply(x, "metrics", metrics=metrics)

    p <- plotListFLQuants(fqs, probs=probs, alpha=alpha, worm=worm, iter=iter)

		return(p)
	}
) # }}}

# plot(FLStocks, FLPar) {{{
# TODO Move to geom_flquantiles

#' @aliases plot,FLStocks,FLPar-method
#' @rdname plot

setMethod("plot", signature(x="FLStocks", y="FLPar"),
	function(x, y, na.rm=TRUE,
		metrics= function(x, y) FLQuants(SSB=ssb(x)/y[,'ssb',], F=fbar(x)/y[,'harvest',],
			Catch=catch(x))) {
		
		# check names not repeated
		dup <- duplicated(names(x))
		if(any(dup)) {
			names(x)[dup] <- paste(names(x)[dup], LETTERS[seq(sum(dup))], sep='_')
			warning('Duplicated names in object, changed to differentiate')
		}
		
		# extract slots by stock
		fqs <- lapply(x, metrics, y)

		# get median & 85% quantiles if iters
		its <- unlist(lapply(x, function(x) dims(x)$iter))
		if(any(its > 1))
		{
			# quantiles
			fqs <- lapply(fqs, function(y) as.data.frame(lapply(y, quantile,
				c(0.10, 0.50, 0.90), na.rm=TRUE)))
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

    # compute quantiles
    df <- reshape(fqs, timevar="iter", direction="wide",
      idvar=c(names(fqs)[1:5], "qname", "stock", "date"))
      
    names(df) <- gsub("data.", "", names(df))

		# plot data vs. date + facet on qname +
		p <- ggplot(data=na.omit(df), aes_string(x='date', y='`50%`', group='stock')) +
			facet_grid(qname~., scales="free") +
			# line + xlab + ylab +
			geom_line(aes_string(colour='stock'), na.rm=na.rm) + xlab("") + ylab("") +
			# limits to include 0 +  no legend
			expand_limits(y=0) + theme(legend.title = element_blank())
		
    # object w/ iters?
		if(any(unlist(lapply(x, function(y) dims(y)$iter)) > 1)) {
				p <- p +
			# 75% quantile ribbon in red, alpha=0.25
			geom_ribbon(aes_string(x='date', ymin = '`10%`', ymax = '`90%`', group='stock',
				colour='stock', fill='stock'), alpha = .20, linetype = 0, na.rm=na.rm)
			# 90% quantile ribbon in red, aplha=0.10
		}
		return(p)
	}
) # }}}

# plot(FLStock, FLStocks) {{{

#' @aliases plot,FLStock,FLStocks,missing-method
#' @rdname plot

setMethod("plot", signature(x="FLStock", y="FLStocks"),
	function(x, y, ...) {
    
    plot(FLStocks(c(x, y)), ...)
    
	}
) # }}}

# plot(FLSR) {{{

#' @aliases plot,FLSR,missing-method
#' @docType methods
#' @rdname plot
#' @examples
#' # plot for FLSR
#'  data(nsher)
#'  plot(nsher)

setMethod('plot', signature(x='FLSR', y='missing'),
	function(x, ...) {

	dat <- model.frame(FLQuants(SSB=ssb(x), Rec=rec(x), Residuals=residuals(x),
		RecHat=fitted(x)))

	uns <- units(x)
  
  unr <- ifelse(uns$rec == 'NA', 'Recruits', as.expression(paste0('Recruits (',
		sub('*', 'A', uns$rec, fixed=TRUE), ')')))
	uns <- ifelse(uns$ssb == 'NA', 'SSB', as.expression(paste0('SSB (', sub('*',
		'%*%', uns$ssb, fixed=TRUE), ')')))

	# SSB vs. REC
	p1 <- ggplot(data=na.omit(dat), aes_string(x='SSB', y='Rec')) + geom_point() +
		geom_smooth(formula= y ~ x, method='loess', span=3) + xlab(uns) + ylab(unr) +
		expand_limits(y=0) + expand_limits(x=0)

	# model fit line
	form <- as.list(model(x))[[3]]
	pars <- as(params(x), 'list')

	fmo <- function(x) {
		c(eval(form, c(list(ssb=FLQuant(x)), pars)))
  }
	
	p1 <- p1 + stat_function(fun=fmo,  colour='red', linewidth=0.5)
	
	# P2
	p2 <- ggplot(data=na.omit(dat), aes_string(x='year', y='Residuals')) +
    geom_point() + geom_smooth(formula= y ~ x, method='loess', span=3) + xlab("Year")

	# P3
	p3 <- ggplot(data=na.omit(data.frame(res1=dat$Residuals[-length(dat$Residuals)],
		res2=dat$Residuals[-1])), aes_string(x='res1', y='res2')) + geom_point() +
		xlab(expression(Residuals[t])) + ylab(expression(Residuals[t + 1])) +
	  geom_smooth(formula= y ~ x, method='lm')

	# P4
	p4 <- ggplot(data=na.omit(dat), aes_string(x='SSB', y='Residuals')) +
    geom_point() + geom_smooth(formula= y ~ x, method='loess', span=3)

	# P5
	p5 <- ggplot(data=na.omit(dat), aes_string(sample = 'Residuals')) +
    stat_qq(color="red", alpha=1) +
    geom_abline(aes_q(intercept = quote(mean(Residuals)),
		slope = quote(sd(Residuals)))) + xlab("Theoretical") + ylab("Sample")

	# P6
	p6 <- ggplot(data=na.omit(dat), aes_string(x='RecHat', y='Residuals')) +
    geom_point() + geom_smooth(formula= y ~ x, method='loess', span=3) +
    xlab(expression(hat(Recruits)))
	
  # ASSEMBLE grid and CONVERT to gg
  res <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol=2)

  return(ggdraw() + draw_grob(grid::grobTree(res)))
	
#	return(p)
	}
) # }}}

# plot(FLSRs) {{{

#' @aliases plot,FLSRs,missing-method
#' @docType methods
#' @rdname plot
#' @param legend_label function to create the legend labels
#' @param facets
#' @examples
#'  # plot for FLSRs
#'  data(nsher)
#'  srs <- FLSRs(sapply(c('segreg', 'bevholt'), function(x) {
#'    y <- nsher
#'    model(y) <- x
#'    return(fmle(y))
#'  }))
#'  plot(srs, facets=TRUE)
#'  plot(srs, legend_label=eqlabel)
#'  plot(srs, legend_label=modlabel)

setMethod("plot", signature(x="FLSRs"),
  function(x, legend_label=names(x), facets=FALSE, ...) {

    uns <- units(x[[1]])

    # DIFFERENT data?
 #   if(all(unlist(lapply(x[-1],
 #     function(y) isTRUE(all.equal(rec(y), rec(x[[1]]))))))) {
 #     dat <- cbind(sr=NA, model.frame(FLQuants(ssb=ssb(x[[1]]),
 #       rec=rec(x[[1]]))))
 #   } else {
      dat <- Reduce(rbind, Map(function(x, i)
        cbind(sr=i, model.frame(FLQuants(ssb=ssb(x), rec=rec(x)), drop=TRUE)),
        x, names(x)))
 #   }
    
    # EXTRACT models & pars
    mods <- lapply(x, 'model')
    pars <- lapply(x, 'params')
    ssbs <- seq(0, max(dat$ssb), length=100)

    # RESULTS
    res <- lapply(names(mods), function(i) {
      if(facets)
        ssbs <- seq(0, max(dat[dat$sr == i, 'ssb']), length=100)
      data.frame(sr=i, ssb=ssbs, rec=c(eval(as.list(mods[[i]])[[3]],
        c(list(ssb=ssbs), as(pars[[i]], 'list'))))
      )
    })

    #
    if(!is(legend_label, 'function')) {
      legend_label <- function(model, params)
        return(setNames(nm=names(model)))
    }

    res <- Reduce('rbind', res)

    # GET plot
    p <- ggplot(na.omit(res), aes(x=ssb, y=rec, colour=sr)) +
      geom_line(aes(group=sr, color=sr)) +
      geom_point(data=dat) + 
      xlab(as.expression(paste0("SSB (", sub('\\*', '%.%', uns$ssb), ")"))) +
      ylab(as.expression(paste0("Recruits (", sub('\\*', '%.%', uns$rec), ")"))) +
      scale_color_discrete(name="", breaks=names(x),
        labels=do.call(legend_label, list(model=mods, param=pars))) +
      theme(legend.position="bottom") +
      guides(color=guide_legend(nrow=length(mods), byrow=TRUE))
 
    return(p)
  }

) # }}}

# plot(FLBiol) {{{

#' @aliases plot,FLBiol,missing-method
#' @docType methods
#' @rdname plot
setMethod("plot", signature(x="FLBiol", y="missing"),
  function(x, metrics=list(Rec=function(x) n(x)[1,], B=tsb), ...) {

    flqs <- metrics(x, metrics)

    p <- plot(flqs) + ylim(c(0,NA))

    # TODO ADD SRR
    
    # TODO ADD mat, fec, m, wt by age

    return(p)
  }
) # }}}

# plot(FLBiols) {{{

#' @aliases plot,FLBiols,missing-method
#' @docType methods
#' @rdname plot
setMethod("plot", signature(x="FLBiols", y="missing"),
  function(x, metrics=list(Rec=function(x) n(x)[1,], B=tsb), ...) {

    fqs <- lapply(x, function(x) metrics(x, metrics))
    
    # GET labels
    labeller <- label_flqs(fqs[[1]])

    dfs <- lapply(fqs, as.data.frame, date=TRUE, units=TRUE)

    data <- do.call("rbind", c(mapply(`[<-`, dfs, "biol", value=names(fqs),
      SIMPLIFY=FALSE), list(make.row.names = FALSE)))
    
    # USE year or date for x axis
    xvar <- sym(ifelse(length(unique(data$season)) == 1, "year", "date"))
    
    # PLOT using geom_flquantiles
    p <- ggplot(data, aes(x=!!xvar, y=data, fill=.data$biol, colour=.data$biol)) + 
      facet_grid(qname~., labeller=labeller, scales="free_y") +
      geom_flquantiles() + xlab("") + ylab("") +
			# SET limits to include 0
			expand_limits(y=0) +
      # SET legend with no title
      theme(legend.title = element_blank()) +
      # and only with lines and no title
      guides(fill = "none")
		
		return(p)
  }
) # }}}

# plot(FLIndexBiomass) {{{

#' @aliases plot,FLIndexBiomass,missing-method
#' @docType methods
#' @rdname plot
setMethod("plot", signature(x="FLIndexBiomass", y="missing"),
  function(x, ...) {

    flqs <- FLQuants(Index=index(x))

    p <- plot(flqs, ...) + geom_smooth(formula=y ~ x, na.rm=TRUE,
      method="loess", se=FALSE)

    return(p)
  }
) # }}}

# plot(FLIndex) {{{
# TODO ADD index.var/se

#' @aliases plot,FLIndex,missing-method
#' @docType methods
#' @rdname plot
#' @examples
#'  # Plot a FLIndex object
#'  data(ple4.index)
#'  plot(ple4.index)

setMethod("plot", signature(x="FLIndex", y="missing"),
  function(x) {

    if(!units(index(x)) %in% c("NA", ""))
      ylab <- paste0("Abundance (", units(index(x)), ")")
    else
      ylab <- "Abundance"

    ggplot(index(x), aes(x=date, y=data)) +
      geom_line() +
      facet_wrap(~age, scales="free_y") +
      xlab("") + ylab(ylab)

  }
) # }}}

# plot(FLIndices) {{{

#' @aliases plot,FLIndices,missing-method
#' @docType methods
#' @rdname plot
#' @examples
#'  # Plot a FLIndices object
#'  data(ple4.indices)
#'  plot(ple4.indices)
#'  plot(ple4.indices) +
#'    geom_smooth(formula=y ~ x, se=FALSE, method="loess", linewidth=0.2)
setMethod("plot", signature(x="FLIndices", y="missing"),
  function(x) {

    fqs <- lapply(x, function(x)
    (index(x) %-% yearMeans(index(x)) %/% sqrt(yearVars(index(x)))))

    aes_(quote(mpg), quote(wt), col = quote(cyl))

    # CHOOSE xvar = date if seasons
    if(dim(fqs[[1]])[4] > 1)
      xvar <- quote(date)
    else
      xvar <- quote(year)
    
    p <- ggplot(fqs, aes_(x=xvar, y=quote(data), group=quote(qname),
      colour=quote(qname), fill=flpalette_colours(quote(qname)))) +
      geom_flquantiles(alpha=0.3) +
      ylab("Standardized relative abundance") + xlab("") +
      theme(legend.title=element_blank())
    
    if(all(unlist(lapply(x, is, "FLIndexBiomass")))) {
      return(p)
    } else {
      return(p + facet_wrap(~age, scales="free_y"))
    }
  }
) # }}}

# plotListFLQuants {{{
plotListFLQuants <- function(x, probs=c(0.10, 0.33, 0.50, 0.66, 0.90),
  alpha=c(0.10, 0.40), worm=iter, iter=NULL, fages=NULL) {
	
	# CHECK names not repeated
	dup <- duplicated(names(x))
	if(any(dup)) {
		names(x)[dup] <- paste(names(x)[dup], LETTERS[seq(sum(dup))], sep='_')
	}
		
  # HACK for F units
  if("F" %in% names(x[[1]]))
    x <- lapply(x, function(i) {
      units(i$F) <- paste0(fages, collapse="-")
      return(i)
  })

  # GET labels
  labeller <- label_flqs(x[[1]])

  # ASSEMBLE data
  data <- lapply(x, as.data.frame, date=TRUE, drop=FALSE)

  # SET stock names
	stk <- rep.int(names(x), unlist(lapply(data, nrow)))
		
  # RBIND dfs
	data <- do.call(rbind, data)
	rownames(data) <- NULL

  # USE year or date for x axis
  xvar <- sym(ifelse(length(unique(data$season)) == 1, "year", "date"))
		
  # ADD stock names
	data <- transform(data, stock=factor(stk, levels=names(x)))

  # PLOT using geom_flquantiles
  p <- ggplot(data, aes(x=!!xvar, y=data, fill=stock, colour=stock)) + 
    facet_grid(qname~., labeller=labeller, scales="free_y") +
    xlab("") + ylab("") +
		# SET limits to include 0
		expand_limits(y=0) +
    # SET legend with no title
    theme(legend.title = element_blank()) +
    # and only with lines and no title
    guides(fill = "none")

  if(length(probs) == 5) {
    # outer quantile
    p <- p + geom_flquantiles(probs=probs[c(1, 5)], alpha=alpha[1],
      colour="white") +
      # inner quantile
      geom_flquantiles(probs=probs[c(2, 4)], alpha=alpha[2],
      colour="white") +
      # median
      geom_flquantiles(probs=probs[3], alpha=1)
  } else if (length(probs) == 3) {
    p <- p + geom_flquantiles(probs=probs[c(1, 3)], alpha=alpha[2],
      colour="white") +
      geom_flquantiles(probs=probs[2], alpha=1)
  } else if (length(probs) == 1) {
    p <- p + geom_flquantiles(probs=probs, alpha=1)
  } else {
    stop("probs can only be of length 1, 3 or 5")
  }
	
  # PLOT iter worms
  if(is.numeric(worm)) {
    idata <- p$data[p$data$iter %in% worm,]
    p <- p + geom_line(data=idata, aes(x=!!xvar, y=data, colour=iter))
  }

	return(p)
}
# }}}
