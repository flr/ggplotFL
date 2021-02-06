# cohcorrplot.R - DESC
# /cohcorrplot.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# cohcorrplot {{{

#' cohcorrplot
#'
#' A correlation plot that show and quantifies correlation along
#' cohorts. Typically used on catch or survey abundances-at-age.
#'
#' The method prints a plot assembled as a combination of grid elements, not a
#' *gg* object.
#'
#' @param x An object with the abundance at age information. FLQuant or FLCohort.
#' @param ... Any extra arguments
#'
#' @rdname cohcorrplot-methods
#'
#' @author The FLR Team
#' @keywords methods
#' @md

setGeneric("cohcorrplot", function(x, ...)
  standardGeneric("cohcorrplot"))

#' @rdname cohcorrplot-methods
#' @examples
#' data(ple4)
#' cohcorrplot(stock.n(ple4))

setMethod("cohcorrplot", signature(x="FLQuant"),
  function(x) {
    cohcorrplot(FLCohort(x))
  })

#' @rdname cohcorrplot-methods
#' @examples
#' cohcorrplot(FLCohort(stock.n(ple4)))

setMethod("cohcorrplot", signature(x="FLCohort"),
  function(x) {

  # DATA.FRAME
  flc <- as.data.frame(x)

  # RESHAPE

  flc.wide <-  reshape(flc, direction="wide", timevar=names(flc)[1],
  idvar=names(flc)[2:6])
  
  names(flc.wide) <-  gsub("data.","", names(flc.wide))

  #

  pL <- vector("list", length=max(flc$age)^2)

  cMat <- matrix(NA, ncol = max(flc$age), nrow = max(flc$age))

  c2 <- numeric(max(flc$age)^2)

  za <- 1

  for (coh in min(flc$age):(max(flc$age))) {
    # 
  	if (coh > 1) {

		  for (corZ in (max(flc$age)-inc):1) {
  			d1 <- flc.wide[,c(ac(coh),ac(coh - corZ))]
	  		names(d1) <- c("x","y")
		  	c1 <- round(with(na.omit(d1),cor(x,y)),2)
			  c2[za] <- c1
  			pL[[za]] <- ggplot(data.frame(x = 1, y = 1, text = ac(c1)), aes(.data$x,.data$y)) +
	  		geom_text(aes(label = .data$text), size=8) +
		  		theme(axis.title=element_blank(),
        			axis.text=element_blank(),
        			axis.ticks=element_blank(),
        			panel.grid.major=element_blank(),
    				panel.grid.minor=element_blank())
			  za <- za + 1
		  }
	  }

  	pL[[za]] <- ggplot(data.frame(x = 1, y = 1, text = ac(coh)), aes(.data$x,.data$y)) +
	    geom_text(aes(label = .data$text), size=16) +
		  theme(axis.title=element_blank(),
        		axis.text=element_blank(),
        		axis.ticks=element_blank(),
        		panel.grid.major=element_blank(),
    			panel.grid.minor=element_blank())
  	c2[za] <- 1
	  za <- za + 1

	  if (coh < max(flc$age)) {
		
      for (inc in 1:(max(flc$age)-coh)) {
  			d1 <- flc.wide[,c(ac(coh),ac(coh + inc))]
	  		names(d1) <- c("x","y")
		  	cMat[coh, (coh+inc)] <- with(na.omit(d1),cor(x,y))
			  c2[za] <- with(na.omit(d1),cor(x,y))

        d1$za <- za
        pL[[za]] <- ggplot(data = na.omit(d1), aes(x=.data$x, y=.data$y)) +
		  		scale_fill_gradient2(low="blue", mid = "white", high="red",
					limits=c(-1,1), guide = FALSE) +
			  	geom_rect(aes(fill = c2[za]),xmin = -Inf,xmax = Inf,
	               ymin = -Inf,ymax = Inf,alpha = 0.8) +
				geom_point() +
				geom_smooth(method = "lm", fullrange = TRUE, col = 1) +
				theme(axis.title=element_blank(),
	        		axis.text=element_blank(),
	        		axis.ticks=element_blank())

			za <- za + 1
		}
	}
}

margin = theme(plot.margin = unit(c(0.01,0.01,0.01,0.01), "cm"))
pL <- lapply(pL, "+", margin)
suppressMessages(do.call("grid.arrange", c(pL, ncol =(length(unique(flc$age))))))
}
) # }}}

globalVariables(c("final", "y", "pred"))

# plotXval (FLIndices, list) {{{

#' Plot of FLIndices cross-validation by retrospective hindcast
#'
#' @param x An *FLIndices* object of the original observations.
#' @param y A list contaning *FLIndices* objects returned by *a4ahcxval*.
#' @param order Order in which retrospective runs are stored, defaults to"inverse".
#'
#' @return A ggplot object
#'
#' @examples
#' # SEE vignette

plotXval <- function(x, y="missing", order="inverse") {
  
  # SINGLE input
  if(missing(y) & is.list(x) & "data" %in% names(x)) {
    y <- x[!names(x) %in% "data"]
    x <- x[["data"]]
  }

  # CHECK names of y, drop 'data'
  if("data" %in% names(y))
    y <- y[!names(y) %in% "data"]

  # TODO CHECK years of y and x

  # SUBSET x, if needed, by names of y
  if(length(y[[1]]) < length(x))
    x <- x[names(y[[1]])]

  # SET first year of plot as 1st of retro - 3
  y0 <- dims(x[[1]])$maxyear - length(y) - 2
  py <- do.call(seq, as.list(rev(dims(x[[1]])$maxyear - c(0, length(y)  - 2))))

  # CONVERT inputs to data.tables

  # Original FLIndices
  dato <- dto(x, y0)

  # Hindcasted list(FLIndices)
  datp <- dtp(y, y0)

  # CALCULATE mase
  if(order == "inverse")
    idr <- 1
  else if (order == "ahead")
    idr <- length(y)
  else {
    idx <- an(names(y)) == dims(x[[1]])$maxyear
    if(sum(idx) == 1)
      idr <- which(idr)
    else
      stop("Could not identify reference run (to last year).")
  }

  # CALCULATE mase, exclude ref run
  imase <- mase(x, y[-idr], order=order)

  # GENERATE facet labels
  lbs <- unlist(lapply(seq(length(imase)), function(x)
    paste(names(imase)[x], "\nMASE:", format(imase[x], digits=3))))
  names(lbs) <- names(imase)
  llb <- names(y)
  llb[idr] <- paste(llb[idr], "(ref)")

  # LINES colors
  colors <- c(c("#0072B2", "#D55E00", "#009E73", "#56B4E9", "#E69F00", "#D55E00",
    "#009E73", "#56B4E9", "#E69F00")[seq(length(llb)) - 1], "#000000")
  
  # PLOT
  p <- ggplot(datp, aes(x=year, y=data, colour=final)) +

  # data lines and points
  geom_line(data=dato, linetype=2, colour="gray") +
  geom_point(data=dato, colour="black", size=3) +
  geom_point(data=dato, colour="white", size=2.6) +
  geom_point(data=dato[year %in% py,], aes(colour=ac(year-1)), size=2.6) +
  
  # retro lines and hindcast point
  geom_line() + 
  geom_point(data=datp[year==pred, ]) +

  # format
  facet_wrap(~index, scales="free_y", ncol=2, labeller=as_labeller(lbs)) +
  xlab("") + ylab("") +
  scale_color_manual("", labels=rev(llb), values=colors) +
  theme(legend.position="bottom")

  return(p)
} # }}}

# data.tables {{{

dtp <- function(flis, y0) {

  rbindlist(lapply(names(flis), function(i) {
    pred <- pmin(an(i) + 1, an(names(flis)[1]))
    data.table(cbind(as.data.frame(lapply(flis[[ac(i)]], function(x) {
      # COMPUTE Total abundance in biomass
      window(quantSums(index(x) * catch.wt(x)), start=y0, end=pred)
    }), drop=TRUE, qname="index"), final=i, pred=pred))}))
}

dto <- function(flis, y0) {

  data.table(as.data.frame(lapply(flis, function(x) {
    dmns <- dimnames(x)
    window(quantSums(index (x) * catch.wt(x)), start=y0)
  }), drop=TRUE, qname="index"))

}
# }}}
