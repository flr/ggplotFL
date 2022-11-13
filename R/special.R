# cohcorrplot.R - DESC
# /cohcorrplot.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

globalVariables(c("final", "y", "pred", "text", "pass", "p.value", "qname",
  "age", "lcl", "ucl", "outlier"))

# cohcorrplot {{{

#' cohcorrplot
#'
#' A correlation plot that show and quantifies correlation along
#' cohorts. Typically used on catch or survey abundances-at-age.
#'
#' The method prints a plot assembled as a combination of grid elements, but
#' reurns it as a *gg* object.
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
#' cohcorrplot(catch.n(ple4))

setMethod("cohcorrplot", signature(x="FLQuant"),
  function(x) {
    cohcorrplot(FLCohort(x))
  })

#' @rdname cohcorrplot-methods
#' @examples
#' cohcorrplot(FLCohort(stock.n(ple4)))

setMethod("cohcorrplot", signature(x="FLCohort"),
  function(x, diag_size=16, lower_size=6) {

  # DIMENSIONS
  
  ages <- dimnames(x)$age
  nag <- length(ages)

  # DIAGONAL elements, for age labels
  diag <- seq(nag) + nag * (seq(nag) - 1)

  # UPPER and LOWER triangle
  matr <- matrix(seq(nag ^ 2), nag, nag, byrow=TRUE)

  # INVERTED positions as matr is column first

  uppt <- which(lower.tri(matr))
  lowt <- unlist(lapply(seq(nag - 1), function(x) seq(x, nag - 1) * nag + x))

  # COMBINATIONS for correlations
  
  combs <- lapply(seq(nag - 1), function(x) seq(x + 1, nag))

  # PLOTS list
  plots <- vector("list", nag ^ 2)

  # EMPTY theme
  empty <- theme(axis.title=element_blank(), axis.text=element_blank(),
    axis.ticks=element_blank(), panel.grid.major=element_blank(),
    panel.grid.minor=element_blank())

  # EXTRACT data pairs for correlations, returns single list
  pairs <- Reduce("c", Map(function(cs, na) {
    lapply(cs, function(k) data.frame(x=c(x[k,]), y=c(x[na,])))
    }, na=seq(nag - 1), cs=combs))

  # COMPUTE correlations
  corrs <- lapply(pairs, function(i) round(cor(i$x, i$y, use="complete.obs"), 2))

  # PLOT correlations in lower triangle
  plots[lowt] <- lapply(corrs, function(i)
    ggplot(data.frame(x = 1, y = 1, text = i), aes(x, y)) +
	  geom_text(aes(label = text), size=lower_size) + empty)

  # PLOT correlations, returns gg
  pcors <- Map(function(da, co) {
    ggplot(da[!is.na(rowSums(da)),], aes(x=x, y=y)) +
		  geom_rect(aes(fill = co), xmin = -Inf, xmax = Inf,
	      ymin = -Inf, ymax = Inf, alpha = 0.8) +
  		scale_fill_gradient2(low="blue", mid = "white", high="red",
        limits=c(-1,1), guide = FALSE) +
      geom_point() +
      geom_smooth(method = "lm", fullrange = TRUE, col = 1, formula = y ~ x) +
      empty
    }, da = pairs, co = corrs)

  # PLACE plots in upper triangle
  plots[uppt] <- pcors

  # PLOT diagonal labels
  labs <- lapply(ages, function(i) {
    ggplot(data.frame(x=1, y=1, text=i), aes(x=x, y=y, label=text)) +
    geom_text(size=diag_size) + empty
    })

  # PLACE labels in diagonal
  plots[diag] <- labs

  # PREPARE grid object

  margin <- theme(plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm"))
  
  plots <- lapply(plots, "+", margin)

  res <- arrangeGrob(grobs=plots, nrow=nag, ncol=nag)

  # RETURN gg
  return(ggdraw() + draw_grob(grid::grobTree(res)))
  }
)
# }}}

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
  dato <- .dto(x, y0)

  # Hindcasted list(FLIndices)
  datp <- .dtp(y, y0)

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
} 

.dtp <- function(flis, y0) {

  rbindlist(lapply(names(flis), function(i) {
    pred <- pmin(an(i) + 1, an(names(flis)[1]))
    data.table(cbind(as.data.frame(lapply(flis[[ac(i)]], function(x) {
      # COMPUTE Total abundance in biomass
      window(quantSums(index(x) * catch.wt(x)), start=y0, end=pred)
    }), drop=TRUE, qname="index"), final=i, pred=pred))}))
}

.dto <- function(flis, y0) {

  data.table(as.data.frame(lapply(flis, function(x) {
    dmns <- dimnames(x)
    if(all(is.na(catch.wt(x))))
      stop("catch.wt in FLIndex is NA, cannot compute biomass.")
    window(quantSums(index (x) * catch.wt(x)), start=y0)
  }), drop=TRUE, qname="index"))

}
# }}}

# plotRunstest {{{

#' Plot the runs test result for one or more time series
#'
#' @param fit The result of a model fit.
#' @param obs The observations used in the fit.
#' @param combine Should ages be combined by addition, defaults to TRUE.
#' @param ... Extra arguments.
#'
#' @return An object of class ggplot2::gg
#'
#' @examples
#' data(nsher)
#' plotRunstest(fitted(nsher), rec(nsher))

setGeneric("plotRunstest", function(fit, obs, ...)
		standardGeneric("plotRunstest"))

#' @rdname plotRunstest

setMethod("plotRunstest", signature(fit="FLQuants", obs="missing"),
  function(fit, combine=TRUE) {

  # COMBINE
  if(combine) {
    fit <- lapply(fit, quantSums)
  }

  # RESIDUALS
  res <- fit
  
  # CREATE data.frame
  dat <- data.table(as.data.frame(res))

  # sigma3, by index
  s3dat <- runstest(res, combine=combine)
  
  # FIND single limits for all indices
  lims <- c(min=min(unlist(lapply(res, dims, c("minyear")))),
    max=max(unlist(lapply(res, dims, c("maxyear")))))
  
  # MERGE s3dat into dat
  if(combine)
    dat <- merge(dat, s3dat[, c('qname', 'lcl', 'ucl', 'pass')], by=c('qname'),
      all=TRUE)
  else {
    # TODO CHECK reasons behind
    dat <- merge(dat, s3dat[, c('qname', 'lcl', 'ucl', 'pass', 'age')],
      by=c('qname', 'age'), all = TRUE)

  }

  # ADD limits to colour outliers
  dat$outlier <- dat$data < dat$lcl | dat$data > dat$ucl
  
  # PLOT
  p <- ggplot(dat) +
    geom_rect(data=s3dat, aes(xmin=lims[1] - 1, xmax=lims[2] + 1,
      ymin=lcl, ymax=ucl, fill=pass), alpha=0.8) +
    scale_fill_manual(values=c("TRUE"="#cbe368", "FALSE"="#ef8575")) +
    geom_hline(yintercept=0, linetype=2) +
    geom_segment(aes(x=year, y=0, xend=year, yend=data), na.rm=TRUE) +
    geom_point(aes(x=year, y=data), size=1.5, na.rm=TRUE) +
    geom_point(aes(x=year, y=data, colour=outlier), size=1, na.rm=TRUE) +
    scale_colour_manual(values=c("FALSE"="#ffffff", "TRUE"="#d50102")) +
    xlab("") + ylab("Residuals") +
    theme(legend.position="none")

  if(combine)
    p <- p + facet_grid(qname ~ .)
  else
    p <- p + facet_grid(factor(age, levels=order(unique(age))) ~ qname,
      scales="free_y")

  return(p)
  }
)

#' @rdname plotRunstest

setMethod("plotRunstest", signature(fit="FLQuants", obs="FLQuants"),
  function(fit, obs, combine=TRUE) {
  
  # COMBINE
  if(combine) {
    fit <- lapply(fit, quantSums)
    obs <- lapply(obs, quantSums)
  }

  # RESIDUALS
  res <- FLQuants(mapply(residuals, obs, fit, SIMPLIFY=FALSE))

  return(plotRunstest(res, combine=combine))
  }
)

#' @rdname plotRunstest

setMethod("plotRunstest", signature(fit="FLQuant", obs="FLQuant"),
  function(fit, obs, combine=TRUE) {

    fit <- FLQuants(A=fit)
    obs <- FLQuants(A=obs)

    plotRunstest(fit, obs, combine=combine) +
      theme(strip.text = element_blank(), strip.background = element_blank())
  }
)
# }}}

# plotLengths {{{

setGeneric('plotLengths', function(x, ...) standardGeneric('plotLengths')) 

#' @examples
#' data(ple4)
#' iak <- invALK(FLPar(linf=42, k=2.1, t0=0.1), age=1:10)
#' les <- lenSamples(catch.n(ple4)[, 1:10], iak)
#' units(les) <- "cm"
#' plotLengths(les)
#' plotLengths(group(les, sum, year=year - year%%5))
#' plotLengths(group(les, mean, year=year - year%%5))
#' plotLengths(group(les, sum, year=year - year%%10))
#' plotLengths(les, block="decade")
#' plotLengths(les, direction="vertical")
#' plotLengths(group(les, mean, year=year - year%%5), direction="vertical")
#' plotLengths(les, direction="vertical", block="decade")
#' plotLengths(les) +
#'   geom_vline(data=as.data.frame(FLPar(L50=38)), aes(xintercept=data),
#'   linewidth=1)
#' plotLengths(les) +
#'   geom_vline(data=as.data.frame(FLPar(seq(38, 46, length=10), dimnames=list(params='L50', year=1957:1966, iter=1))), aes(xintercept=data),
#'   linewidth=1)
#' plotLengths(les, block="lustrum") +
#'   geom_vline(data=as.data.frame(FLPar(seq(38, 46, length=10), dimnames=list(params='L50', year=1957:1966, iter=1))), aes(xintercept=data),
#'   linewidth=1)

# TODO: plotComps
# TODO: OPTION to remove mean/median

setMethod("plotLengths", signature(x="FLQuant"),
  function(x, direction = c("horizontal", "vertical"),
    block=c("lustrum", "decade"), palette=flpalette) {

  # args
  direction <- match.arg(direction)
  block <- match.arg(block)
  step <- switch(block, 'decade'=10, 'lustrum'=5)

  # CONVERT to data.table w/date, timestep
  dat <- data.table(as.data.frame(x, timestep=TRUE,
    date=TRUE))

  # CALCULATE props by timestep & unit
  dat[, prop:=data / min(data[data>0]), by=.(timestep, unit)]
  # median
  dat[, median:=median(rep(len, prop)), by=.(timestep, unit)]
  # mean
  dat[, mean:=mean(rep(len, prop)), by=.(timestep, unit)]
  # min & max
  dat[, min:=min(len[data > 0]), by=.(timestep, unit)]
  dat[, max:=max(len[data > 0]), by=.(timestep, unit)]
 
  # PLOT without facets
  p <- ggplot(dat, aes(x=len, y=data)) +
    geom_col(fill="gray", alpha=0.4, colour="black") +
    ylab("") + xlab(paste0("Length (", units(x), ")")) +
    theme(axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
    geom_vline(aes(xintercept=mean), color=palette[1], linewidth=1, alpha=0.5) +
    geom_point(aes(x=mean, y=1), color=palette[1], size=2, alpha=0.5) + 
    geom_vline(aes(xintercept=median), color=palette[2], linewidth=1, alpha=0.5) +
    geom_point(aes(x=median, y=1), color=palette[2], size=2, alpha=0.5) +
    geom_point(aes(x=min, y=1), size=2, alpha=0.5, shape=15) +
    geom_point(aes(x=max, y=1), size=2, alpha=0.5, shape=15) +
    xlim(c(0,NA)) +
    theme(legend.position="none")

  # PARSE dims and CREATE formula
  dm <- dim(x)

  #  - year + season ~ area
  #  - fill =  unit | area
  
  if(direction == "horizontal") {

  # SET facetting
    if(dm[4] > 1)
      facets <- ~ year + season
    else
      facets <- ~ year

    p <- p + facet_grid(facets, scale="free") +
      coord_flip() + scale_y_reverse() +
      geom_vline(aes(xintercept=0,
        color=as.factor(year -  year %% step)), linewidth=3)
  } else {

  # SET facetting
    if(dm[4] > 1)
      facets <- (year %% step) + season ~ (year - year %% step)
    else
      facets <- (year %% step) ~ (year - year %% step)

    p <- p + facet_grid(facets, scales="free")
  }

  return(p)
})
