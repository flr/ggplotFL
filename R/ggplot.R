# ggplot.R - 
# ggplotFL/R/ggplot.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT
# $Id:  $

# as.data.frame
setMethod("as.data.frame", signature(x="FLQuant", row.names="missing",
  optional="missing"),
	function(x, row.names=NULL, optional="missing") {
    res <- callNextMethod()

    # create cohort column as year - age
    res$cohort  <-  as.numeric(NA)
    if(quant(x) == "age")
      try(res$cohort <- res$year - res$age)

    return(res)
  }
)

# ggplot {{{
setMethod("ggplot", signature(data="FLQuant"),
  function(data, ...) {
    dat <- as.data.frame(data)
    ggplot(dat, ...)
  }
)

setMethod("ggplot", signature(data="FLQuants"),
  function(data, ...) {
    dat <- as.data.frame(data)
    dat$cohort <- dat$year - dat$age
    ggplot(dat, ...)})

setMethod("ggplot", signature(data="FLQuants"),
  function(data, ...) {
    dat <- as.data.frame(data)
    dat$cohort <- dat$year - dat$age
    ggplot(dat, ...)})

setMethod("ggplot", signature(data="FLComp"),
  function(data, ...) {
    dat <- as.data.frame(data)

    try(dat$cohort <- dat$year - dat$age)
    ggplot(dat, ...)})

setMethod("ggplot", signature(data="FLComps"),
  function(data, ...) {
    dat <- as.data.frame(data)

    try(dat$cohort <- dat$year - dat$age)
    ggplot(dat, ...)})

# TODO delete, as redundant
setMethod("ggplot", signature(data="FLCohort"),
  function(data, ...){
    dat <- as.data.frame(data)
    dat$cohort <- dat$year - dat$age
    ggplot(dat, ...)})
