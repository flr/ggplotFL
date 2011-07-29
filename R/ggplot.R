# ggplot.R - 
# ggplotFL/R/ggplot.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT
# $Id:  $

setMethod("ggplot", signature(data="FLQuant"),
  function(data, ...) {
    dat <- as.data.frame(data, cohort=TRUE)
    ggplot(dat, ...)})

setMethod("ggplot", signature(data="FLQuants"),
  function(data, ...) {
    dat <- as.data.frame(data)
    dat$cohort <- dat$year - as.numeric(dat$age)
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
