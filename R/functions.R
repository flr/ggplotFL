# functions.R - DESC
# ggplotFL/R/functions.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the GPL-2

# eqlabel {{{

#' Functions create labels for FLSR models
#'
#' These functions create labels from FLSR models and params to be used, for
#' example, on the legend of the ggplot-based plot method for FLSRs
#'
#' @rdname labels
#' @aliases eqlabel
#' @param model a list of model formulas
#' @param param a list of FLPar objects for the params slot
#' @examples
#'  data(nsher)
#'  srs <- FLSRs(sapply(c('ricker', 'bevholt'), function(x) {
#'    y <- nsher
#'    model(y) <- x
#'    return(fmle(y))
#'  }))
#'  eqlabel(model=lapply(srs, model),
#'    param=lapply(srs, params))
#'
eqlabel <- function(model, param) {

  # CREATE model formula labels
  labs <- sapply(names(model), function(x) deparse(do.call(substitute, list(model[[x]],
    env=lapply(as(param[[x]], 'list'), format, digits=4)))))
  # DROP "
  labs <- lapply(labs, function(x) gsub('"', '', x))
  # CHANGE ~ to %~%
  labs <- lapply(labs, function(x) gsub('~', '%~%', x))
  # CHANGE * to %.%
  labs <- lapply(labs, function(x) gsub('\\*', '%.%', x))

  # PARSE to get expression
  labs <- lapply(labs, function(x) parse(text=x))

  return(labs)
} # }}}

# modlabel {{{
#' @aliases modlabel
#' @rdname labels
#' @examples
#'
#'  modlabel(model=lapply(srs, model),
#'    param=lapply(srs, params))
#'
modlabel <- function(model, param) {

  # GET model name
  modn <- lapply(model, SRModelName)
  # GET params
  parn <- lapply(param, function(x) dimnames(x)$params)
 
  # ASSEMBLE param string
  par <- lapply(param, function(x)
    paste0("(", paste(format(c(iterMedians(x)), digits=4, trim=TRUE), collapse=", "), ")"))
 
  labs <- sapply(names(model), function(x) paste0(modn[[x]], par[[x]]))

  # PARSE to get expression
  labs <- lapply(labs, function(x) parse(text=x))

  return(labs)
} # }}}

# unitsLabels {{{
unitsLabels <- function(units, drop=c("NA", "NC", "f", "m", "z", "prop")) {

    # DROP certain units
    units[units %in% drop] <- ""

    # FORMAT
    idx <- units !=""
    units[idx] <- paste0(" (", units[idx], ")")

    units[] <- paste0(names(units), units)

    return(units)
} # }}}
