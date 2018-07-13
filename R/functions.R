# functions.R - DESC
# ggplotFL/R/functions.R

# Copyright 2012-2017 FLR Team. Distributed under the GPL 2
# Maintainer: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu

utils::globalVariables(c("qname", "time"))

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
  labs <- sapply(names(model),
    function(x) deparse(do.call(substitute, list(model[[x]],
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
    paste0("(", paste(format(c(iterMedians(x)), digits=4, trim=TRUE),
      collapse=", "), ")"))
 
  labs <- sapply(names(model), function(x) paste0(modn[[x]], par[[x]]))

  # PARSE to get expression
  labs <- lapply(labs, function(x) parse(text=x))

  return(labs)
} # }}}

# label_flqs {{{

#' A ggplot2 labeller for FLQuants
#'
#' Plots of \code{\link{FLQuants}} objects use by default \code{\link{facet_grid}}
#' to separate the different elements. This function generates facet labels that
#' have both the element name and the units of measurement of each, as stored
#' in the 'units' slot of each \code{\link{FLQuant}}.
#'
#' Certain *units* are dropped from the label, as being uninformative: "NA", "NC",
#' "m", "z", and "prop". This can be selected with the *drop* argument.
#'
#' @param x An object of class \code{FLQuants}
#' @param drop Character string to be dropped from the label shen found in the
#' *units* slot. Defaults to c("NA", "NC", "m", "z", "prop")
#'
#' @return A ggplot2 labeller function
#'
#' @name label_flqs
#' @rdname label_flqs
#' @aliases label_flqs
#'
#' @author Iago Mosqueira (EC JRC)
#' @seealso \link[ggplot2]{labeller}
#' @keywords dplot

label_flqs <- function(x, drop=c("NA", "NC", "m", "f", "z", "prop")) {
		
    units <- unlist(lapply(x, attr, 'units'))

    # DROP certain units
    units[units %in% drop] <- ""
    
    # DROP NAs and empty characters
    units <- gsub("NA", "", units)
    units <- gsub(" ", "", units)

    # DROP unparseable units (w/o any alnum & not in uomTable)
    units[!uomUnits(units) & !grepl("[[:alnum:]]", units)]  <- character(1)

    # FORMAT
    idx <- units != ""
    units[idx] <- paste0("~(", units[idx], ")")
    
    units[] <- paste0("`", names(units), "`", as.expression(units))

    return(as_labeller(units, label_parsed))
} # }}}

# .flpalette {{{
# <http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/>
.flpalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7")
# }}}
