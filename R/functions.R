# functions.R - DESC
# ggplotFL/R/functions.R

# Copyright 2012-2017 FLR Team. Distributed under the GPL 2
# Maintainer: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu

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

    # FORMAT
    idx <- units != ""
    units[idx] <- paste0("~(", units[idx], ")")
    
    units[] <- paste0(names(units), as.expression(units))

    
    return(as_labeller(units, label_parsed))
} # }}}

setGeneric("%+%", useAsDefault=ggplot2::`%+%`)

# FLQuant %+% FLQuant {{{
setMethod("%+%", signature(e1="FLQuant", e2="FLQuant"),
	function(e1, e2) {

    # get dims
    de1 <- dim(e1)
    de2 <- dim(e2)

    # final dims
    di <- pmax(de1, de2)
    dli <- lapply(as.list(di), function(e1) rep(1, e1))

    # TEST: No expansion n -> m allowed, must be originally 1
    if(any(di != de1 &  de1 != 1) | any(di != de2 &  de2 != 1))
      stop("dims to be expanded cannot be of length > 1")

    # new e1
    dle1 <- lapply(as.list(de1), seq)
    dle1[di > de1] <- dli[di > de1]

    re1 <- do.call('[', c(list(e1=e1@.Data, drop=FALSE), dle1))

    # new e2
    dle2 <- lapply(as.list(de2), seq)
    dle2[di > de2] <- dli[di > de2]

    re2 <- do.call('[', c(list(e1=e2@.Data, drop=FALSE), dle2))

    # dimnames
    dni <- dimnames(e1)
    dni[di > de1] <- dimnames(e2)[di > de1]
	
		# units
		if(identical(units(e1), units(e2))) {
			units <- units(e1)
		} else {
			units <- uom('+', units(e1), units(e2))
		}

    return(FLQuant(re1 + re2, dimnames=dni, units=units))
  }
) # }}}

# FLPar %+% FLQuant {{{
setMethod("%+%", signature(e1="FLPar", e2="FLQuant"),
	function(e1, e2) {

    x <- e1
    y <- e2

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

        # TEST: non-matching dims in x should be of length 1
    idy <- !names(dnx) %in% names(dny)
    if(any(dx[idy] > 1))
      stop("dimensions in 'x' not matching those in 'y' must be of length=1")

    # aperm if FLPar dimnames sorted differently to FLQuant's
    idx <- matchDimnames(dnx, dny)
    if(any(idx != sort(idx))) {
      x <- aperm(x, idx)
      dx <- dx[idx]
      dnx <- dnx[idx]
    }

    # tmp FLQuant dims
    di <- rep(1, 6)
    di[names(dny) %in% names(dnx)] <- dx[names(dnx) %in% names(dny)]

    # x data in 6D array
    rx <- array(x@.Data, dim=di)

    # expansion done in %+%(FLQuant, FLQuant)
    return(FLQuant(rx, quant=quant(y)) %+% y)
  }
) # }}}

# FLQuant %+% FLPar {{{
setMethod("%+%", signature(e1="FLQuant", e2="FLPar"),
	function(e1, e2) {

    x <- e1
    y <- e2

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

    # TEST: non-matching dims in y should be of length 1
    idx <- !names(dny) %in% names(dnx)
    if(any(dy[idx] > 1))
      stop("dimensions in 'y' not matching those in 'x' must be of length=1")

    # aperm if FLPar dimnames sorted differently to FLQuant's
    idy <- matchDimnames(dny, dnx)
    if(any(idy != sort(idy))) {
      y <- aperm(y, idy)
      dy <- dy[idy]
      dny <- dny[idy]
    }

    # tmp FLQuant dims
    di <- rep(1, 6)
    di[names(dnx) %in% names(dny)] <- dy[names(dny) %in% names(dnx)]

    # y data in 6D array
    ry <- array(y@.Data, dim=di)

    # expansion done in %+%(FLQuant, FLQuant)
    return(x %+% FLQuant(ry))
  }
) # }}}

# FLPar %+% FLPar {{{
setMethod("%+%", signature(e1="FLPar", e2="FLPar"),
	function(e1, e2) {

    x <- e1
    y <- e2

    # dimnames
    dnx <- dimnames(x)
    dny <- dimnames(y)
		
		ldx <- unlist(lapply(dnx, length))
		ldy <- unlist(lapply(dny, length))

		# apply operation directly if dimnames match
		if(identical(ldx, ldy))
			return(x + y)
    
		# vector of final dim
    dnd <- rbind(ldx, ldy)
    
    # TEST: non-matching dnames in x or y should be of length 1
    if(any(apply(dnd, 2, function(x) all(x > 0) && max(x)/min(x) != max(x))))
      stop("dimensions in 'x' not matching in length those in 'y' must be of length=1")

		# new dim
    dr <- pmax(ldx, ldy)

		# new dimnames
		dni <- apply(dnd, 2, which.max)
		dnx[dni == 2] <- dny[dni == 2]

    # TODO expand & aperm FLPars
    FLPar(array(x@.Data, dim=dr, dimnames=dnx) + array(y@.Data, dim=dr, dimnames=dnx))
  }
) # }}}
