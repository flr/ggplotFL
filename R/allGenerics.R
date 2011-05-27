# allGenerics.R - 
# ggplotFL/R/allGenerics.R


# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

setGeneric("ggplot", useAsDefault = ggplot2::ggplot)

setGeneric('diags', function(object, ...)
	standardGeneric('diags'))

setGeneric("plotDiags", function(object,...)
	standardGeneric("plotDiags"))

setGeneric("plotEP", function(object,...)
   standardGeneric("plotEP"))
