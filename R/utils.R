# utils.R - DESC
# utils.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# FLQ2df {{{

FLQ2df <- function(object, quantiles=c(0.5)) {

	# FLQuants

	# labels for qname factors
	nms <- unlist(lapply(object, function(y) paste("(", attr(y, 'units'), ")", sep="")))
	nms <- paste(names(nms), nms)
		
	# extract quantiles
	res <- lapply(object, function(y)
		cast(as.data.frame(quantile(y, quantiles)), 
		age+year+unit+season+area~iter, value="data"))

	# reshape data.frame
	res <- cast(melt(res), age+year+unit+season+area+L1~iter)
	
	# names of columns
	names(res) <- c("age","year","unit","season","area","qname",
		paste("q", quantiles*100, sep=""))

	# qname as factor w/ level labels from nms
	res <- transform(res, qname=factor(qname, levels=names(object), labels=nms))

	# quantiles
	attr(res, 'quantiles') <- quantiles

	return(res)
} # }}}
