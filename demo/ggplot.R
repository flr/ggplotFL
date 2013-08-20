# ggplot.R - DESC
# ggplot.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(ggplotFL)

data(ple4)

# FLQuant

dat <- catch.n(ple4)

ggplot(data=dat, aes(year, data)) + geom_point()

# FLQuants

dat <- FLQuants(catch=catch(ple4), f=fbar(ple4), catch2=catch(ple4), f2=fbar(ple4))

ggplot(data=dat, aes(year, data)) + geom_point() + facet_wrap(~qname, scales="free")

ggplot(data=dat, aes(year, data)) + geom_point() + facet_wrap(~qname, nrow=2, scales="free")

ggplot(data=dat, aes(year, data)) + geom_point() + facet_grid(qname~. , scales="free")

# FLStock

ggplot(data=ple4, aes(year, data)) + geom_point(aes(colour=age)) + facet_wrap(~slot, scale="free")
