# plot.R - DESC
# plot.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(ggplotFL)

data(ple4)

plot(ple4)

catch(ple4) <- rnorm(100, catch(ple4), catch(ple4))
catch(ple4)[catch(ple4) < 0]  <- 0

plot(ple4)

pls <- FLStocks(runA=ple4, runB=qapply(ple4, function(x) x*1.10))

plot(pls)


