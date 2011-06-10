# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

library(ggplotFL)

data(ple4sex)

fls <- FLStocks(mal=ple4sex[,,'male'], fem=ple4sex[,,'female'])

plot(fls)

plot(fls[[1]])

plot(fls[[1]], 1)
