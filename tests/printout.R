# printout.R - DESC
# /printout.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ggplotFL)

data(ple4)
data(ple4sex)

# FLQuant {{{

pdf(paste0(tempfile(), ".pdf"))

  # dim c(1, 61, 1, 1, 1, 1)
plot(catch(ple4))
  
  # dim c(1, 61, 1, 1, 1, 300)
plot(rnorm(300, catch(ple4), catch(ple4)/2))
  
  # dim c(10, 61, 1, 1, 1, 1)
plot(catch.n(ple4))
  
  # dim c(1, 61, 1, 1, 1, 300)
plot(rnorm(300, catch.n(ple4), catch.n(ple4)/2))
  
  # dim c(1, 61, 2, 1, 1, 1)
plot(catch(ple4sex))
  
  # dim c(1, 61, 2, 1, 1, 300)
plot(rnorm(300, catch(ple4sex), catch(ple4sex) / 2))
  
  # dim c(1, 61, 2, 1, 2, 1)
plot(expand(catch(ple4sex), area=1:3))
  
  # dim c(1, 61, 2, 1, 2, 300)
plot(rnorm(300, expand(catch(ple4sex), area=1:3), 100000))
  
  # dim c(3, 61, 2, 1, 2, 1)
plot(expand(catch.n(ple4sex)[1:3,], area=1:3))
  
  # dim c(3, 61, 2, 1, 2, 300)
plot(rnorm(300, expand(catch.n(ple4sex)[1:3,], area=1:3), 100000))

dev.off()
# }}}


data("ple4")
stkn <- ple4@stock.n
stkn[1,] <- NA
stkn[,ac(1981:2015)] <- NA
plot(stkn)
