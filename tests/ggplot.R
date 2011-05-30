# ggplot.R - 
# ggplotFL/R/ggplot.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT
# $Id:  $

flq           <-FLQuants()
flq[["age"]]  <-as.FLQuant(rnorm(100),dimnames=list(age  =1:10, year=2001:2010))
flq[["quant"]]<-as.FLQuant(rnorm(100),dimnames=list(quant=1:10, year=2001:2010))
flq[["all"]]  <-as.FLQuant(rnorm(10), dimnames=list(age  ="all",year=2001:2010))

head(ggplot(flq[["age"]]  )$data)
head(ggplot(flq[["quant"]])$data)
head(ggplot(flq[["all"]]  )$data)

head(ggplot(flq)$data)
head(ggplot(ldply(flq, function(x) as.data.frame(x,cohort=TRUE)))$data)

data(ple4)
head(ggplot(ple4)$data)
head(ggplot(data(FLStocks("a"=ple4,"b"=window(ple4,2000))))$data)
