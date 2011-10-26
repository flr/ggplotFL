# plot.R - 
# ggplotFL/R/plot.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT
# $Id:  $

#     fn=list("SSB"=ssb, "Recruits" = rec,
#       "Plus Group"=function(x) stock.n(x)[ac(dims(x)$max)],
#       "Fpg"=function(x) harvest(x)[ac(dims(x)$max)],
#       "F2:5"=function(x) apply(harvest(x)[ac(2:5)],2,mean))
    
# plot(FLStock) {{{
setMethod("plot", signature(x="FLStock", y="missing"),
  function(x, probs=c(0.75,0.50,0.25), size=c(0.5,1.0,0.5), lty=c(2,1,2),
    facet=facet_wrap(~qname,scale="free"),
    fn=list("SSB"=ssb, "Recruits" = rec, "Yield"=catch, F=fbar),...) {
    
    plotComp(x,fn,probs,size,lty,facet)
  }
) # }}}

# plot(FLStocks) {{{
setMethod("plot", signature(x="FLStocks", y="missing"),
  function(x, probs=c(0.75,0.50,0.25), size=c(0.5,1.0,0.5), lty=c(2,1,2),
    facet=facet_wrap(~qname,scale="free"),
    fn=list("SSB"=ssb, "Recruits"=rec, "Yield"=catch, F=fbar),...)
    plotComps(x,fn,probs,size,lty,facet)
) # }}}
