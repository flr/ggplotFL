# plotEP.R - 
# ggplotFL/R/plotEP.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT
# $Id:  $

stdz  <-function(x,na.rm=TRUE) ((x-mean(x,na.rm=na.rm))/sd(x,na.rm=na.rm))

# fnplotEP {{{
fnplotEP <- function(object,vars,theme) {
  
  # data into data.frame 
  grw <- mdply(vars, function(x,stk)
    as.data.frame(do.call(x,list(stk))), stk=object)
  grw$X1 <- factor(vars[grw$X1], levels=vars)
  grw$decade <- factor(decade(grw$year))

  # themes
 G.theme <- theme(
            axis.ticks.length=unit(0.1, "line"),
            axis.title.x=element_blank(),
  		       axis.text.x=element_blank(),
  		       plot.margin=unit(c(0,1,0,1), "lines"))

  # snug fit
  snug.opts <- theme(
      axis.ticks        = element_blank(), 
		  axis.title.x      = element_blank(), 
		  axis.title.y      = element_blank(), 
		  axis.text.x       = element_blank(), 
		  axis.text.y       = element_blank(), 
		  axis.ticks.length = unit(0, "lines"), 
		  axis.ticks.margin = unit(0, "lines"), 
		  panel.margin      = unit(c(0.5, 0.25,-1.0, 0.25), "lines"), 
		  plot.margin       = unit(c(1,1.75,-1.75,3.5), "lines")) 


  ## ggplot objects
  # -by-age
  pG <- ggplot(ddply(grw,.(X1), transform, data=data/max(data,na.rm=T))) +
    geom_point(aes( age,data,group=decade,colour=decade)) +
    stat_smooth(aes(age,data,group=decade,colour=decade)) +
    facet_grid(~X1) +
    scale_colour_discrete(guide="none") +
    G.theme

  # residuals
  pR <- ggplot(ddply(grw, .(X1, age), transform, data=stdz(data,na.rm=T))) +
    geom_point(aes(age,year,size=abs(data),col=ifelse(data<0,"red","black"))) +
	  scale_size_area(max_size=7.5,name="Residual",  guide="none") +
	  scale_colour_manual(values=c("black","red"),guide="none") +
	  facet_grid(~X1) +
	  ylab("Year") + xlab("Age")

  
  return(list(rsdl=pR,age=pG))

  grid.newpage()
  pushViewport(viewport(layout=grid.layout(4,1)))

  print(pR + opts(strip.background=element_blank()),
    vp=viewport(layout.pos.row=2:4, layout.pos.col=1))
  
  print(pG + snug.opts, vp=viewport(layout.pos.row=1, layout.pos.col=1))

  # plotting
  invisible(list(rsdl=pR,age=pG))
  
} # }}}

# plotEP(FLStock) {{{
setMethod('plotEP', signature(object='FLStock'), 
  function(object,vars=c("mat","stock.wt","EP"),theme=theme_ms) fnplotEP(object, vars, theme))
# }}}

# plotEP(FLBiol) {{{
setMethod('plotEP', signature(object='FLBiol'), 
  function(object,vars=c("mat","stock.wt","EP"),theme=theme_ms) fnplotEP(object, vars, theme))
# }}}
