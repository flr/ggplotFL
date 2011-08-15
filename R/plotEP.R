# plotEP.R - 
# ggplotFL/R/plotEP.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT
# $Id:  $

# fnplotEP {{{
fnplotEP <- function(object,vars,theme) {
  
  # data into data.frame 
  grw <- mdply(vars, function(x,stk)
    as.data.frame(do.call(x,list(stk))), stk=object)
  grw$X1 <- factor(vars[grw$X1], levels=vars)
  grw$decade <- factor(decade(grw$year))

  # themes
  G.theme <- opts(theme_flr(12,
      list(axis.ticks.length=unit(0.1, "line"),
           axis.title.x=theme_blank(),
		       axis.text.x=theme_blank(),
		       plot.margin=unit(c(0,1,0,1), "lines"))))

  # snug fit
  snug.opts <- opts(
      axis.ticks        = theme_blank(), 
		  axis.title.x      = theme_blank(), 
		  axis.title.y      = theme_blank(), 
		  axis.text.x       = theme_blank(), 
		  axis.text.y       = theme_blank(), 
		  axis.ticks.length = unit(0, "lines"), 
		  axis.ticks.margin = unit(0, "lines"), 
		  panel.margin      = unit(c(0.5, 0.25,-1.0, 0.25), "lines"), 
		  plot.margin       = unit(c(1,1.75,-1.75,3.5), "lines")) 

  ## ggplot objects
  # -by-age
  pG <- ggplot(ddply(grw,.(X1), transform, data=data/mean(data,na.rm=T))) +
    geom_point(aes( age,data,group=decade,colour=decade)) +
    stat_smooth(aes(age,data,group=decade,colour=decade)) +
    facet_grid(~X1) +
    scale_colour_discrete(legend=FALSE) +
    opts(G.theme)

  # residuals
  pR <- ggplot(ddply(grw, .(X1, age), transform, data=stdz(data,na.rm=T))) +
    geom_point(aes(age,year,size=abs(data),col=ifelse(data<0,"red","black"))) +
	  scale_area(to=c(0,7.5),name="Residual",      legend=FALSE) +
	  scale_colour_manual(values=c("black","red"),legend=FALSE) +
	  facet_grid(~X1) +
	  ylab("Year") + xlab("Age")

  # plotting
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(4,1)))

  print(pR + opts(strip.background=theme_blank()),
    vp=viewport(layout.pos.row=2:4, layout.pos.col=1))
  
  print(pG + snug.opts, vp=viewport(layout.pos.row=1, layout.pos.col=1))

  invisible(list(rsdl=pR,age=pG))
} # }}}

# plotEP(FLStock) {{{
setMethod('plotEP', signature(object='FLStock'), 
  function(object,vars=c("mat","stock.wt","EP"),theme=theme_flr) fnplotEP(object, vars, theme))
# }}}

# plotEP(FLBiol) {{{
setMethod('plotEP', signature(object='FLBiol'), 
  function(object,vars=c("mat","stock.wt","EP"),theme=theme_flr) fnplotEP(object, vars, theme))
# }}}
