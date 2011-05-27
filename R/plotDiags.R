# plotDiags.R - 
# ggplotFL/R/plotDiags.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT
# $Id:  $

# Functions {{{
xCord <- function(i)
  floor(i/2)*2-i+2

yCord <- function(i)
  floor((i+1)/2)

vplayout <- function(x, y)
  viewport(layout.pos.row=x, layout.pos.col=y)


qqLine <- function(x,y) {
  qtlx <- quantile(x, prob=c(0.25,0.75), na.rm=T)
  qtly <- quantile(y, prob=c(0.25,0.75), na.rm=T)

  a <- (qtly[1]- qtly[2]) / (qtlx[1] - qtlx[2])
  b <- qtly[1] - qtlx[1] * a

  res <- c(a,b)
  names(res) <- NULL
  names(res) <- c("a","b")

  return(x*res["a"]+res["b"])
}

# adds QQ and AR1 data
QQAR <- function(object) {
  residualLag <- c(object$residual[-1],NA)
  qq. <- qqnorm(c(stdz(object$residual)),plot.it=FALSE)
  qqx <- qq.$x
  qqy <- qq.$y

  res <- data.frame(object, residualLag=residualLag, qqx=qqx, qqy=qqy,
    qqHat=qqLine(qqx,qqy))

  return(res)
} # }}}

# plotDiags(data.frame) {{{
setMethod("plotDiags", signature(object="data.frame"),
  function(object, col="#CCFF33", group=1, stuff=facet_wrap(~age,scale="free"),
    plots=c(1,2,3,4,5,6)) {
    
    #
    p <- ggplot(data.frame(object, col=col, group=group))

    # plot1
    plot1 <- function(i,p,stuff) {
      qqpar <- qqLine(p$data$qqx,p$data$qqy)
      p1 <- p + geom_point(aes(qqx,qqy,col=col,group=group),size=2.5) +
        opts(title = "Normal Q-Q Plot") +
        scale_x_continuous(name="Theoretical Quantiles", scale="free") +
        scale_y_continuous(name="Sample Quantiles"     , scale="free") +
        geom_abline(intercept=qqpar["b"], slope=qqpar["a"]) +
        stuff

      print(p1, vp=vplayout(xCord(i),yCord(i)))
    }

    # plot2
    plot2 <- function(i,p,stuff) {
      p2 <- p + geom_point(aes(obs, hat, col=col, group=group)) +
        geom_line(aes(x,yHat,col=col,group=group)) +
        stuff

      print(p2, vp=vplayout(xCord(i),yCord(i)))
    }

    # plot3
    plot3 <- function(i, p, stuff) {
      p3 <- p + geom_point(aes(year, rsdl, col=col, group=group)) +
        stat_smooth(aes(year,rsdl,col=col,group=group)) +
        scale_x_continuous(name="Year") +
        scale_y_continuous(name=expression(Residuals)) +
        stuff +
        geom_abline(intercept=0, slope=0)

      print(p3, vp=vplayout(xCord(i),yCord(i)))
    }

    # plot4
    plot4 <- function(i, p, stuff) {
      p4 <- p + geom_point(aes(rsdl, rsdlLag, col=col, group=group)) +
        stat_smooth(aes(rsdl,rsdlLag,col=col,group=group)) +
        stuff +
        geom_abline(intercept=0, slope=01)

      print(p4, vp=vplayout(xCord(i),yCord(i)))
    }

    # plot5
    plot5 <- function(i,p,stuff) {
      p5 <- p + geom_point(aes(obs, rsdl, col=col, group=group)) +
        stat_smooth(aes(x, rsdl, col=col, group=group)) +
        scale_x_continuous(name="Index") +
        scale_y_continuous(name=expression(Residuals)) +
        stuff +
        geom_abline(intercept=0, slope=0)

      print(p5, vp=vplayout(xCord(i),yCord(i)))
    }

    # plot6
    plot6 <- function(i,p,stuff) {
      p6 <- p + geom_point(aes(hat, rsdl, col=col, group=group)) +
        stat_smooth(aes(yHat,rsdl,col=col,group=group)) +
        scale_x_continuous(name=expression(Residuals[t])) +
        scale_y_continuous(name=expression(Residuals)) +
        stuff + geom_abline(intercept=0, slope=0)

      print(p6, vp=vplayout(xCord(i),yCord(i)))
    }

    #
    for (i in 1:length(plots)) {
      switch(ac(plots[i]),
           "1"=plot1(i,p,stuff),
           "2"=plot2(i,p,stuff),
           "3"=plot3(i,p,stuff),
           "4"=plot4(i,p,stuff),
           "5"=plot5(i,p,stuff),
           "6"=plot6(i,p,stuff))
     }
   }
) # }}}


