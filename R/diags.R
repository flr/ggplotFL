# ggplotFL/R/diags.R
# 
# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT
# $Id:  $

## local function to calculated expected QQ line
qqLine <- function(x,y){ 
  qtlx <- quantile(x, prob=c(0.25,0.75), na.rm=T)
  qtly <- quantile(y, prob=c(0.25,0.75), na.rm=T)
      
  a <- (qtly[1]- qtly[2]) / (qtlx[1] - qtlx[2])
  b <- qtly[1] - qtlx[1] * a
      
  res <- c(a,b)
        
  names(res) <- NULL
  names(res) <- c("a","b")

 return(res)}

 fnDiags=function(res){
      res$residualLag <- c(res$residual[-1],NA)
     
      qq.     <- qqnorm(res$residual,plot.it=FALSE,na.rm=T)
      res$qqx <- qq.$x
      res$qqy <- qq.$y
      
      qqpar <- qqLine(qq.$x,qq.$y)[c("a","b")]

      res$qqHat=qqpar["a"]*res$qqx+qqpar["b"]
      
      res}
 
# diags(FLXSA) {{{
setMethod("diags", signature(object="FLXSA"),
  function(object,i=NULL) {

    # TODO
    fn <- function(object,i) {
      x <- index(object)[[i]]
      yHat <- index.hat(object)[[i]]
      residual <- index.res(object)[[i]]

      #
      dmns <- dimnames(x)
      y <- stock.n(object)[dmns$age,dmns$year]

      #
      residualLag      =FLQuant(NA,dimnames=dimnames(residual))
      residualLag[,-dim(residual)[2]] <- residual[,-1]
      qq. <- qqnorm(c(residual),plot.it=FALSE)
      qqx <- FLQuant(qq.$x,dimnames=dimnames(residual))
      qqy <- FLQuant(qq.$y,dimnames=dimnames(residual))

      #
      res <- model.frame(FLQuants(x=x, y=y, yHat=yHat, residual=residual,
          residualLag=residualLag, qqx=qqx, qqy=qqy))

      return(res)
    }

    #
    if (is.null(i)) {

      nms <- index.name(object)
      res <- mdply(data.frame(x=1:length(nms)),
        function(x,object)
          data.frame(index=nms[x],fn(object,x)), object=object)
    } else
      res <- fn(object, i)

    return(res)
  }
) # }}}

# diags(FLSR) {{{

setMethod("diags", signature(object="FLSR"),
  function(object, i=NULL) {
    
    #
    x <- ssb(object)
    y <- rec(object)
    yHat <- predict(object)
    residual <- residuals(object)

    dmns <- dimnames(x)

    residualLag <- FLQuant(NA, dimnames=dimnames(residual))
    residualLag[,-dim(residual)[2]] <- residual[,-1]

    qq. <- qqnorm(c(residual),plot.it=FALSE)
    qqx <- FLQuant(qq.$x,dimnames=dimnames(residual))
    qqy <- FLQuant(qq.$y,dimnames=dimnames(residual))

    ssb <- FLQuant(seq(0, max(x,na.rm=T), length.out=dim(x)[2]), dimnames=dimnames(x))
    rec <- predict(object, ssb=ssb)

    res <- model.frame(FLQuants(x=x, y=y, yHat=yHat, residual=residual, residualLag=residualLag,
                                qqx=qqx, qqy=qqy, rec=rec, ssb=ssb))
    
    qqpar <- qqLine(c(qqx),c(qqy))[c("a","b")]

    res <-data.frame(res,qqHat=c(qqpar["a"]*qqx+qqpar["b"]))
    
    return(res)
  }
) # }}}

