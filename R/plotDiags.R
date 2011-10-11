# plotDiags.R - 
# ggplotFL/R/plotDiags.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT
# $Id:  $

setGeneric('diags', function(object, ...)
  standardGeneric('diags'))
 

diagPlots=function() list(#### Specification of what is to be plotted in diagnostic plot

"FLSR"    =data.frame(name=c("Functional Form","Residuals By Year","AR(1) Residuals","Residuals by Predictor","Normal Q-Q Plot","Residuals by Predicted"),
                      x   =c("x",              "year",             "residual",       "x",                     "qqx",            "yHat"),
                      y   =c("y",              "residual",         "residualLag",    "residual",              "qqy",            "residual"),
                      hat =c("yHat",           "ref",              "ref",            "ref",                   "qqHat",          "ref")),
                  
"seine"   =data.frame(name=c("Functional Form","Residuals By Year","AR(1) Residuals","Residuals by Predictor","Normal Q-Q Plot","Residuals by Predicted"),
                      x   =c("x",              "year",             "residual",       "x",                     "qqx",            "yHat"),
                      y   =c("y",              "residual",         "residualLag",    "residual",              "qqy",            "residual"),
                      hat =c("yHat",           "ref",              "ref",            "ref",                   "qqHat",          "ref")),

"FLBioDym"=data.frame(name=c("Functional Form","Residuals By Year","AR(1) Residuals","Residuals by Predictor","Normal Q-Q Plot","Residuals by Predicted"),
                      x   =c("x",              "year",             "residual",       "x",                     "qqx",            "yHat"),
                      y   =c("y",              "residual",         "residualLag",    "residual",              "qqy",            "residual"),
                      hat =c("yHat",           "ref",              "ref",            "ref",                   "qqHat",          "ref")),

"FLSAM"   =data.frame(name=c("Functional Form","Residuals By Year","AR(1) Residuals","Residuals by Predictor","Normal Q-Q Plot","Residuals by Predicted"),
                      x   =c("x",              "year",             "residual",       "x",                     "qqx",            "yHat"),
                      y   =c("y",              "residual",         "residualLag",    "residual",              "qqy",            "residual"),
                      hat =c("yHat",           "ref",              "ref",            "ref",                   "qqHat",          "ref")),

"FLXSA"   =data.frame(name=c("Functional Form","Residuals By Year","AR(1) Residuals","Residuals by Predictor","Normal Q-Q Plot","Residuals by Predicted"),
                      x   =c("x",              "year",             "residual",       "x",                     "qqx",            "yHat"),
                      y   =c("y",              "residual",         "residualLag",    "residual",              "qqy",            "residual"),
                      hat =c("yHat",           "ref",              "ref",            "ref",                   "qqHat",          "ref")))



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

## creates plotting df
reform=function(x,y,hat,name,object) 
             data.frame("name"=name,
                        "x"   =as.numeric(object[,ac(x)]),
                        "y"   =as.numeric(object[,ac(y)]),
                        "hat" =as.numeric(object[,ac(hat)]))
 
## function to create diags DF for plotting from residuals
getDiag=function(object,diagPlots=diagPlots()[[class(object)]]) {
    
    ## extract values  
    x        = object$x
    y        = object$y
    yHat     = object$hat
    residual = object$residual

    #residualLag = c(NA,rev(rev(residual)[-1]))
    residualLag = object$residualLag

    qq = qqnorm(residual,plot.it=FALSE)
       
    res = data.frame(x=x, y=y, yHat=yHat, residual=residual, residualLag=residualLag,
                                qqx=qq$x, qqy=qq$y, year=object$year)
    
    qqpar = qqLine(res$qqx,res$qqy)[c("a","b")]

    res =data.frame(res,qqHat=qqpar["a"]*res$qqx+qqpar["b"],"ref"=0)
    
    res=mdply(diagPlots, reform, object=res)
    
    return(res)}

## FLSR
setMethod("diags", signature(object="FLSR"),
  function(object,sctPlot=diagPlots()$FLSR){
    #   function(object, col="#CCFF33", group=1, stuff=facet_wrap(~age,scale="free"),
    
      res=model.frame(FLQuants("x"       =ssb(      object),
                               "y"       =rec(      object),
                               "hat"     =predict(  object),
                               "residual"=residuals(object)),drop=TRUE)
 
     res=getDiag(res,sctPlot)
     
     if ("Functional Form" %in% sctPlot$name)  
        res=rbind.fill(res,cbind(name="Functional Form",  
                model.frame(FLQuants("hat" =predict(object,ssb=FLQuant(seq(0,min(ssb(object)),length.out=21))),
                                     "x"   =FLQuant(seq(0,min(ssb(object)),length.out=21))),drop=TRUE)[,-1]))
                
    # ggplot(diags(pSR,srPlot))                                  + 
    #     geom_point(aes(x,y))+facet_wrap(~name,scale="free")    +
    #     geom_line(aes(x,hat),colour="red")                     +
    #     stat_smooth(aes(x,y))

    return(res)})

## function to create diags DF for plotting from residuals
getDiag=function(object,diagPlots=diagPlots()[[class(object)]]) {
         
    qqpar=qqLine(object$qqx,object$qqy)[c("a","b")]
    res  =data.frame(object,qqHat=qqpar["a"]*object$qqx+qqpar["b"],"ref"=0)
    res  =mdply(diagPlots, reform, object=res)
    
    return(res)}

setMethod("diags", signature(object="FLSAM"),
  function(object){
      
    res=FLSAM::diags(object)
    res=res[do.call(order, res[,c("fleet","age","x")]),]
 
    res2=ddply(res,.(fleet,age), getDiag,diagPlots()[["FLSAM"]])
    res2=res2[do.call(order, res2[,c("fleet","age","x")]),]
     
    p.=ggplot(res2) + 
        geom_point(aes(x,y,group=age:fleet))                           +
        facet_wrap(~name,scale="free")                 +
        geom_path(aes(x,hat,colour="red",group=factor(fleet):factor(age)))+geom_smooth(aes(x,y))
 
  print(p.)
  return(invisible(p.))})