## moments

##### Utility functions
xCord <- function(i)
  floor(i/2)*2-i+2

yCord <- function(i)
  floor((i+1)/2)

vplayout <- function(x, y)
  viewport(layout.pos.row=x, layout.pos.col=y)

moments<-function(x,n,p=1) (sum(x^p*n)/sum(n))^(1/p)
decade <-function(x) x-(x %% 10)

stdz  <-function(x,na.rm=TRUE) ((x-mean(x,na.rm=na.rm))/sd(x,na.rm=na.rm))
minMax<-function(x,na.rm=TRUE) (x-min(x,na.rm=na.rm))/diff(range(x,na.rm=na.rm))

## Calculates frequency of points in squares
calcFreq=function(x,y,x.n=11,y.n=x.n){
 
  df=data.frame(x=x,y=y)
  df=data.frame(df,xFac=cut(df$x,seq(min(df$x),max(df$x),length.out=x.n)),
                   yFac=cut(df$y,seq(min(df$y),max(df$y),length.out=y.n)))
  
  c.=ddply(data.frame(df,count=1),.(xFac,yFac), function(x) count(x$count))[,c("xFac","yFac","freq")]
  
  p.=merge(df,c.,by=c("xFac","yFac"))[,c("x","y","freq","xFac","yFac")]
  
  return(p.[order(p.$freq),])}

## calculates density of points
calcDensity=function(x,y,x.n=11,y.n=x.n){

   require(MASS)
   require(coda)
   require(emdbook)

    dat=data.frame(x=x,y=y,n=50)
    f1 =with(dat, kde2d(x,y,n=n)) 
    f2 =data.frame(expand.grid(x=f1$x, y=f1$y), z=as.vector(f1$z))
  
  return(f2)}

## calculates 2D probabilities
calcProb=function(x,y,prob=c(0.6,0.9)){

   require(coda)
   require(emdbook)

   tmp=HPDregionplot(mcmc(data.frame(x,y)),prob=prob)


   prb=ldply(tmp, function(dat) data.frame(level=dat$level,x=dat$x, y=dat$y))
   
   return(prb)}
 
setMethod("ssn", signature(object="FLStock"),
    function(object) stock.n(object)*exp(-m(object)*(1.0-m.spwn(object))-harvest(object)*(1.0-harvest.spwn(object))))

setGeneric('EP', function(x, ...) standardGeneric('EP'))
setMethod("EP",  signature(x="FLStock"),
    function(x) stock.wt(x)*mat(x))

setGeneric("catchSel", function(object, ...){
	standardGeneric("catchSel")})
setMethod("catchSel", signature(object="FLStock"),
   function(object,fapex=F)  if (fapex) sweep(harvest(object),2:6,fapex(object),"/")
                            else              sweep(harvest(object),2:6,fbar( object),"/"))
                           
setGeneric("landingsSel", function(object, ...){
	standardGeneric("landingsSel")})
setMethod("landingsSel", signature(object="FLStock"),
   function(object,fapex=F) catchSel(object,fapex)*landings.n(object)/(landings.n(object)+discards.n(object)))
   
setGeneric("discardsSel", function(object, ...){
	standardGeneric("discardsSel")})
setMethod("discardsSel", signature(object="FLStock"),
   function(object,fapex=F) catchSel(object,fapex)*discards.n(object)/(landings.n(object)+discards.n(object)))

setGeneric("computeSel", function(x, ...)
  standardGeneric("computeSel"))
setMethod('computeSel', signature(x='FLStock'),
   function(x) {
      ## Get geometric mean
      fn<-function(x) FLQuant(apply(x,c(1,3:6),function(x) exp(mean(log(x),na.rm=T))))

      h   <-fn(harvest(x))
      l   <-fn(landings.n(x)/catch.n(x))
      d   <-1.0-l

      return(FLQuants("harvest"   =h,
                      "landings.n"=l,
                      "discards.n"=d,
                      "catch.n"   =FLQuant(1,dimnames=dimnames(h))))})

setGeneric("sel<-", function(object,value){
	standardGeneric("sel<-")})
setMethod("sel<-", signature(object="FLStock", value="FLQuants"),
	function(object, value) {

   harvest(   object)<-recycleFLQuantOverYrs(harvest(   object),value[["harvest"]])
   catch.n(   object)[]<-NA
   discards.n(object)<-recycleFLQuantOverYrs(discards.n(object),value[["discards.n"]])
   landings.n(object)<-recycleFLQuantOverYrs(landings.n(object),value[["landings.n"]])

   return(object)})


alignSR<-function(x,recAge){
    if (recAge>0)
      sr<-data.frame(Recruits=x[-(1:recAge),               "rec"],
                     SSB     =x[-(dim(x)[2] +1-(1:recAge)),"ssb"])
    else
      sr<-x[,c("Recruits","SSB")]

    return(sr)}

