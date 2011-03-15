## moments
moments<-function(x,n,p=1) (sum(x^p*n)/sum(n))^(1/p)
decade <-function(x) x-(x %% 10)

stdz  <-function(x,na.rm=TRUE) ((x-mean(x,na.rm=na.rm))/sd(x,na.rm=na.rm))
minMax<-function(x,na.rm=TRUE) (x-min(x,na.rm=na.rm))/diff(range(x,na.rm=na.rm))


setGeneric('ssn', function(x, ...) standardGeneric('ssn'))
setMethod("ssn", signature(x="FLStock"),
    function(x) stock.n(x)*exp(-m(x)*(1.0-m.spwn(x))-harvest(x)*(1.0-harvest.spwn(x))))

setGeneric('EP', function(x, ...) standardGeneric('EP'))
setMethod("EP",  signature(x="FLStock"),
    function(x) stock.wt(x)*mat(x))


alignSR<-function(x,recAge){
    if (recAge>0)
      sr<-data.frame(Recruits=x[-(1:recAge),               "rec"],
                     SSB     =x[-(dim(x)[2] +1-(1:recAge)),"ssb"])
    else
      sr<-x[,c("Recruits","SSB")]

    return(sr)}


