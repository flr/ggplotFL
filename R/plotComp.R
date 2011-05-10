whooow  <-function(x,fn,probs) as.data.frame(FLQuants(lapply(fn, function(fn,x) quantile(fn(x), probs=probs, na.rm=T), x=x)))

plotComp<-function(x,fn=NULL,probs=c(0.95,0.50,0.05),size=c(0.5,1.0,0.5),lty=c(2,1,2),facet=facet_wrap(~qname,scale="free")){

   res   <-whooow(x,fn,probs)

   p1<-ggplot(res)+ geom_line(aes(x=year,y=data,group=iter,size=iter,lty=iter)) +
                    scale_size_manual(    values=size, name="Quantile") +
                    scale_linetype_manual(values=lty , name="Quantile") +
                    expand_limits(y = 0)                                +
                    xlab("Year") + ylab("")                             +
                    facet

   print(p1)
   invisible(p1)}

plotComps<-function(x,fn=NULL,probs=c(0.95,0.50,0.05),size=c(0.5,1.0,0.5),lty=c(2,1,2),facet=facet_wrap(~qname,scale="free")){

   res     <-ldply(x, whooow, fn=fn, probs=probs)
   res$.id <-factor(res$.id)
   res$iter<-factor(res$iter)

   p1<-ggplot(res)+ geom_line(aes(x=year,y=data,group=.id:iter,size=iter,col=.id)) +
                    scale_size_manual(    values=size, name="Quantile") +
                    scale_linetype_manual(values=lty , name="Quantile") +
                    expand_limits(y = 0)                                +
                    xlab("Year") + ylab("")                             +
                    facet

   print(p1)
   invisible(p1)}

setMethod("plot", signature(x="FLStock", y="missing"),
 function(x,probs=c(0.95,0.50,0.05),size=c(0.5,1.0,0.5),lty=c(2,1,2),facet=facet_wrap(~qname,scale="free"),
                       fn=list("SSB"       =ssb,
                               "Recruits"  =rec,
                               "Plus Group"=function(x) stock.n(x)[ac(dims(x)$max)],
                               "Fpg"       =function(x) harvest(x)[ac(dims(x)$max)],
                               "F2:5"      =function(x) apply(harvest(x)[ac(2:5)],2,mean)),...)

    plotComp(x,fn,probs,size,lty,facet))

setMethod("plot", signature(x="FLStocks", y="missing"),
  function(x,probs=c(0.95,0.50,0.05),size=c(0.5,1.0,0.5),lty=c(2,1,2),facet=facet_wrap(~qname,scale="free"),
                       fn=list("SSB"       =ssb,
                               "Recruits"  =rec,
                               "Plus Group"=function(x) stock.n(x)[ac(dims(x)$max)],
                               "Fpg"       =function(x) harvest(x)[ac(dims(x)$max)],
                               "F2:5"      =function(x) apply(harvest(x)[ac(2:5)],2,mean)),...)

    plotComps(x,fn,probs,size,lty,facet))
