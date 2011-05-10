xCord    <-function(i) floor(i/2)*2-i+2
yCord    <-function(i) floor((i+1)/2)
vplayout <-function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)

#### Line thru 25t & 75th percentiles
qqLine<-function(x,y){
   qtlx<-quantile(x,prob=c(0.25,0.75),na.rm=T)
   qtly<-quantile(y,prob=c(0.25,0.75),na.rm=T)

   a=(qtly[1]-qtly[2])/(qtlx[1]-qtlx[2])
   b=qtly[1]-qtlx[1]*a

   res<-c(a,b)
   names(res)<-NULL
   names(res)<-c("a","b")

   return(x*res["a"]+res["b"])}

## adds QQ and AR1 data
QQAR<-function(object){
   residualLag  =c(object$residual[-1],NA)
   qq.          =qqnorm(c(stdz(object$residual)),plot.it=FALSE)
   qqx          =qq.$x
   qqy          =qq.$y

   res<-data.frame(object,residualLag=residualLag,qqx=qqx,qqy=qqy,qqHat=qqLine(qqx,qqy))

   return(res)}

setGeneric("plotDiags", function(object,...){
	standardGeneric("plotDiags")})
#### Diagnostics
setMethod("plotDiags", signature(object="data.frame"),
t.<-  function(object,col="#CCFF33",group=1,stuff=facet_wrap(~age,scale="free"),plots=c(1,2,3,4,5,6)){

  p<-ggplot(data.frame(object,col=col,group=group))

  plot1<-function(i,p,stuff){
      qqpar<-qqLine(p$data$qqx,p$data$qqy)
      p1<-p + geom_point(aes(qqx,qqy,col=col,group=group),size=2.5)   +
                        opts(title = "Normal Q-Q Plot") + scale_x_continuous(name="Theoretical Quantiles", scale="free") +
                                                          scale_y_continuous(name="Sample Quantiles"     , scale="free") +
                        geom_abline(intercept=qqpar["b"], slope=qqpar["a"]) + stuff
      print(p1, vp=vplayout(xCord(i),yCord(i)))}

  plot2<-function(i,p,stuff){
      #plotFunc(obs,prd,indVar,indVar.,xttl=Xttl,yttl=Yttl,mttl="FunctionalForm",splt=c(1,1,2,3),more=TRUE)
      p2<-p+geom_point(aes(obs,hat,col=col,group=group))+geom_line(aes(x,yHat,col=col,group=group)) + stuff
      print(p2, vp=vplayout(xCord(i),yCord(i)))}

  plot3<-function(i,p,stuff){
      #plotResidYr(resid,xttl="Year",yttl='Residuals',mttl="",splt=c(2,1,2,3),more=TRUE)
      p3<-p+geom_point(aes(year,rsdl,col=col,group=group))+stat_smooth(aes(year,rsdl,col=col,group=group)) +
               scale_x_continuous(name="Year") + scale_y_continuous(name=expression(Residuals)) + stuff +geom_abline(intercept=0, slope=0)
      print(p3, vp=vplayout(xCord(i),yCord(i)))}

  plot4<-function(i,p,stuff){
      #plotResidAR1(resid,xttl='Residualsatt',yttl='Residualsatt+1',mttl='AR(1)Residuals',splt=c(1,2,2,3),more=TRUE)
      p4<-p+geom_point(aes(rsdl,rsdlLag,col=col,group=group))+stat_smooth(aes(rsdl,rsdlLag,col=col,group=group)) +
              stuff + geom_abline(intercept=0, slope=01)
      print(p4, vp=vplayout(xCord(i),yCord(i)))}

  plot5<-function(i,p,stuff){
      #plotResidX(resid,indVar,xttl=Xttl,yttl='Residuals',mttl="ResidualsbyIndvar",splt=c(2,2,2,3),more=TRUE)
      p5<-p+geom_point(aes(obs,rsdl,col=col,group=group))+stat_smooth(aes(x,rsdl,col=col,group=group))  +
            scale_x_continuous(name="Index") + scale_y_continuous(name=expression(Residuals)) +
             stuff + geom_abline(intercept=0, slope=0)
      print(p5, vp=vplayout(xCord(i),yCord(i)))}

  plot6<-function(i,p,stuff){
      #plotResidX(resid,hat,xttl=paste(Yttl,"Hat"),yttl='Residuals',mttl="ResidualsbyHat",splt=c(1,3,2,3),more=TRUE)
      p6<-p+geom_point(aes(hat,rsdl,col=col,group=group))+stat_smooth(aes(yHat,rsdl,col=col,group=group))  +
            scale_x_continuous(name=expression(Residuals[t])) + scale_y_continuous(name=expression(Residuals)) +
             stuff + geom_abline(intercept=0, slope=0)
      print(p6, vp=vplayout(xCord(i),yCord(i)))}

  for (i in 1:length(plots)){
    switch(ac(plots[i]),
           "1"=plot1(i,p,stuff),
           "2"=plot2(i,p,stuff),
           "3"=plot3(i,p,stuff),
           "4"=plot4(i,p,stuff),
           "5"=plot5(i,p,stuff),
           "6"=plot6(i,p,stuff))}
    })


