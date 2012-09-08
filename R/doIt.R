doDiags=function(runs,dir,themeMy=theme_ms(8)){
  
  require(stringr)
  
  dgFiles=paste(dir,"Run_",runs,"/MINUS0.R",sep="")  
  dgs=transform(mdply(dgFiles, readVpa2boxDiags),Run =factor(runs)[X1],
                cpue=str_trim(cpue))[,-1]
  
  cpues=dgs[with(dgs, !duplicated(cbind(cpue,year))),]
  
  f1=ggplot(dgs)+
    geom_point( aes(year,obs,col=cpue)) +
    geom_line(  aes(year,obs,col=cpue)) +
    stat_smooth(aes(year,obs),           se=F,size=1) +
    facet_wrap(~cpue,scales="free_y",ncol=2) + 
    ylab("CPUE") + xlab("Year") + themeMy +
    opts(legend.position="none")
  
  f2=plotmatrix(cast(subset(cpues, select=c(cpue,year,obs)), year~cpue, value="obs"))
  f2$layers[[2]]=NULL
  f2$data=ddply(subset(f2$data, !is.na(x) & !is.na(y)), .(xvar,yvar), transform, x=stdz(x), y=stdz(y))
  f2 = f2 + stat_smooth(method="lm")+geom_hline(aes(yintercept=0),colour="red")+themeMy +
    xlab("")+ylab("") 
  
  f3=ggplot(dgs)+
    geom_point( aes(year,obs,group=cpue,colour=cpue)) +
    geom_line(  aes(year,obs,group=cpue,colour=cpue)) +
    stat_smooth(aes(year,obs,group=cpue),           se=F,size=1) +
    facet_grid(Run~cpue,scales="free",space="free") +
    ylab("CPUE") + xlab("Year") + themeMy +
    scale_x_continuous(breaks=c(1960,1970,1980,1990,2000))  +
    opts(legend.position="none")
  
  f4=ggplot(aes(year,residual,col=cpue),data=dgs) +
    geom_point() + 
    facet_grid(Run~cpue,scales="free",space="free") +
    stat_smooth() + 
    themeMy   + 
    ylab("Residual") + xlab("Year") +
    geom_hline(aes(yintercept=0))
  
  f5=ggplot(aes(hat,residual,col=cpue),data=dgs) +
    geom_point() + 
    #geom_errorbar(aes(hat,max=residual,min=0)) + 
    facet_grid(Run~cpue,scales="free") +
    stat_smooth() + 
    themeMy   + 
    ylab("Residual") + xlab("Fitted") +
    geom_hline(aes(yintercept=0))
  
  f6=ggplot(dgs) + 
    geom_line(aes(qqx,qqHat))+ 
    geom_point(aes(qqx,qqy,col=cpue)) + 
    facet_grid(Run~cpue,scales="free") +
    ylab("Data Quantiles") + xlab("Normal Theoretical Quantiles") + 
    themeMy
  
  dgs.=ddply(dgs,.(cpue,Run), transform, residual   =(residual   -min(c(residual,residualLag),na.rm=T))/diff(range(c(residual,residualLag), na.rm=T))-mean(c(residual,residualLag),na=T),
             residualLag=(residualLag-min(c(residual,residualLag),na.rm=T))/diff(range(c(residual,residualLag), na.rm=T))-mean(c(residual,residualLag),na=T))
  f7=ggplot(dgs.)+geom_point(aes(residual,residualLag,col=cpue)) +
    facet_grid(Run~cpue) + 
    stat_smooth(aes(residual,residualLag),method="lm") + 
    themeMy + 
    ylab(expression(Residual[t+1])) + xlab(expression(Residual[t])) +
    geom_hline(aes(yintercept=V1), data=ddply(dgs.,.(Run,cpue),function(x) mean(x$residualLag,na.rm=T)))
  
  f8=ggplot(dgs)+
    geom_point( aes(hat,obs,group=cpue,colour=cpue)) +
    stat_smooth(aes(hat,obs,group=cpue),se=F,method="lm",colour="blue") +
    geom_abline(intercept = 0, slope =1) +
    facet_grid(cpue~Run,scales="free_y") + 
    xlab("Predicted") + ylab("Observed") + themeMy +
    scale_x_continuous(breaks=NULL)  +
    scale_y_continuous(breaks=NULL)  
  
  return(list("1"=f1,"2"=f2,"3"=f3,"4"=f4,"5"=f5,"6"=f6,"7"=f7,"8"=f8))}



doRetros=function(runs,dir="",plotFn=list("SSB"=ssb, "Recruits"=rec, 
                                         "F2:5"=function(x) apply(harvest(x)[2:5],2,function(x) exp(mean(log(x)))), 
                                         "F10+"=function(x) apply(harvest(x)[10 ],2,mean),
                                         "N10+"=function(x) stock.n(x)[10]),
                 fileFn=function(dir,runs) paste(dir,"Run_",runs,"/adapt.c1",sep=""),
                 m=NULL,
                 themeMy=theme_ms(8)){
  
  require(stringr)
  
  rnFiles=fileFn(runs,dir)
  
  # get runs
  if (is.null(m)) rns=mlply(rnFiles,readVpa2box) else
    rns=mlply(rnFiles,readVpa2box,m=m)
  
  # plots by run
  plts=ldply(rns, function(x)  plot(x, fn=plotFn)$data)
  plts=transform(plts, Run  =factor(runs)[X1], 
                 Retro=factor(2011-as.numeric(as.character(.id)),levels=rev(2011-as.numeric(as.character(.id)))))
  
  plts=ddply(plts,.(qname,Run,Retro), transform, data.frame(data=data/max(data)))
  sel=ldply(rns, function(x) as.data.frame(catch.sel(brp(FLBRP(x[[1]]))),drop=T))
  sel=transform(sel, Run=factor(runs)[X1])[,-1]
  
  ## retro plots
  f1=ggplot(subset(plts,Retro %in% 2007:2011))+
    geom_line(aes(year,data,group=Run,colour=Run),size=.5)+
    facet_grid(qname~Retro,scale="free")+
    scale_x_continuous(limits=c(1950,2012),breaks=c(1960,1980,2000))+
    scale_y_continuous(                    breaks=NULL)+
    themeMy + xlab("Year") + ylab("")
  
  f2=ggplot(subset(plts,Retro %in% 2007:2011))+
    geom_line(aes(year,data,group=Retro,colour=Retro),size=.5)+
    facet_grid(qname~Run,scale="free")+
    scale_x_continuous(limits=c(1950,2012),breaks=c(1960,1980,2000))+
    scale_y_continuous(                    breaks=NULL)+
    themeMy + xlab("Year") + ylab("")
  
  f3=ggplot(sel)+
    geom_line(aes(age,data,col=Run,group=Run))+
    themeMy+xlab("Age")+ylab("Selectivity")           
  
  td=ddply(subset(plts[,c("qname","Run","Retro","year","data")],year>2008), .(Run,Retro,qname), transform, data=stdz(data))
  td=cast(td,Retro+qname+year~Run,value="data")
  
  return(list(retro1=f1,retro2=f2,
              sel   =f3,
              td    =td))}
