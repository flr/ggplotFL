#x="/home/lkell/Dropbox/ICCAT/SCRS/WHM/section_6/Inputs/SS3/posteriors.sso"

mcmcSS3=function(x,start,end,thin,nrows=-1){
  
   hd=names(read.csv(x,sep=" ",nrows=1,header=T))

   dat = laf_open_csv(filename=x,column_types=rep("double",length(hd)),
                                 column_names=hd,
                                 sep=" ",
                                 skip=2)
  
   res <- dat[ ,]
    
   #res=read.table(x,sep="",colClasses="numeric",nrows=nrows,header=T)
   res=mcmc(res,start=min(res$Iter),
               end  =max(res$Iter),
               thin =unique(diff(res$Iter))[1])
  
  return(res)}
