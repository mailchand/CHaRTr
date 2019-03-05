plotQuantiles = function(dat) 
{
  qps=seq(.1,.9,.2); 
  nq=length(qps)
  
  dat$q=dat$q[as.character(qps),,]
  
  plot(y=NULL,x=NULL,ylim=range(dat$q),xlim=c(0,1),main=s,ylab="",xlab="")
  
  points(x=rep(dat$p,each=nrow(dat$q)),y=dat$q[,"cor",],pch=4,col="green3",lwd=2)
  
  use=(1-dat$p)>.00   # only plot if more than 2% corrects
  
  points(x=rep(1-dat$p[use],each=nrow(dat$q[,,use])),y=dat$q[,"err",use],pch=4,col="red2",lwd=2);
}



