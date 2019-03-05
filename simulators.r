# 
#
#
#
simulateRTs = function(model, ps, nmc=50000, maxiter=10000, nds=7, showAxisLabels = FALSE, plotData=TRUE){
  # model specifies the model to simulate
  # ps passes in parameters
  # nmc and maxiter are parameters for the simulation
  
  bailouttime=5
  trlsteps=seq(0,bailouttime,.001)
  maxTimeStep=as.double(length(trlsteps))
  
  
  lwds=2.5
  ptcexs=1.4
  axcexs=.9
  labcexs=1.1
  
  Ter = ps["Ter"]
  st0 = ps["st0"]
  eta = ps["eta"]
  aU = ps["aU"]
  
  qps=seq(.1,.9,.1) ; 
  nq=length(qps)
  
  dNameRows = c("cor", "err");
  dNameCols = seq(1,nds,1);
  
  predps=array(dim=c(nds),dimnames=list(NULL))
  predrts=array(dim=c(nq,2,nds),dimnames=list(qps,c("cor","err"),NULL))
  counts = array(dim=c(2,nds), dimnames=list(dNameRows, dNameCols));
  pb = array(data=NA, dim = c(10,2,nds), dimnames=list(1:10, dNameRows, dNameCols))
  
  
  actualParams = paramsandlims(model, nds)
  fitUGM = unname(actualParams$fitUGM)
  lowers = actualParams$lowers
  uppers = actualParams$uppers
  parnames = actualParams$parnames
  
  stepsize=ifelse(fitUGM < 0,.001,1)  # time step for diffusion process: .001s (DDM), 1ms (UGM)
  stoch.s=ifelse(fitUGM < 0,.1,100)   # diffusion constant
  timecons=ifelse(fitUGM < 0,0,100)   # time constant for low pass filter
  usign=ifelse(fitUGM < 0,0,1)
  
  if(fitUGM < 0) {
    aL=0 ; z=ps["aU"]/2
    dt=.001 ; stoch.s=.1 ; usign=0 ; timecons=0
  } else {
    aL=-ps["aU"] ; 
    z=0
    dt=1 ; 
    stoch.s=100; 
    usign=1; 
    timecons=100
    
  }
  graycol=rgb(red=80,green=80,blue=80,alpha=180,maxColorValue=255)
  graylinecol =rgb(red=20,green=20,blue=20,alpha=180,maxColorValue=255)
  
  
  # predictions for fitted parameters
  backcol=rgb(red=220,green=220,blue=220,alpha=180,maxColorValue=255)
  
  if(plotData)
  { 
    par(pty="s")
    plot(y=NULL,x=NULL,ylim=c(0.2,0.9),
         xlim=c(0,1),ylab="",xlab="",axes=F)
    xaxs=seq(0,1,.2) ; xaxs[xaxs<1]=gsub("0.",".",xaxs[xaxs<1])
    
    axis(side=1,at=xaxs,NA,tcl=-.4, labels=NULL, pos=0.25, cex.axis=1.6)
    
    if(showAxisLabels){
      mtext("Reaction time (sec)", side=2, line=3 , cex=1.5)
      mtext("Probability Correct ", side=1, line=3 , cex=1.5)
    }
  }
  
  for(v in 1:nds) {
    rts <- resps <- numeric(nmc)
    
    tmp = diffusionC(v=ps[v], eta=ps["eta"], aU = ps["aU"], aL = aL, Ter=ps["Ter"], 
                     intercept=ps["intercept"], ieta=ps["ieta"], st0 = ps["st0"], z=z, zmin=ps["zmin"],
                     zmax=ps["zmax"], timecons_var=ps["timecons_var"], usign_var=ps["usign_var"], 
                     sx=ps["sx"], sy=ps["sy"], delay=ps["delay"], lambda=ps["lambda"], 
                     aprime=ps["aprime"], k=ps["k"], timecons=timecons, usign=usign, s=stoch.s,dt=dt,
                     n=nmc,maxTimeStep=maxTimeStep, fitUGM=fitUGM)
    
   
    # Now count the number of good and bad numbers
    counts["cor",v] = sum(tmp$resp==1)
    counts["err",v] = sum(tmp$resp==2)
    
    predps[v]=(table(tmp$resp)/nmc)["1"]
    predrts[,"cor",v]=quantile(tmp$rt[tmp$resp==1],probs=qps)
    predrts[,"err",v]=quantile(tmp$rt[tmp$resp==2],probs=qps)
    
    pb[1:10,1,v] = counts[1,v]/10; # Divide by 10 because you have quantiles :)!
    pb[1:10,2,v] = counts[2,v]/10;
    
  }
  # If you want the data plotted on the 
  if(plotData)
  {
    for(i in seq(1,nq,2)) {
      y=c(rev(predrts[i,"err",]),predrts[i,"cor",])
      x=c(rev(1-predps),predps)

      points(y=y,x=x,pch=21,col=graylinecol,bg=backcol, cex=ptcexs*2,type="b",
             lwd=lwds*1.2)
      
      # text(0.5, 0.96*predrts[i,"err",1], qps[i]*100,col=graycol)
    }
    
    yaxs=seq(.3,max(c(round(1.25*max(predrts),digits=1),0.9)),.2)
    text(0.15, yaxs[length(yaxs)]-0.04, model,cex=1.5, pos=4)
    axis(side=2,at=yaxs,NA,tcl=-.4, labels=NULL, las=1, cex.axis=1.6)
  }
  list(q=predrts, p=predps, n=counts, pb = pb)
}

plotData = function(dat, ExistingPlot=FALSE) {
  qps=seq(.1,.9,.2) ; nq=length(qps)
  dat$q=dat$q[as.character(qps),,]
  if(!ExistingPlot)
  {
    plot(y=NULL,x=NULL,ylim=range(dat$q),xlim=c(0,1),ylab="",xlab="", frame.plot = FALSE )
    yaxs=seq(.3,round(1.2*max(dat$q),digits=1),.2)
  }
  points(x=rep(dat$p,each=nrow(dat$q)),y=dat$q[,"cor",],pch=4,col="green3",lwd=2)
  use=(1-dat$p)>.01   # only plot if more than 2% corrects
  points(x=rep(1-dat$p[use],each=nrow(dat$q[,,use])),y=dat$q[,"err",use],pch=4,col="red2",lwd=2)
}


