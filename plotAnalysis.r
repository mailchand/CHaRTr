# fit EAM and UGM to two PHS2005 participants #
#     one that's best fit by EAM, the other by UGM
rm(list=ls())
source("plotUtils.r")
currwd = getwd();

source("chartr-FitRoutines.r")
source("diffusion-EAM-UGM.r")
source("plotUtils.r")

# load C functions
dyn.load("chartr-modelspec.so")
# load RS2002 data sets to find suitable subjects

setwd("Example2/")
# set up plot for predicted distributions from the 3x3 of changing drift rate and changing threshold
par(mfrow=c(1,2),mar=c(4,4,2,1))
qps=seq(.1,.9,.2) ; nq=length(qps)
fnams=dir()
fnams = fnams[1]
data=list()
for(s in fnams) {
  load(s)
  data[[s]]=dat
  plotQuantiles(dat);
}

# 
setwd("../Example2_Fits/")


usemodel  = c("stoneEta","stoneEtaVarTer",
              "stoneEtaUGMintercept","stoneEtaUGMallVar",
              "stoneEtaUrgencyVarTer")

usemodel=c("stoneEta","stoneEtaVarTer",
           "stoneEtaUrgency","stoneEtaUGMallVar",
           "stoneEtaDitterich")


#
snams=names(data) ; nsubj=length(data)
nreps=5
lets=letters[1:nreps]

# for now, only get reobj value (parameters later)
landouts=array(dim=c(nsubj,length(usemodel)),dimnames=list(snams,usemodel))
landpars=list(stone=list(),stoneUGM=list())

for(s in snams) {  
  for(mod in usemodel) {
    # iterate over jobs per subject to get best fit
    bestfit=-Inf ; uselet=NULL
    for(l in lets) {
      fnam=paste(s,mod,l,sep="-")
      if(!file.exists(fnam)) next else load(fnam)
      if(out$reobj>bestfit) {bestfit=out$reobj ; uselet=l}
      rm(fnam,out)
    }
    load(paste(s,mod,uselet,sep="-")); 
    rm(bestfit,uselet)
    
    landouts[s,mod]=out$reobj
    used=paste("data",dat,sep="-") ; 
    usem=paste("model",mod,sep="-")
    landpars[[mod]]=rbind(landpars[[mod]],out$pars)
    rm(out)
  }
}
for(mod in usemodel)
{
  rownames(landpars[[mod]]) = snams
}
setwd("../")



dyn.load("ugm-diffusion.so")
setwd("../")
# model parameters
bailouttime=5
trlsteps=seq(0,bailouttime,.001)
maxiter=as.double(length(trlsteps))
nmc=5e4

# array to hold predictions
nd=length(data[[1]]$p)  # number of drifts
qps=seq(.1,.9,.2) ; nq=length(qps)
predrts=array(dim=c(nq,2,nd,length(usemodel),nsubj),dimnames=list(qps,c("cor","err"),NULL,
                                                                  usemodel,snams))
predps=array(dim=c(nd,length(usemodel),nsubj),dimnames=list(NULL,usemodel,snams))
# predictions for the fitted models
for(mod in usemodel) {
  m=paste("model",mod,sep="-")
  cat("\n   ",m,"\n")
  # loop over nsims landscaping fits
  for(s in snams) {
    cat(" ",s)
    ps=c(unlist(landpars[[mod]][s,]))  # add 0 drift for 0% coherence condition
    
    Ter = ps["Ter"]
    st0 = ps["st0"]
    eta = ps["eta"]
    aU = ps["aU"]
     
    nds=7  # v1 is 0% coherence, will be set to 0 drift in internal function
    
    actualParams = paramsandlims(mod, nds)
    fitUGM = unname(actualParams$fitUGM)
    lowers = actualParams$lowers
    uppers = actualParams$uppers
    parnames = actualParams$parnames
    
    stepsize=ifelse(fitUGM < 0,.001,1)  # time step for diffusion process: .001s (Stone), 1ms (UGM)
    stoch.s=ifelse(fitUGM < 0,.1,100)   # diffusion constant
    timecons=ifelse(fitUGM < 0,0,100)   # time constant for low pass filter
    usign=ifelse(fitUGM < 0,0,1)     

    if(fitUGM < 0) {
      aL=0 ; z=ps["aU"]/2
      h=.001 ; stoch.s=.1 ; usign=0 ; timecons=0
    } else {
      aL=-ps["aU"] ; z=0
      h=1 ; stoch.s=100 ; usign=1 ; timecons=100
    }
    # predictions for fitted parameters
    for(v in 1:nd) {
      rts <- resps <- numeric(nmc)
      
    # Analyse and run each model with the parameters that are present 
      browser()
      
      tmp=diffusionC(v=z$v[N],eta=z$eta,aU=z$aU,aL=z$aL,Ter=z$Ter,
                     intercept=z$intercept, ieta=z$ieta, st0=z$st0,
                     z=z$z, zmin=z$zmin,zmax=z$zmax,timecons_var = z$timecons_var, usign_var = z$usign_var, 
                     sx = z$sx, sy=z$sy, delay = z$delay,lambda = z$lambda, aprime = z$aprime, k = z$k,
                     nmc=nmc,h=stepsize,
                     stoch.s=stoch.s,maxiter=maxiter,fitUGM=fitUGM, timecons=timecons,usign=usign) 
      
      tmp = diffusionC(z=z, v=ps[v], eta=ps["eta"], aU = ps["aU"], aL = aL, Ter=ps["Ter"], 
                       intercept=ps["intercept"], ieta=ps["ieta"], st0 = ps["st0"], zmin=ps["zmin"],
                       zmax=ps["zmax"], timecons_var=ps["timecons_var"], usign_var=ps["usign_var"], sx=ps["sx"], sy=ps["sy"],
                       delay=ps["delay"], lambda=ps["lambda"], aprime=ps["aprime"], k=ps["k"], timecons=timecons, usign=usign, s=stoch.s,h=h,
                       resp=resps,rt=rts,n=nmc,maxiter=maxiter, fitUGM=fitUGM)
      
      
      switch(mod,
             # Same as the vanilla diffusion model, except with a modification of the 
             # model to include some variability in the drift rates.
             stoneEta={
               tmp=.C("stoneEta",z=z,v=ps[v],eta=ps["eta"],aU=ps["aU"],aL=aL,s=stoch.s,h=h,
                      resp=resps,rt=rts,n=nmc,maxiter=maxiter)
                tmp$rt=tmp$rt+ps["Ter"]
               # tmp$rt=tmp$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2)
             },
             
             ratcliffVarTer=
             {
               tmp = .C("ratcliff", zmin=ps["zmin"],zmax=ps["zmax"],v=ps[v],
                        aU=ps["aU"],aL=aL,eta=ps["eta"], s=stoch.s,h=h,resp=resps,rt=rts,n=nmc,maxiter=maxiter);
               tmp$rt=tmp$rt+ runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
               
             },
             stoneEtaVarBase={
               tmp=.C("stoneEtaVarBase",zmin=ps["zmin"],zmax=ps["zmax"],v=ps[v],
                      aU=ps["aU"],aL=aL,eta=ps["eta"], s=stoch.s,h=h,resp=resps,rt=rts,n=nmc,maxiter=maxiter);
               tmp$rt=tmp$rt+ps["Ter"]
             },

             stoneEtaVarTer={
               tmp=.C("stoneEta",z=z,v=ps[v],eta=ps["eta"],aU=ps["aU"],aL=aL,s=stoch.s,h=h,
                      resp=resps,rt=rts,n=nmc,maxiter=maxiter)
                tmp$rt=tmp$rt+ runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
             },
             
             stoneEtaUrgency={
               tmp=.C("stoneEtaUrgency",z=z,v=ps[v],eta=eta,aU=aU,aL=aL,timecons=timecons,usign=usign,
                      intercept=ps["intercept"],ieta = ps["ieta"],usign_var = ps["usign_var"],s=stoch.s,h=h,
                      resp=resps,rt=rts,n=nmc,maxiter=maxiter);
               tmp$rt=(tmp$rt/1000)+ps["Ter"]
             },
             
             stoneEtaUrgencyVarTer={
               tmp=.C("stoneEtaUrgency",z=z,v=ps[v],eta=eta,aU=aU,aL=aL,timecons=timecons,usign=usign,
                      intercept=ps["intercept"],ieta = ps["ieta"],usign_var = ps["usign_var"],s=stoch.s,h=h,
                      resp=resps,rt=rts,n=nmc,maxiter=maxiter);
               tmp$rt=(tmp$rt/1000) + runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
             },
             
             stoneEtaUrgencySimple={
               tmp=.C("stoneEtaUrgencySimple",z=z,v=ps[v],eta=eta,aU=aU,aL=aL,timecons=timecons,usign=usign,
                      intercept=ps["intercept"],usign_var = ps["usign_var"],s=stoch.s,h=h,
                      resp=resps,rt=rts,n=nmc,maxiter=maxiter);
               tmp$rt=(tmp$rt/1000)+ps["Ter"]
             },
             
             stoneEtaUGMintercept={
               tmp=.C("stoneEtaUGMintercept",z=z,v=ps[v],eta=ps["eta"],aU=ps["aU"],aL=aL,timecons=timecons,
                                usign=usign,intercept=ps["intercept"], s=stoch.s,h=h,resp=resps,rt=rts,n=nmc,maxiter=maxiter)
               tmp$rt=(tmp$rt/1000)+ps["Ter"]
                      #   tmp$rt=(tmp$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2)
               
             },
             stoneEtaUGMinterceptVarTer={
               tmp=.C("stoneEtaUGMintercept",z=z,v=ps[v],eta=ps["eta"],aU=ps["aU"],aL=aL,timecons=timecons,
                      usign=usign,intercept=ps["intercept"], s=stoch.s,h=h,resp=resps,rt=rts,n=nmc,maxiter=maxiter)
               tmp$rt=(tmp$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
               #   tmp$rt=(tmp$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2)
               
             },
             stoneEtaUGMallVar={
               out=.C("stoneEtaUGMallVar",z=z,v=ps[v],eta=ps["eta"],aU=ps["aU"],aL=aL,timecons = timecons, usign=usign, intercept=ps["intercept"],
                      ieta=ps["ieta"],timecons_var=ps["timecons_var"],
                      usign_var=ps["usign_var"], s=stoch.s,h=h, resp=resps,rt=rts,n=nmc,maxiter=maxiter);
               tmp = out;
               tmp$rt=(tmp$rt/1000) + runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
             },
             stoneEtaDitterich={
               
               tmp=.C("stoneEtaDitterich",z=z,v=ps[v],eta=ps["eta"], delay=ps["delay"], sx=ps["sx"], sy=ps["sy"], 
                      aU=ps["aU"],aL=aL,s=stoch.s,h=h,resp=resps,rt=rts,n=nmc,maxiter=maxiter)
               tmp$rt=(tmp$rt)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
             },
             stoneEtaCollapse={
               
               tmp=.C("stoneEtaCollapse",z=z,v=ps[v],eta=ps["eta"], lambda=ps["lambda"],  
                      aU=ps["aU"],aL=aL, aprime=ps["aprime"], k=ps["k"],
                      s=stoch.s,h=h,resp=resps,rt=rts,n=nmc,maxiter=maxiter)
               tmp$rt=(tmp$rt)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
             }
      )      
      predps[v,mod,s]=(table(tmp$resp)/nmc)["1"]
      predrts[,"cor",v,mod,s]=quantile(tmp$rt[tmp$resp==1],probs=qps)
      predrts[,"err",v,mod,s]=quantile(tmp$rt[tmp$resp==2],probs=qps)
    }
  }
}


savefigs=F

# plot predicted values
if(savefigs) pdf(file="landscaping/Figures/EAM-UGM-predictions-RS2002.pdf",width=6,height=6)
usesubj=fnams 
snamplot=usesubj ; names(snamplot)=usesubj
lwds=2.5
ptcexs=1.4
axcexs=.9
labcexs=1.1
xaxs=seq(0,1,.2) ; xaxs[xaxs<1]=gsub("0.",".",xaxs[xaxs<1])
if(savefigs) {
  greencol=rgb(red=0,green=170,blue=0,alpha=210,maxColorValue=255)
  redcol=rgb(red=238,green=0,blue=0,alpha=190,maxColorValue=255)
  graycol=rgb(red=169,green=169,blue=169,alpha=180,maxColorValue=255)
} else { greencol="green4" ; redcol="red2" ; graycol="gray" }
par(mar=rep(0,4))
figLayout = rbind(0,c(0,1,0,2,0,3,0,4,0,5,0),0,c(0,6,0,7,0,8,0,9,0,10,0),0)
# layout(figLayout,h=c(.4,1,.1,1,.32),w=c(.5,1,.1,1,.05))
layout(figLayout, h=c(.1,1,.1,1,.1), w=c(0.1,1,0.1,1,0.1,1,0.1,1,0.1,1,0.1))
# layout.show(n=8)


for(s in usesubj) {
  dat=data[[s]] ; dat$q=dat$q[as.character(qps),,]
  # different y axis scaling for different subjects
  yaxs=seq(.3,1.2*round(max(predrts[1:5,1:2,1:5,1:1,s]),2),.2)
  #  if(s=="e9mk") yaxs=seq(.4,1.2,.2) else yaxs=seq(.4,1.6,.2)
  yaxs[yaxs<1]=gsub("0.",".",yaxs[yaxs<1])
  for(mod in usemodel) {
    # plot(y=NULL,x=NULL,ylim=c(as.numeric(min(yaxs))-.1,as.numeric(max(yaxs))+.1),
    plot(y=NULL,x=NULL,ylim=c(0.2,1.2*max(predrts[1:5,1:2,1:5,1:1,s])),
         xlim=c(0,1),ylab="",xlab="",axes=F)
    axis(side=1,at=xaxs,NA,tcl=-.3)
    axis(side=2,at=yaxs,NA,tcl=-.3)
    text(x=1.05,y=max(as.numeric(yaxs)),labels=round(landouts[s,mod]),pos=2,cex=labcexs*1.2)
    if(mod=="stoneEta") {
      mtext(side=2,snamplot[s],cex=labcexs*1.15,line=6.2,las=1)
      mtext(side=2,at=yaxs,yaxs,line=.8,cex=axcexs,las=1)
      if(which(s==usesubj)==2) {
        mtext(side=2,"Response Time (seconds)",cex=labcexs,line=3,at=max(as.numeric(yaxs))+.15)
      }
    }
    if(which(s==usesubj)==1) {
      if(mod=="stoneEta") mtext(side=3,"Fitting Model",cex=labcexs*1.3,line=4,at=1.1)
      mtext(side=3,switch(mod,"stoneEta"="EAM","stoneEtaUGM"="UGM"),cex=labcexs*1.15,line=1)
    } else {
      mtext(side=1,at=xaxs,xaxs,line=.8,cex=axcexs,las=1)
      if(mod=="stoneEtaUGMslope") mtext(side=1,"Probability Correct",cex=labcexs,line=3.5,at=1.1)
    }
    # plot predicted quantiles from fit
    use=(1-predps[,mod,s])>.01  # only plot if more than X% corrects
    for(i in 1:nq) points(y=c(rev(predrts[i,"err",use,mod,s]),predrts[i,"cor",,mod,s]),
                          x=c(rev(1-predps[use,mod,s]),predps[,mod,s]),pch=19,col=graycol, 
                          bg=graycol,cex=ptcexs,type="b",lwd=lwds*1.2)
    #    points(y=predrts[,"cor",,mod,s],x=rep(predps[,mod,s],each=nq),pch=1,col=graycol,cex=ptcexs)
    #    points(y=predrts[,"err",use,mod,s],x=1-rep(predps[use,mod,s],each=nq),pch=1,col=graycol,cex=ptcexs)
    # overlay data
    points(x=rep(dat$p,each=nrow(dat$q)),y=dat$q[,"cor",],pch=4,col=greencol,lwd=lwds,cex=ptcexs)
    use=(1-dat$p)>.00   # only plot if more than X% corrects
    points(x=rep(1-dat$p[use],each=nrow(dat$q[,,use])),y=dat$q[,"err",use],pch=4,col=redcol,lwd=lwds,cex=ptcexs)
  }
}
if(savefigs) dev.off()


setwd(currwd)
