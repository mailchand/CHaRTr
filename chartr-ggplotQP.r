# fit EAM and UGM to two PHS2005 participants #
#     one that's best fit by EAM, the other by UGM
rm(list=ls())

currwd = getwd();
source("chartr-FitRoutines.r")
source("chartr-helperfunctions.r")
source("plotUtils.r")
source("modelSelection.r")

# load C functions
dyn.load("chartr-modelspec.so")
# load RS2002 data sets to find suitable subjects

setwd("Example2/")
# set up plot for predicted distributions from the 3x3 of changing drift rate and changing threshold
par(mfrow=c(1,2),mar=c(4,4,2,1))
qps=seq(.1,.9,.2) ; nq=length(qps)
fnams=dir()
# fnams = fnams[c(1,2,3)]
fnams = fnams[c(1,3)]
data=list()
for(s in fnams) {
  load(s)
  data[[s]]=dat
  plotQuantiles(dat);
}

# 
setwd("../Example2_Fits/");


usemodel  = c("DDM","DDMSvSt","cDDMSvSt","dDDMSvSt","bUGMSvSbSt")
modelLabels = c("DDM","DDMS[v]S[t]","cDDMS[v]S[t]","dDDMS[v]S[t]", "bUGMS[v]S[b]S[t]")
#
snams=names(data) ; 
nsubj=length(data);
nreps=5;
lets=letters[1:nreps];

# for now, only get reobj value (parameters later)
landouts=array(dim=c(nsubj,length(usemodel)),dimnames=list(snams,usemodel))
landAIC=array(dim=c(nsubj,length(usemodel)),dimnames=list(snams,usemodel))

landpars=list(stone=list(),stoneUGM=list())
numpars = array(length(usemodel));
modelOutput=list()

# Load each and every model, check which model fit is best. Use best fitting model
#
#
#
for(s in snams) {  
  for(mod in usemodel) {
    # iterate over jobs per subject to get best fit
    bestfit=-Inf ; uselet=NULL
    for(l in lets) {
      fnam=paste(s,mod,l,sep="-")
      if(!file.exists(fnam)) next else load(fnam)
      if(out$reobj>bestfit) 
      {
        bestfit=out$reobj ; 
        uselet=l
        modelOutput[[mod]]=out;
      }
      rm(fnam,out)
    }
    load(paste(s,mod,uselet,sep="-")); 
    
    
    landouts[s,mod]=out$reobj
    used=paste("data",dat,sep="-") ; 
    usem=paste("model",mod,sep="-")
    landpars[[mod]]=rbind(landpars[[mod]],out$pars)
    numpars[mod] = length(out$pars)
    landAIC[s,mod] = calcIC(bestfit, numpars[mod], sum(data[[s]]$n))
    
    
    rm(out)
    rm(bestfit,uselet)
    
    # rownames(landpars[[mod]]) = snams[s]
  }
}

for(mod in usemodel) {
  rownames(landpars[[mod]]) = snams
}

setwd("../")
# End loading 
ms=modelSelection(models=modelOutput, data=dat)

# Now simulate data using the existing parameters from these models.

bailouttime=5
trlsteps=seq(0,bailouttime,.001)
maxTimeStep=as.double(length(trlsteps))
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
    
    # v1 is 0% coherence, will be set to 0 drift in internal function
    
    actualParams = paramsandlims(mod, nd)
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
      dt=.001 ; stoch.s=.1 ; usign=0 ; timecons=0
    } else {
      aL=-ps["aU"] ; z=0
      dt=1 ; stoch.s=100 ; usign=1 ; timecons=100
    }
    # predictions for fitted parameters
    for(v in 1:nd) {
      rts <- resps <- numeric(nmc)
      
      # Analyse and run each model with the parameters that are present 
      tmp = diffusionC(v=ps[v], eta=ps["eta"], aU = ps["aU"], aL = aL, Ter=ps["Ter"], 
                       intercept=ps["intercept"], ieta=ps["ieta"], st0 = ps["st0"], z=z, zmin=ps["zmin"],
                       zmax=ps["zmax"], timecons_var=ps["timecons_var"], usign_var=ps["usign_var"], 
                       sx=ps["sx"], sy=ps["sy"], delay=ps["delay"], lambda=ps["lambda"], 
                       aprime=ps["aprime"], k=ps["k"], timecons=timecons, usign=usign, s=stoch.s,dt=dt,
                       n=nmc,maxTimeStep=maxTimeStep, fitUGM=fitUGM)
      predps[v,mod,s]=(table(tmp$resps)/nmc)["1"]
      predrts[,"cor",v,mod,s]=quantile(tmp$rt[tmp$resps==1],probs=qps)
      predrts[,"err",v,mod,s]=quantile(tmp$rt[tmp$resps==2],probs=qps)
    }
  }
}




usesubj=fnams 
snamplot=usesubj ; names(snamplot)=usesubj

# Setup some simple parameters for plotting
lwds=2.5
ptcexs=2
axcexs=1.1
labcexs=1.1
dataMarker=4;
modelFitMarker=21;

# xaxes go from 0 to 1, make sure they show as . instead of 0.
xaxs=seq(0,1,.2) ; xaxs[xaxs<1]=gsub("0.",".",xaxs[xaxs<1])

# Define colors for plotting
bluecol=rgb(red=30,green=144,blue=255,alpha=210,maxColorValue=255)
yellowcol=rgb(red=255,green=165,blue=0,alpha=190,maxColorValue=255)
graycol=rgb(red=79,green=79,blue=79,alpha=180,maxColorValue=255)
graybgcol=rgb(red=129,green=129,blue=129,alpha=180,maxColorValue=255)


# Set margins and figure layout. We have chosen five models to be plotted in this example, so 
par(mfrow=c(length(usesubj),length(usemodel)))
par(mar=c(10,6,10,4))

for(s in usesubj) {
  dat=data[[s]] ; dat$q=dat$q[as.character(qps),,]
  # different y axis scaling for different subjects
  
  yaxs=seq(.2,1.2*round(max(predrts[1:5,1:2,1:5,1:1,s]),2),.2)
  yaxs[yaxs<1]=gsub("0.",".",yaxs[yaxs<1])
  for(mod in usemodel) {
    plot(y=NULL,x=NULL,ylim=c(0.2,1.2*max(predrts[1:5,1:2,1:5,1:1,s])),
         xlim=c(0,1),ylab="",xlab="",axes=F)
    
    axis(side=1,at=xaxs,NA,tcl=-.3, lwd=2)
    axis(side=2,at=yaxs,NA,tcl=-.3, lwd=2)
    
    text(x=1.05,y=max(as.numeric(yaxs)),labels=round(landAIC[s,mod]-landAIC[s,usemodel[1]]),pos=2,cex=labcexs*1.2)
    
    if(which(s==usesubj)==1)
      mtext(side=3,mod,cex=labcexs*0.8,line=-2,las=1, at=0.3)
    mtext(side=2,at=yaxs,yaxs,line=.8,cex=axcexs,las=1)
    mtext(side=1,at=xaxs,xaxs,line=.8,cex=axcexs,las=1)
    
    if(mod==usemodel[1]){
      mtext(side=2,"Response Time (seconds)",cex=labcexs,line=3, at=0.5);
      mtext(side=3, s, cex=labcexs*1.2, line=0, at=0, col="blue")
    }
    if(mod==usemodel[ceiling(length(usemodel)/2)]) 
      mtext(side=1,"Probability Correct",cex=labcexs,line=3.5,at=.5)
    
    # plot predicted quantiles from fit
    use=(1-predps[,mod,s])>.02  # only plot if more than X% corrects
    
    # Plot the predicted RTs
    for(i in 1:nq)
    {
      points(y=c(rev(predrts[i,"err",use,mod,s]),predrts[i,"cor",,mod,s]),
             x=c(rev(1-predps[use,mod,s]),predps[,mod,s]),pch=modelFitMarker,col=graycol, 
             bg=graybgcol,cex=ptcexs,type="b",lwd=lwds*1.2)
    }
    
    # Plot the actual data in the appropriate markers. Colors 
    points(x=rep(dat$p,each=nrow(dat$q)),y=dat$q[,"cor",],pch=dataMarker,col=bluecol,lwd=lwds,cex=ptcexs)
    use=(1-dat$p)>.02   # only plot if more than X% corrects
    points(x=rep(1-dat$p[use],each=nrow(dat$q[,,use])),y=dat$q[,"err",use],
           pch=dataMarker,col=yellowcol,lwd=lwds,cex=ptcexs)
  }
}
# Get ggplotQP,  to show all the points..




setwd(currwd)

plotted = 1;
s = "Subj3"

dat=data[[s]] ; dat$q=dat$q[as.character(qps),,]

qp = c();
for(mod in usemodel) {
  
  for(i in 1:nq)
  {
    y=c(rev(predrts[i,"err",use,mod,s]),predrts[i,"cor",,mod,s]);
    x=c(rev(1-predps[use,mod,s]),predps[,mod,s]);
    data = data.frame(x=x, y=y);
    if(i==1)
    {
      currPlot = ggplot(data, aes(x=x, y=y)) + geom_point(shape=modelFitMarker,fill=graybgcol, color=graycol, size=2);
      currPlot = currPlot + geom_line(data = data, mapping=aes(x=x, y=y, color=graycol))
    }
    else
    {
      currPlot = currPlot + geom_point(data = data, mapping=aes(x=x, y=y, shape=modelFitMarker,fill=graybgcol, color=graycol, size=2));
      currPlot = currPlot + geom_line(data = data, mapping=aes(x=x, y=y, color=graycol));
    }
    
    realData = data.frame(x=dat$p, y=dat$q[i,"cor",]);
    currPlot = currPlot + geom_point(data=realData, mapping=aes(x=x,y=y, color=bluecol), shape=dataMarker);
    realData = data.frame(x=1-dat$p, y=dat$q[i,"err",]);
    currPlot = currPlot + geom_point(data=realData, mapping=aes(x=x,y=y, color=yellowcol), shape=dataMarker);
    
    
    
  }
  currPlot = currPlot + theme_minimal() + theme(aspect.ratio = 1);
  currPlot= currPlot  + ylab(NULL) + xlab(NULL);  
  
  currPlot = currPlot + theme(panel.grid.major= element_blank(), panel.grid.minor = element_blank());
  currPlot = currPlot +  theme(axis.line = element_line(color=gray(0.8))); 
  currPlot = currPlot +  theme(axis.ticks = element_line("gray", size=.25), axis.ticks.length = unit(.15,"cm")); 
  
  
  
  qp[[mod]] = currPlot;
  plotted = plotted + 1;
}
do.call("grid.arrange", c(qp, ncol=5, nrow=2)); 




