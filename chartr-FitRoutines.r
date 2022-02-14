# generic fitting routine for fixed vs collapsing bound models #
#
#
#
#
# simulate contaminant trials, for plotting output
contaminantmixresps=function(preds,contp,nmc) {
  contn=floor(contp$p*nmc)
  if(contn>0) {
    preds$rts[1:contn]=runif(n=contn,min=contp$minrt,max=contp$maxrt)
    preds$resps[1:contn]=sample(x=c(1,2),size=contn,replace=TRUE)
  }
  preds
}

qmpouts=function(qs,preds,pred=F,qps,contp,nmc) {
  # get probability of correct[1] or error[2]
  out=list(p=numeric(2))
  out$p[1]=sum(preds$resps==1)/nmc
  out$p[2]=1-out$p[1]
  
  nbins=length(qps)+1
  if(!pred) {
    # Calculate probability masses in inter-quantile ranges.
    out$predp=array(dim=c(nbins,2))
    for (i in 1:2) {
      # correct probabilities for total probability of response
      userts=preds$rts[preds$resps==i]
      if(length(userts)==0) {
        out$predp[,i]=(.5/nbins)/nmc
      } else {
        out$predp[,i]=table(cut(x=userts,breaks=c(0,qs[,i],Inf)))/nmc
      }
    }
    # contaminant mixture process
    out$predp=out$predp*(1-contp)+(contp/(nbins*2))
  } else {
    # calculate predicted quantile values
    out$predq=cbind(quantile(preds$rts[preds$resps==1],probs=qps),
                    quantile(preds$rts[preds$resps==2],probs=qps))
  }
  out
}


getpreds=function(x,dat,nmc,contp,ncohs,fitUGM,pred=F,gub,
                  qps,stepsize,stoch.s,timecons,usign,parnames,maxTimeStep) {
  # pred=F returns chi-squared, pred=T returns a list like dat with predicted
  # quantiles and probabilities (for later plotting)
  # - don't need dat for fitting, but use it to get shape for model preds
  names(x)=parnames
 
  z=makeparamlist(params=x,fitUGM=fitUGM,ncohs=ncohs)
  

  #  maxiter=as.double(length(seq(0,bailouttime,stepsize)))  # bailouttime for C code
  mod=list(p=vector(length=length(dat$p)))
  if(pred) mod$q=array(dim=dim(dat$q)) else mod$q=array(dim=dim(dat$pb))
  
  interval=0.00001;
  LUT=qnorm(seq(interval,1-interval,interval));
  nLUT = length(LUT);
  
  for(N in 1:length(ncohs)){
    tmp=diffusionC(v=z$v[N],eta=z$eta,aU=z$aU,aL=z$aL,Ter=z$Ter,
                   intercept=z$intercept, ieta=z$ieta, st0=z$st0,
                   z=z$z, zmin=z$zmin,zmax=z$zmax,boundv = z$boundv, timecons_var = z$timecons_var, usign_var = z$usign_var, usigneta = z$usigneta,
                   sx = z$sx, sy=z$sy, delay=z$delay,lambda = z$lambda, aprime = z$aprime, k = z$k,
                   nmc=nmc,dt=stepsize,stoch.s=stoch.s,maxTimeStep=maxTimeStep,fitUGM=fitUGM, timecons=timecons,usign=usign, 
                   nLUT=nLUT, LUT=LUT)   
              # remove trials slower than gub
              use=tmp$rts<gub
              # with no between trial variability, can get stuck in bad parameter space
              #   sub in a single dud RT, so there's something there to prevent optimiser crashing
            if(sum(use)==0) {
              tmp$resps=2 ; tmp$rts=runif(1,0,gub) ; use=T
            } else {
              tmp$resps=tmp$resps[use] ; tmp$rts=tmp$rts[use]
            }
    # use function qmpouts for simulation models
    tmp=qmpouts(qs=dat$q[,,N],preds=tmp,pred=pred,qps=qps,contp=contp$p,nmc=sum(use))
    mod$p[N]=tmp$p[1]   # correct stored in p[1], error in p[2]
    mod$q[,,N]=switch(pred+1,tmp$predp,tmp$predq)
  }
  mod
}

obj=function(x,dat,nmc,contp,ncohs,fitUGM,pred=F,gub,
             qps,stepsize,stoch.s,timecons,usign,parnames,maxTimeStep) {
    preds=getpreds(x=x,dat=dat,nmc=nmc,contp=contp,ncohs=ncohs,fitUGM=fitUGM,
    pred=pred,gub=gub,qps=qps,stepsize=stepsize,stoch.s=stoch.s,
    timecons=timecons,usign=usign,parnames=parnames,maxTimeStep=maxTimeStep)
  if(!pred) {
    -sum(dat$pb*log(pmax(preds$q,1e-10)))
   } else {
     preds
   }
}

objPerQ=function(x,dat,nmc,contp,ncohs,fitUGM,pred=F,gub,
                 qps,stepsize,stoch.s,timecons,usign,parnames,maxTimeStep) {
  preds=getpreds(x=x,dat=dat,nmc=nmc,contp=contp,ncohs=ncohs,fitUGM=fitUGM,
                 pred=pred,gub=gub,qps=qps,stepsize=stepsize,stoch.s=stoch.s,
                 timecons=timecons,usign=usign,parnames=parnames,maxTimeStep=maxTimeStep)
  if(!pred) {
    -dat$pb*log(pmax(preds$q,1e-10))
  } else {
    preds
  }
}

