# interface script for fitting routines to test Stone vs Stone+UGM model #
rm(list=ls())

totaljobs=20  # 2 models, x2 data sets (2 RS2002 subjects), x5 reps per data set

# this code is designed to be submitted to a grid. first, generate a replicate
#    of this code for each job you will run, where each file has a number
#    from 1:totaljobs for jobnum. 
# If you need help with this, ask Scott (he taught me this way of doing it!).  
jobnum=1
set.seed(jobnum)

# source the fitting code files
source("EAM-UGM-FitRoutines.r")
source("diffusion-EAM-UGM.r")

nreps=5   # number of times to repeat each job, due to randomness in optimiser
dirs="RS2002"  # directory name of data files to fit
subjs=dir(dirs) ; nsubj=length(subjs)  # subject names
# set up fit to data
subjnam=rep(subjs,each=nreps,times=2)[jobnum]
model=rep(c("stoneEta","stoneEtaUGM"),each=nsubj*nreps)[jobnum]
fnam=rep(letters[1:nreps],times=nsubj*2)[jobnum]
estcontp=FALSE  # estimate contaminant mixture probability from data. usually set to false
# load data set to fit
load(paste(dirs,"/",subjnam,sep=""))
cat(paste("\n",dirs,"dataset:",subjnam,", model:",model,fnam,"\n\n",sep=" "))

# for simple switching in fitting routine
fitUGM=ifelse(model=="stoneEtaUGM",TRUE,FALSE)  
contp=list(p=0)  # if estcontp==FALSE, give proportion of contaminant responses
maxits=500  # number of iterations of DEoptim to run
nparticles=100  # number of particles/chains for DEoptim
nmc=10000  # number of MC samples for simulating the models at each iteration
#nc=1  # change if future data sets have other manipulation, e.g.,  speed & accuracy
qps=as.numeric(dimnames(dat$q)[[1]])
ncohs=1:dim(dat$q)[3]
bailouttime=4  # time after which to bail out of diffusion sims, in seconds
maxiter=as.double(length(seq(0,bailouttime,.001)))   # max iterations for fitting routine, then bail out
pred=F  # generate model predictions from fitting routine (only use this once you have estimtated parameters from data using DEoptim)

# switch parameters depending on model
stepsize=ifelse(!fitUGM,.001,1)  # time step for diffusion process: .001s (Stone), 1ms (UGM)
stoch.s=ifelse(!fitUGM,.1,100)   # diffusion constant
timecons=ifelse(!fitUGM,0,100)   # time constant for low pass filter
usign=ifelse(!fitUGM,0,1)        # scaling value for linear urgency function (usign=1 is linear with time). EAM needs usign=0, timecons=0

# determine min(rt) and max(rt) for simulating contaminant mixture responses
#   not used in fitting RS2002 for JNeurophys paper
#contp$minrt=min(dat$x$rt,na.rm=T) ; contp$maxrt=max(dat$x$rt,na.rm=T)

# make parameter vector - different scaling for Stone and Stone+UGM
#    order: drifts (5), eta, upper boundary, Ter
nds=length(ncohs)-1    # v1 is 0% coherence, will be set to 0 drift in internal function
parnames=c(paste("v",2:(nds+1),sep=""),"eta","aU","Ter")
if(!fitUGM) {
  lowers=rep(0,nds+3)
  uppers=c(rep(.6,nds),.35,.4,.6)
} else {
  lowers=rep(0,nds+3)
  uppers=c(rep(40,nds),20,20000,.6)   # convert inside makeparamlist(), so estimated on s scale
}

# cutoff for very slow RTs
# gub = global upper bound in seconds - no RTs should be slower than this (and none were in data)
gub=4


library(DEoptim)
system.time({tmp=DEoptim(fn=obj,lower=lowers,upper=uppers,dat=dat,nmc=nmc,
       contp=contp,ncohs=ncohs,fitUGM=fitUGM,gub=gub,pred=FALSE,
       qps=qps,stepsize=stepsize,stoch.s=stoch.s,timecons=timecons,usign=usign,
       parnames=parnames,maxiter=maxiter,
       control=DEoptim.control(itermax=maxits,NP=nparticles,trace=TRUE,
         parallelType=1,reltol=1e-6,steptol=200,
  # load objects used for fitting, for parallelType==1
  parVar=list("dat","lowers","uppers","nmc","contp","ncohs","fitUGM","pred","qps","stepsize","stoch.s","timecons","usign","parnames","maxiter","maxits","nparticles","gub",
  # same again, but for functions
  "diffusionC","makeparamlist","contaminantmixresps","qmpouts","getpreds","obj")
  ))})

cat(paste("\n",dirs,"dataset:",subjnam,", model:",model,fnam,"\n\n",sep=" "))
out=tmp$optim$bestmem
names(out)=parnames
print(round(out,4))

print(round(tmp$optim$bestval,4))

# re-calculate obj for best fitting parameters, to determine amount of noise
# in the obj value for the best fit
mcsforpreds=50000
reobj=obj(x=tmp$optim$bestmem,dat=dat,nmc=mcsforpreds,
    contp=contp,ncohs=ncohs,fitUGM=fitUGM,gub=gub,pred=FALSE,
    qps=qps,stepsize=stepsize,stoch.s=stoch.s,timecons=timecons,usign=usign,
    parnames=parnames,maxiter=maxiter)
print(round(reobj,4))

out=list(dataset=subjnam,model=model,ndataset=fnam,pars=out,
    obj=-tmp$optim$bestval,reobj=-reobj)

save(out,file=paste("modelFits/",subjnam,"-",model,"-",fnam,sep=""))

