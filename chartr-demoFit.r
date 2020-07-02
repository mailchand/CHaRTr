# Remove variables
rm(list=ls())

# Add some helper functions
source("chartr-FitRoutines.r")
source("chartr-HelperFunctions.r")

# Which job number to run, this tells you the run number
jobnum=1
set.seed(jobnum)
fnam = letters[jobnum];

# source the fitting code files
dirs="caseStudy2"  # directory name of data files to fit
resultDir = "caseStudy2_Fits/"

subjs=dir(dirs); 
nsubj=length(subjs) 
listOfSubjects = subjs;



# Setup some baseline parameters
contp = list(p=0)  # if estcontp==FALSE, give proportion of contaminant responses
maxits = 750  # number of iterations of DEoptim to run
nparticles = 400  # number of particles/chains for DEoptim
nmc =10000  # number of MC samples for simulating the models at each iteration
estcontp=FALSE  # estimate contaminant mixture probability from data. usually set to false


bailouttime=4  # time after which to bail out of diffusion sims, in seconds
maxTimeStep=as.double(length(seq(0,bailouttime,.001)))   # max iterations for fitting routine, then bail out
pred=F  # generate model predictions from fitting routine (only use this once you have estimtated parameters from data using DEoptim)

nreps = 5;
gub=4



subjnam="Subj1"

load(paste(dirs,"/",subjnam,sep=""))
model= "DDM"
saveFileName=paste(resultDir,subjnam,"-",model,"-",fnam,sep="")

# for simple switching in fitting routine
qps=as.numeric(dimnames(dat$q)[[1]])
ncohs=1:dim(dat$q)[3]

# make parameter vector - different scaling for Stone and Stone+UGM
#    order: drifts (7), eta, upper boundary, Ter
nds=length(ncohs)

actualParams = paramsandlims(model, nds)
fitUGM = unname(actualParams$fitUGM)
lowers = actualParams$lowers
uppers = actualParams$uppers
parnames = actualParams$parnames

stepsize=ifelse(fitUGM < 0,.001,1)  # time step for diffusion process: .001s (Stone), 1ms (UGM)
stoch.s=ifelse(fitUGM < 0,.1,100)   # diffusion constant
timecons=ifelse(fitUGM < 0,0,100)   # time constant for low pass filter
usign=ifelse(fitUGM < 0,0,1)        # scaling value for linear urgency function (usign=1 is linear with time). EAM needs usign=0, timecons=0
# cutoff for very slow RTs
# gub = global upper bound in seconds - no RTs should be slower than this (and none were in data)

print("Starting optimization ...")
library(DEoptim)
system.time({
  tmp=DEoptim(
    fn=obj,
    lower=lowers,
    upper=uppers,
    dat=dat,
    nmc=nmc,
    contp=contp,
    ncohs=ncohs,
    fitUGM=fitUGM,
    gub=gub,
    pred=FALSE,
    qps=qps,
    stepsize=stepsize,
    stoch.s=stoch.s,
    timecons=timecons,
    usign=usign,
    parnames=parnames,
    maxTimeStep=maxTimeStep,
    control=DEoptim.control(itermax=maxits,NP=nparticles,trace=TRUE,
                            parallelType=1,reltol=1e-6,steptol=200,
                            # load objects used for fitting, for parallelType==1
                            parVar=list("dat","lowers","uppers","nmc","contp","ncohs", "fitUGM","pred",
                                        "qps", "stepsize","stoch.s","timecons","usign","parnames","maxTimeStep","maxits","nparticles","gub",
                                        "diffusionC","makeparamlist","contaminantmixresps","qmpouts","getpreds","obj","returnListOfModels")
                            # same again, but for functions
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
          parnames=parnames,maxTimeStep=maxTimeStep)
print(round(reobj,4))


# Now compute it for each level of quantile. Suspicion is that you go awry for the hardest coherences and you really need to think 
# about what goes on there. Life is not easy there :)
mcsforpreds=50000
reobjperpoint=objPerQ(x=tmp$optim$bestmem,dat=dat,nmc=mcsforpreds,
                      contp=contp,ncohs=ncohs,fitUGM=fitUGM,gub=gub,pred=FALSE,
                      qps=qps,stepsize=stepsize,stoch.s=stoch.s,timecons=timecons,usign=usign,
                      parnames=parnames,maxTimeStep=maxTimeStep)
print(round(reobjperpoint,4))
out=list(dataset=subjnam,model=model,ndataset=fnam,pars=out,
         obj=-tmp$optim$bestval,reobj=-reobj, reobjperpoint=reobjperpoint)


save(out,file=saveFileName)