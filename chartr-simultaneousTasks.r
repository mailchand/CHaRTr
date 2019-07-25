#' Processes RT data from our experiments and uses it for testing if the DDM or the UGM
#' are better fits to the data
#' interface script for fitting routines to test Stone vs Stone+UGM model #
#' 

# Typically because there is randomness in the optimizer you end up using at least five runs of the model in order
# to be sure your data are correct


rm(list=ls())
library(tictoc);
source("chartr-FitRoutines.r")
source("chartr-HelperFunctions.r")

jobnum=as.numeric(Sys.getenv("SGE_TASK_ID"));

set.seed(jobnum)
fnam = letters[jobnum];

# source the fitting code files

dirs="RS2002"  # directory name of data files to fit
subjs=dir(dirs); 
nsubj=length(subjs) 

# subject names
# set up fit to data

# Grab a list of subjects
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

allValidModels = returnListOfModels()
modelList = unname(allValidModels$modelNames)

for(subjId in seq(1,length(listOfSubjects)))
{
  subjnam=listOfSubjects[subjId]
  for(modelId in seq(1,length(modelList)))
  {

    # Clear logs for tic
    tic.clearlog();
    tic("dataLoading", quiet=TRUE);
    
    # load data set to fit
    load(paste(dirs,"/",subjnam,sep=""))
    model= modelList[modelId]
    
    saveFileName=paste(dirs,"_Fits/",subjnam,"-",model,"-",fnam,sep="")
    if(!file.exists((saveFileName))){
    cat(paste("\n",dirs,"dataset:",subjnam,", \n model:",model,"\n Filename", fnam,"\n\n",sep=" "))  
    
    # for simple switching in fitting routine
    qps=as.numeric(dimnames(dat$q)[[1]])
    ncohs=1:dim(dat$q)[3]
    
    
    # make parameter vector - different scaling for Stone and Stone+UGM
    #    order: drifts (7), eta, upper boundary, Ter
    nstart=1;
    nds=length(ncohs)    # v1 is 0% coherence, will be set to 0 drift in internal function
    
    
    actualParams = paramsandlims(model, nds, nstart=nstart)
    fitUGM = unname(actualParams$fitUGM)
    lowers = actualParams$lowers
    uppers = actualParams$uppers
    parnames = actualParams$parnames
    
    stepsize=ifelse(fitUGM < 0,.001,1)  # time step for diffusion process: .001s (Stone), 1ms (UGM)
    stoch.s=ifelse(fitUGM < 0,.1,100)   # diffusion constant
    timecons=ifelse(fitUGM < 0,0,100)   # time constant for low pass filter
    usign=ifelse(fitUGM < 0,0,1)        # scaling value for linear urgency function (usign=1 is linear with time). EAM needs usign=0, timecons=0
    
    toc(log=TRUE,quiet=TRUE);
    
    
    # cutoff for very slow RTs
    # gub = global upper bound in seconds - no RTs should be slower than this (and none were in data)
    tic("optimization", quiet=TRUE);
    print("Starting optimization ...")
    library(DEoptim)
    
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
        ));
    cat(paste("\n",dirs,"dataset:",subjnam,", model:",model,fnam,"\n\n",sep=" "))
    
    toc(log=TRUE,quiet=TRUE);
      
    tic("objective",quiet=TRUE);
      
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
    toc(log=TRUE,quiet=TRUE);
    
    # Now get timings etc from the toc log.
    
    timingLogTxt <- tic.log(format = TRUE);
    timingLogLst <- tic.log(format = FALSE)
    timings <- unlist(lapply(timingLogLst, function(x) x$toc - x$tic))
    rawTimingsTic <- unlist(lapply(timingLogLst, function(x) x$tic))
    rawTimingsToc <- unlist(lapply(timingLogLst, function(x) x$toc))
    
    out=list(dataset=subjnam,model=model,ndataset=fnam,pars=out,
             obj=-tmp$optim$bestval,reobj=-reobj, reobjperpoint=reobjperpoint, 
             lower=lowers, upper=uppers,
             timings = timings, timingLogLst = timingLogLst, timingLogTxt = timingLogTxt, 
             rawTimingsTic = rawTimingsTic, rawTimingsToc = rawTimingsToc);
    
    
    save(out,file=saveFileName)
    }
    else
    {
      cat(paste("\n",dirs,"dataset:",subjnam,", model:",model,fnam," already exists..skipping",sep=" "))  
    }

  }
}
