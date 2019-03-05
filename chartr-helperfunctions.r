# diffusion model - compare fixed and collapsing bounds #
# load C functions

dyn.load("chartr-modelspec.so")

# convert into a table object for the future so that life is a bit easier
# 24 models 
returnListOfModels = function()
{
  modelList = c(
    "DDM",                     # -1
    "DDMSv",                   # -2
    "DDMSvSt",                 # -3
    "dDDMSvSt",                # -4
    "DDMSvSzSt",               # -5
    "DDMSvSz",                 # -6
    "cDDMSvSt",                # -7
    "dDDMSv",                  # -8
    "cfkDDMSvSt",              # -9
    "dDDMSvSzSt",              # -10
    "cfkDDMSvSzSt",            # -11
    "cDDMSvSzSt",              # -12
    "UGM",                     #  1
    "UGMSt",                   #  2 
    "UGMSv",                   #  3
    "UGMSvSt",                 #  4  
    "bUGMSv",                  #  5       
    "bUGMSvSt",                #  6       
    "bUGMSvSb",                #  7       
    "bUGMSvSbSt",              #  8
    "uDDMSvSb",                #  9 // A simple urgency model that has no time constant or low pass filtering
    "uDDMSv",                  #  10
    "uDDMSvSbSt",              #  11 
    "uDDMSvSt"                 #  12
  );

  #"cDDMSv",                  # -15
  #"cDDMSvSz",                # -16
  #"cfkDDMSv",                # -17
  #"cfkDDMSvSz",              # -18
  #"cUGMSvSt"                #  13    
  
  modelIds = c(seq(-1,-12), seq(1,12));
  modelNames <- setNames( modelList, modelIds)
  names(modelIds) = modelList
  list(modelIds=modelIds,modelNames=modelNames)
}

printModels = function()
{
  allValidModels = returnListOfModels()
  modelList = unname(allValidModels$modelNames);
  modelList = sort(modelList, decreasing=TRUE)
  
  cat(sprintf("CHaRTr has %d models", length(modelList)))
  priorClass = '';
  for(m in modelList)
  {
    firstChar = substr(m,1,1);
    if(firstChar!=priorClass)
      cat("\n");
    cat(sprintf("\n%15s", m))

    switch(firstChar,
           "D"={
             modelClass = "DDM"
           },
           "U"={
             modelClass="UGM"
           },
           "u"={
             modelClass = "urgency"
           },
           "c"={
             modelClass = "Collapsing"
           },
           "d"={
             modelClass = "Ditterich"
           },
           "b"={
             modelClass = "UGM with intercept"
           }
           
    )
    priorClass = firstChar;
    cat(sprintf(" ----- %s",modelClass))
  }
}



# Generates the parameters and 
paramsandlims=function(model, nds, fakePars=FALSE, nstart=1)
{
  # Non UGMs will have values less than 0
  # UGMs will have values greater than 0
  listOfModels = returnListOfModels()
  
  fitUGM = listOfModels$modelIds[model]
  
  upper_v_ddm = .6;
  upper_aU_ddm = .5;
  upper_Ter = 0.8;
  upper_st0 = 0.6;
  upper_eta = .3;
  upper_zmin = 1;
  upper_zmax = 1;
  
  
  upper_v_urgency = 40;
  upper_aU_urgency = 20000;
  upper_eta_urgency = 20;
  upper_intercept = 1000;
  upper_ieta = 300;
  upper_usign_var = 20;
  upper_timecons_var = 2000;
  
  
  upper_aprime = 0.5;
  
  NdriftRates = nds - nstart + 1; # Say 6 drift rates, starting at 2, means 5 conditions
  
  TempNamesDDM = c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta", "st0",
                "zmin","zmax","sx","sy","delay","lambda", "aprime","k");
  
 
  fakeParsDDM = c(seq(0.04, 0.4, length.out = NdriftRates), 0.08, 0.3, 0.1, 0.15, 0.08, 0.12, 9, 9,0.14, 5, 0.3, 10);
  names(fakeParsDDM) = TempNamesDDM;
  
 TempNamesUGM = c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta", "st0",
                "intercept","ieta","timecons_var","usign_var", "lambda", "aprime","k");
  fakeParsUGM = c(seq(1.5, 15,  length.out=NdriftRates) + 0.5*rnorm(nds), 12000, 0.3, 4, 0.1, 1000, 600, 200,1, 5, 0.3, 10);
  names(fakeParsUGM) = TempNamesUGM;

  switch(model, 
         DDM={ # 7 coherences, an upper bound, and a lower bound, symmetric bounds
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter")
           print("Vanilla diffusion model with no bells and whistles")
         },
         DDMSv={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta")
           print("Diffusion model with some drift variance")
         },
         DDMSvSz={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","zmin","zmax")
           print("Diffusion model, Variable Baseline, Variable Movement Time")
         },
         DDMSvSzSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","zmin","zmax","st0")
           print("Diffusion model, Variable Baseline, Variable Movement Time")
         },
         ratcliff={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","zmin","zmax","st0")
           print("Ratcliff model with variable movement time")
         },
         DDMSvSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","st0")
           print("Diffusion model with some drift variance and variable movement time")
         },
         dDDMSvSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","st0","sx","sy","delay")
           print("Diffusion model with some drift variance and variable movement time and most importantly urgency")
         },
         
         dDDMSv={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","sx","sy","delay")
           print("Diffusion model with some drift variance and variable movement time and most importantly urgency")
         },
         
         cDDMSvSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","st0","lambda","aprime","k")
           print("Diffusion model with some drift variance, variable movement time and collapsing bounds")
         },
         
         cfkDDMSvSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","st0","lambda","aprime")
           print("Diffusion model with some drift variance, variable movement time and collapsing bounds")
         },
         
         cfkDDMSvSzSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","st0","lambda","aprime","zmin","zmax")
           print("Diffusion model with some drift variance, variable movement time and collapsing bounds")
         },
         
         cDDMSvSzSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","st0","lambda","aprime","k","zmin","zmax")
           print("Diffusion model with some drift variance, variable movement time and collapsing bounds")
         },
         
         cDDMSv={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","lambda","aprime","k")
           print("Diffusion model with some drift variance, variable movement time and collapsing bounds")
         },
         cDDMSvSz={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","lambda","aprime","k","zmin","zmax")
           print("Diffusion model with some drift variance, variable movement time and collapsing bounds")
         },
         
         cfkDDMSv={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","lambda","aprime","zmin","zmax")
           print("Diffusion model with some drift variance, variable movement time and collapsing bounds")
         },
         cfkDDMSvSz={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","lambda","aprime","zmin","zmax")
           print("Diffusion model with some drift variance, variable movement time and collapsing bounds")
         },
         cfkDDMSvSz={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","lambda","aprime","zmin","zmax")
           print("Diffusion model with some drift variance, variable movement time and collapsing bounds")
         },
         
         dDDMSvSzSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","st0","sx","sy","delay","zmin","zmax")
           print("Diffusion model with some drift variance and variable movement time and most importantly urgency")
         },
         
         
         
         UGM={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter")
           print("Urgency Gating Model")
         },
         UGMSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","st0")
           print("Urgency gating model with some drift variance and variable residual movement time")
         },
         UGMSv={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta")
           print("Urgency Gating With Drift Variance")
           
         },
         UGMSvSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","st0")
           print("Urgency Gating, drift variance, variable residual movement time")
         },
         bUGMSv={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","intercept")
           print("Urgency Gating with an intercept")           
         }, 
         bUGMSvSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","intercept", "st0")
           print("Diffusion model with some drift variance, urgency gating, residual
                 movement time, and an intercept for urgency gating")
         }, 
         bUGMSvSb={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","intercept","ieta")
           print("Diffusion model with some drift variance, urgency gating, variable residual 
                 movement time, and an intercept for urgency gating")
           
         },  
         bUGMSvSbSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","intercept","ieta","st0")
           print("Diffusion model with some drift variance, urgency gating, variable residual 
                 movement time, and an intercept for urgency gating")
         }, 
         
         # A UGM model with variability in all the parameters so that the time constant is not causing unnecessary issues       
         UGMSvallVar={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","intercept","ieta","st0","timecons_var","usign_var")
           print("Full UGM model with variability in time constants and slope parameter")
         }, 
         
         # Urgency signal in PMd
         uDDMSvSb={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","intercept","ieta","usign_var")
           print("DDM with Urgency and no gating and fixed Ter")
         },
         
         uDDMSv={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","intercept","usign_var")
           print("DDM with Urgency and no gating, constant slope, and fixed Ter")
         }, 
         uDDMSvSbSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","intercept","ieta","usign_var","st0")
           print("DDM with Urgency and no gating and variable Ter")
           upper_intercept=20;
           upper_ieta=20;
         },
         
         uDDMSvSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","intercept","usign_var","st0")
           print("DDM with Urgency and no gating, constant slope, and variable Ter")
           upper_intercept=20;
           upper_ieta=20;
           
         }, 
         cUGMSvSt={
           parnames=c(paste("v",(nstart):(nds),sep=""),"aU","Ter","eta","st0","lambda","aprime","k")
           print("Collapsing bounds urgency gating model with a 100 ms time constant and variable non decision time")
          upper_aU_urgency = 3;
          upper_v_urgency = 20;
         }
  )  
  parUppersDDM = c(rep(upper_v_ddm,NdriftRates), upper_aU_ddm, upper_Ter, upper_eta, upper_st0,
                   upper_zmin, upper_zmax, 50, 50, 3, 10,upper_aprime,20);
  names(parUppersDDM) = TempNamesDDM;
  
  parUppersUGM = c(rep(upper_v_urgency,NdriftRates), upper_aU_urgency, upper_Ter, upper_eta_urgency, upper_st0,
                   upper_intercept, upper_ieta, upper_timecons_var, upper_usign_var,  10,upper_aprime,20)
  names(parUppersUGM)  = TempNamesUGM;
  
  
  
  uppers = seq(1,length(parnames));
  for(i in seq(1,length(parnames))){
    if(fitUGM < 0)
      uppers[i] = parUppersDDM[parnames[i]]  
    else
      uppers[i] = parUppersUGM[parnames[i]]  
    
  }
  lowers=rep(0,length(uppers))
  
  # Set lower limit for Ter to be 0.15, otherwise you can get very low Ter values
  idx = which(parnames == "Ter")
  lowers[idx] = 0.05;
  
  
  if(fakePars)
  {
    fakeParams = seq(1,length(parnames));
    for(i in seq(1,length(parnames))){
      if(fitUGM < 0)
        fakeParams[i] = fakeParsDDM[parnames[i]]  
      else
        fakeParams[i] = fakeParsUGM[parnames[i]]  
    }
    names(fakeParams) = parnames;
  }
  else
  {
    fakeParams = NULL;
  }
  
  list(parnames=parnames, uppers=uppers, lowers=lowers, fitUGM=fitUGM, fakeParams = fakeParams)
  
}
# End the list of parameters for the models

diffusionC=function(v,eta,aU,aL,Ter,intercept,ieta,st0, z, zmin, zmax, nmc, dt,stoch.s,
                    maxTimeStep,fitUGM,timecons,usign=1, timecons_var = timecons_var, usign_var = usign_var, 
                    sx=sx, sy=sy, delay=delay,
                    lambda = lambda, aprime = aprime, k = k, VERBOSE=FALSE) 
  # v - drift rates
  # eta - Standard deviation for the drift rate
  # aU - upper bound, aL, lower bound
  # intercept - intercept for the urgency signal
  # ieta - variability for the intercept
  # st0 - variability in non decision-time
  # z - start point
  # zmin - min range
  # zmax - max range
  # nmc - number of montecarlo simulations
  # dt - time step
# fitUGM - Should we be fitting a UGM
# timecons- fixed paramter for the Urgency gating model usually 100 - 200 ms
# usign - Fixed value for the slope of the urgency signal, usually 1 for UGM
# timecons_var - used if you wish to allow time constant to vary freely
# usign_var - used if you wish to allow urgency signal slope to vary freely
# sx, xy, delay - parameters for the Ditterich model
# lambda, aprime, k - parameters for the collapsing bounds model
{
  
  dyn.load("chartr-modelspec.so")
  rts <- resps <- numeric(nmc)
  
  listOfModels = returnListOfModels()
  
  modelName = unname(listOfModels$modelNames[as.character(fitUGM)])
  if(VERBOSE) 
    print(modelName);
  
  switch(modelName,
         # -----------------------------                
         # Vanilla diffusion model .. Works on a simple DDM principle
         # Nothing fancy in terms of the model spec
         DDM={
           
           out=.C("DDM",z=z,v=v,aU=aU,aL=aL, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep)
           rts=out$rt + Ter;
         },
         
         # Same as the vanilla diffusion model, except with a modification of the 
         # model to include some variability in the drift rates.
         DDMSv={
           out=.C("DDMSv",z=z,v=v,eta=eta,aU=aU,aL=aL, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=out$rt + Ter;
         },
         
         # DDM Model with urgency gating
         # 
         UGM={
           out=.C("UGM",z=z,v=v,aU=aU,aL=aL, timecons=timecons, usign=usign, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000) + Ter;
         },
         
         # DDM model now adding variability of the drift rate
         UGMSv={
           out=.C("UGMSv",z=z,v=v,eta=eta,aU=aU,aL=aL, timecons=timecons, usign=usign, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000) + Ter;
         },
         
         
         # DDM Eta UGM model with an intercept term for the urgency signal
         bUGMSv={
           out=.C("bUGMSv",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons=timecons,
                  usign=usign,intercept=intercept, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000) + Ter;
         },
         
         # Assume that this intercept is variable as well. Assume constant slope though (1 in case of Cisek)
         bUGMSvSb={
           out=.C("bUGMSvSb",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons=timecons,
                  usign=usign,intercept=intercept,ieta=ieta, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000) + Ter;
         },
         
         # Now assume a Urgency model where you fit the intercept, variability in this intercept and a variable slope
         uDDMSvSb={
           out=.C("uDDMSvSb",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, intercept=intercept,ieta=ieta,
                  usign_var=usign_var, s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+Ter;
         }, 
         
         # Now assume a Urgency model where you fit the intercept, and a slope term. 
         uDDMSv={
           out=.C("uDDMSv",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, 
                  intercept=intercept, usign_var=usign_var,s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+Ter;
         },
         
         
         # -------------------------------- Models without any variable movement time
         
         
         # Same as the model with drift variance but incorporation of a variable
         # time for the non decision time (See Ratcliff and Tuerlinckx, 2002)
         
         DDMSvSt={
           out=.C("DDMSv",z=z,v=v,eta=eta,aU=aU,aL=aL, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep)
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # This is a bit of a misnomer. The Ratcliff model always assumes a variable residual movement time
         ratcliff={
           out=.C("ratcliff",zmin=zmin, zmax=zmax,v=v,aU=aU,aL=aL,eta=eta,s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt) +runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         DDMSvSz={
           out=.C("DDMSvSz",zmin=zmin, zmax=zmax,v=v,aU=aU,aL=aL,eta=eta,s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=out$rt+Ter;
         },
         # Identical to the ratcliffSt model
         DDMSvSzSt={
           out=.C("DDMSvSz",zmin=zmin, zmax=zmax,v=v,aU=aU,aL=aL,eta=eta,s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # This is the ditterich model that assumes a complex shape for the bound collapse
         dDDMSvSt={
           out=.C("dDDMSv",z=z,v=v,eta=eta, delay=delay, sx=sx, sy=sy, aU=aU,aL=aL,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep)
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         dDDMSv={
           out=.C("dDDMSv",z=z,v=v,eta=eta, delay=delay, sx=sx, sy=sy, aU=aU,aL=aL,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep)
           rts=out$rt+Ter;
           
         },
         
         dDDMSvSzSt={
           out=.C("dDDMSvSz",zmin=zmin,zmax=zmax,v=v,eta=eta, delay=delay, sx=sx, sy=sy, aU=aU,aL=aL,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep)
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         cfkDDMSvSt={
           out=.C("cfkDDMSv",z=z,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL,aprime=aprime,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # Replicating model from Hawkins et al. 2015
         cfkDDMSvSzSt={
           out=.C("cfkDDMSvSz",zmin=zmin,zmax=zmax,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL,aprime=aprime,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         cDDMSvSt={
           out=.C("cDDMSv",z=z,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL, aprime = aprime, k=k,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep)
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         cDDMSvSzSt={
           out=.C("cDDMSvSz",zmin=zmin, zmax=zmax,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL, aprime = aprime, k=k,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep)
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # Now do all the Collapsing bounds without the variabililty in nondecision-time
         
         
         cfkDDMSv={
           out=.C("cfkDDMSv",z=z,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL,aprime=aprime,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=out$rt+Ter;
         },
         
         # Replicating model from Hawkins et al. 2015
         cfkDDMSvSz={
           out=.C("cfkDDMSvSz",zmin=zmin,zmax=zmax,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL,aprime=aprime,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=out$rt+Ter;
         },
         
         cDDMSv={
           out=.C("cDDMSv",z=z,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL, aprime = aprime, k=k,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep)
           rts=out$rt+Ter;
         },
         
         cDDMSvSz={
           out=.C("cDDMSvSz",zmin=zmin, zmax=zmax,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL, aprime = aprime, k=k,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep)
           rts=out$rt+Ter;
         },
         
         
         
         cUGMSvSt={
             out=.C("cUGMSv",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons=timecons,lambda=lambda,
                  k=k, aprime=aprime, s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
             rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);   
           
         },
         
         
         # Now do the urgency model with variable Ter
         UGMSt={
           out=.C("UGM",z=z,v=v,aU=aU,aL=aL, timecons=timecons, usign=usign, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # Consider a UGM with variable drift rate and also variable non decision time. 
         UGMSvSt={
           out=.C("UGMSv",z=z,v=v,eta=eta,aU=aU,aL=aL, timecons=timecons, usign=usign, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # Suggestion by somebody like paul Cisek who suggested adding an intercept to the urgency model to 
         # better describe the RTs
         # The final mega model, adding variability and a slope for the data
         bUGMSvSt={
           out=.C("bUGMSv",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons=timecons,
                  usign=usign,intercept=intercept, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # Var intercept, Var Ter
         bUGMSvSbSt={
           out=.C("bUGMSvSb",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons=timecons,
                  usign=usign,intercept=intercept,ieta=ieta, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         UGMallVar={
           out=.C("UGMSvallVar",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, intercept=intercept,ieta=ieta,timecons_var=timecons_var,
                  usign_var=usign_var, s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
           
         }, 
         
         uDDMSvSbSt={
           out=.C("uDDMSvSb",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, intercept=intercept,ieta=ieta,
                  usign_var=usign_var, s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         }, 
         uDDMSvSt={
           out=.C("uDDMSv",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, 
                  intercept=intercept, usign_var=usign_var,s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         }
         # Example to get a collapsing UGM up and running.
     
         # Now calibrate variable slope for the data
         
  )
  rts[rts<0]=0
  list(rts=rts,resps=out$resp)
} 


makeparamlist=function(params,fitUGM,ncohs, zeroCoh=FALSE) 
{
  # transform parameter vector from obj into usable form for getpreds
  #   different parameter settings for DDM and DDM+UGM models
  if(zeroCoh)
  {
    v=c(0,params[paste("v",2:length(ncohs),sep="")])
  }
  else
  {
    v=c(params[paste("v",1:length(ncohs),sep="")])
  }
  aU=params["aU"]
  Ter=params["Ter"]
  
  if(fitUGM < 0) {
    aL=0
    z=aU/2
  } else {
    aL=-aU
    z=0
  }
  eta=params["eta"]
  intercept = params["intercept"]
  st0 = params["st0"]
  ieta = params["ieta"]
  zmin = params["zmin"]
  zmax = params["zmax"]
  
  timecons_var = params["timecons_var"]
  usign_var = params["usign_var"]
  
  sx = params["sx"]
  sy = params["sy"]
  delay = params["delay"]
  
  k = params["k"]
  aprime = params["aprime"]
  lambda = params["lambda"]
  
  
  # Use some scaling and delay parameters as well for the analysis of urgency models
  list(v=v,eta=eta,aU=aU,aL=aL,z=z,Ter=Ter, intercept=intercept,st0=st0, ieta=ieta, zmin = zmin, 
       zmax = zmax, timecons_var = timecons_var, usign_var = usign_var,
       sx=sx, sy=sy, delay=delay, k=k, aprime = aprime, lambda = lambda)  
  #,contp=contp,timecons=timecons,usign=usign)
}

