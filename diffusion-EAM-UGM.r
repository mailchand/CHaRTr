# diffusion model - compare fixed and collapsing bounds #

# load C functions
dyn.load("chartr-modelspec.so")

# convert into a table object for the future so that life is a bit easier
# 9 models and counting. This is a little bit ridiculous. But good resource for the future
# 
returnListOfModels = function()
{
  modelList = c(
                "DDM",                    # -1
                "DDMSv",                  # -2
                "DDMSvSt",                # -3
                "DDMdSvSt",               # -6
                "DDMSvSzSt",              # -7
                "DDMSvSz",                # -8
                "DDMcSvSt",               # -9
                "DDMdSv",                 # -10
                "UGM",                    #  1
                "UGMSt",                  #  2 
                "UGMSv",                  #  3
                "UGMSvSt",                #  4  
                "UGMbSv",                 #  5       
                "UGMbSvSt",               #  6       
                "UGMbSvSb",               #  7       
                "UGMbSvSbSt",             #  8
                "DDMuSvSb",               #  10 // A simple urgency model that has no time constant or low pass filtering
                "DDMuSv",                 #  11
                "DDMuSvSbSt",             #  12 
                "DDMuSvSt"                #  13
  );                             
  
  
  
  modelIds = c(-1,-2,-3,-6,-7, -8, -9,-10, 1,2,3,4,5,6,7,8,10,11, 12, 13)
  modelNames <- setNames( modelList, modelIds)
  names(modelIds) = modelList
  list(modelIds=modelIds,modelNames=modelNames)
}


# This function returns an integer that evaluates the 
paramsandlims=function(model, nds, fakePars=FALSE)
{
  # Non UGMs will have values less than 0
  # UGMs will have values greater than 0
  listOfModels = returnListOfModels()
  
  fitUGM = listOfModels$modelIds[model]
  
  upper_v_ddm = 0.6;
  upper_aU_ddm = 1;
  upper_Ter = 0.6;
  upper_st0 = 0.6;
  upper_eta = 0.35;
  upper_zmin = 1;
  upper_zmax = 1;
  
  
  upper_v_urgency = 40;
  upper_aU_urgency = 20000;
  upper_eta_urgency =50;
  upper_intercept = 5000;
  upper_ieta = 5000;
  upper_usign_var = 20;
  upper_timecons_var = 2000;
  
  
  parUppersDDM = c(rep(upper_v_ddm,nds), upper_aU_ddm, upper_Ter, upper_eta, upper_st0,
                    upper_zmin, upper_zmax, 10, 10, 0.5, 10,1,3);
  TempNames = c(paste("v",1:(nds),sep=""),"aU","Ter","eta", "st0",
                                   "zmin","zmax","sx","sy","delay","lambda", "aprime","k");
  names(parUppersDDM) = TempNames;
  
  fakeParsDDM = c(seq(0.04, 0.4, length.out = nds), 0.08, 0.3, 0.1, 0.15, 0.08, 0.12, 9, 9,0.14, 5, 0.3, 3);
  names(fakeParsDDM) = TempNames;
    
  parUppersUGM = c(rep(upper_v_urgency,nds), upper_aU_urgency, upper_Ter, upper_eta_urgency, upper_st0,
                    upper_intercept, upper_ieta, upper_timecons_var, upper_usign_var)
  TempNames = c(paste("v",1:(nds),sep=""),"aU","Ter","eta", "st0",
                          "intercept","ieta","timecons_var","usign_var");
  fakeParsUGM = c(seq(1.5, 15,  length.out = nds) + 0.5*rnorm(nds), 12000, 0.3, 4, 0.1, 1000, 600, 200,1 );
  names(fakeParsUGM) = TempNames;
  names(parUppersUGM)  = TempNames;
  
  switch(model, 
         DDM={ # 7 coherences, an upper bound, and a lower bound, symmetric bounds
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter")
           print("Vanilla diffusion model with no bells and whistles")
         },
         DDMSv={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta")
           print("Diffusion model with some drift variance")
         },
         DDMSvSz={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","zmin","zmax")
           print("Diffusion model, Variable Baseline, Variable Movement Time")
         },
         DDMSvSzSt={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","zmin","zmax","st0")
           print("Diffusion model, Variable Baseline, Variable Movement Time")
         },
         ratcliff={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","zmin","zmax","st0")
           print("Ratcliff model with variable movement time")
         },
         DDMSvSt={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","st0")
           print("Diffusion model with some drift variance and variable movement time")
         },
         DDMdSvSt={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","st0","sx","sy","delay")
           print("Diffusion model with some drift variance and variable movement time and most importantly urgency")
         },
         
         DDMdSv={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","sx","sy","delay")
           print("Diffusion model with some drift variance and variable movement time and most importantly urgency")
         },
         
         DDMcSvSt={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","st0","lambda","aprime","k")
           print("Diffusion model with some drift variance, variable movement time and collapsing bounds")
         },
         
         DDMcfkSvSt={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","st0","lambda","aprime")
           print("Diffusion model with some drift variance, variable movement time and collapsing bounds")
         },
         
         
         UGM={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter")
            print("Urgency Gating Model")
         },
         UGMSt={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","st0")
           print("Urgency gating model with some drift variance and variable residual movement time")
         },
         UGMSv={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta")
           print("Urgency Gating With Drift Variance")
           
         },
         UGMSvSt={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","st0")
           print("Urgency Gating, drift variance, variable residual movement time")
         },
         UGMbSv={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","intercept")
           print("Urgency Gating with an intercept")           
         }, 
         UGMbSvSt={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","intercept", "st0")
           print("Diffusion model with some drift variance, urgency gating, variable residual 
                 movement time, and an intercept for urgency gating")
         }, 
         UGMbSvSb={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","intercept","ieta")
           print("Diffusion model with some drift variance, urgency gating, variable residual 
                 movement time, and an intercept for urgency gating")
           
         },  
         UGMbSvSbSt={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","intercept","ieta","st0")
           print("Diffusion model with some drift variance, urgency gating, variable residual 
                 movement time, and an intercept for urgency gating")
         }, 
         
         # A UGM model with variability in all the parameters so that the time constant is not causing unnecessary issues       
         UGMSvallVar={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","intercept","ieta","st0","timecons_var","usign_var")
           print("Full UGM model with variability in time constants and slope parameter")
         }, 
         
         # Urgency signal in PMd
         DDMuSvSb={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","intercept","ieta","usign_var")
           print("DDM with Urgency and no gating and fixed Ter")
         },
         
         DDMuSv={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","intercept","usign_var")
           print("DDM with Urgency and no gating, constant slope, and fixed Ter")
         }, 
         DDMuSvSbSt={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","intercept","ieta","usign_var","st0")
           print("DDM with Urgency and no gating and variable Ter")
         },
         
         DDMuSvSt={
           parnames=c(paste("v",1:(nds),sep=""),"aU","Ter","eta","intercept","usign_var","st0")
           print("DDM with Urgency and no gating, constant slope, and variable Ter")
         } 
  )  
  
  uppers = seq(1,length(parnames));
  for(i in seq(1,length(parnames))){
    if(fitUGM < 0)
        uppers[i] = parUppersDDM[parnames[i]]  
    else
        uppers[i] = parUppersUGM[parnames[i]]  
    
  }
  lowers=rep(0,length(uppers))
  
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
                    lambda = lambda, aprime = aprime, k = k, VERBOSE=FALSE) {
  
  dyn.load("chartr-modelspec.so")
  rts <- resps <- numeric(nmc)
  
  listOfModels = returnListOfModels()
  
  modelName = unname(listOfModels$modelNames[as.character(fitUGM)])
  if(VERBOSE) print(modelName);
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
         UGMbSv={
           out=.C("UGMSvintercept",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons=timecons,
                  usign=usign,intercept=intercept, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000) + Ter;
         },
         
         # Assume that this intercept is variable as well. Assume constant slope though (1 in case of Cisek)
         UGMbSvSb={
           out=.C("UGMSvVarintercept",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons=timecons,
                  usign=usign,intercept=intercept,ieta=ieta, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000) + Ter;
         },
         
         # Now assume a Urgency model where you fit the intercept, variability in this intercept and a variable slope
         DDMuSvSb={
           out=.C("DDMSvUrgency",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, intercept=intercept,ieta=ieta,
                  usign_var=usign_var, s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+Ter;
         }, 
         
         # Now assume a Urgency model where you fit the intercept, and a slope term. 
         DDMuSv={
           out=.C("DDMSvUrgencySimple",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, 
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
         DDMdSvSt={
           out=.C("DDMSvStDitterich",z=z,v=v,eta=eta, delay=delay, sx=sx, sy=sy, aU=aU,aL=aL,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep)
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
        
        DDMdSv={
          out=.C("DDMSvDitterich",z=z,v=v,eta=eta, delay=delay, sx=sx, sy=sy, aU=aU,aL=aL,
                 s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep)
          rts=out$rt+Ter;
          
        },
        
         
        DDMcfkSvSt={
          out=.C("DDMSvCollapse",z=z,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL, aprime = aprime, k=3,
                 s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
          rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
        },
        
        DDMcSvSt={
          out=.C("DDMSvCollapse",z=z,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL, aprime = aprime, k=k,
                 s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep)
          rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
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
         UGMbSvSt={
           out=.C("UGMSvintercept",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons=timecons,
                  usign=usign,intercept=intercept, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # Var intercept, Var Ter
         UGMbSvSbSt={
           out=.C("UGMSvVarintercept",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons=timecons,
                  usign=usign,intercept=intercept,ieta=ieta, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         UGMallVar={
           out=.C("UGMSvallVar",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, intercept=intercept,ieta=ieta,timecons_var=timecons_var,
                  usign_var=usign_var, s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
           
         }, 
         
         DDMuSvSbSt={
           out=.C("DDMSvUrgency",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, intercept=intercept,ieta=ieta,
                  usign_var=usign_var, s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         }, 
         DDMuSvSt={
           out=.C("DDMSvUrgencySimple",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, 
                  intercept=intercept, usign_var=usign_var,s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         }
         
         
         # Now calibrate variable slope for the data
         
  )
  rts[rts<0]=0
  list(rts=rts,resps=out$resp)
} 


makeparamlist=function(params,fitUGM,ncohs) 
{
  # transform parameter vector from obj into usable form for getpreds
  #   different parameter settings for DDM and DDM+UGM models
  # v=c(0,params[paste("v",2:6,sep="")])
  v=c(params[paste("v",1:7,sep="")])
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

