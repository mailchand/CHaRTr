dyn.load("chartr-ModelSpecFast.so")

# 24 models 
#' Generate a list of models to be used for fitting RTs and Choice
#'
#' @return A list of models
#' @examples
#' modelList = returnListOfModels()
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
            "uDDMSvSb",                #  9 
            "uDDMSv",                  #  10
            "uDDMSvSbSt",              #  11 
            "uDDMSvSt"                 #  12
  );
  modelIds = c(seq(-1,-12), seq(1,12));
  modelNames <- setNames( modelList, modelIds)
  names(modelIds) = modelList
  list(modelIds=modelIds,modelNames=modelNames)
}


#' printModels
#'
#' @return prints models
#' @export 
#'
#' @examples
#' printModels()
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




#' paramsandlims
#'
#' @param model -- name of the model that is typically generated using list of models
#' @param nds   -- Number of stimulus levels used for decision-making
#' @param fakePars -- Generate fakeparameters for simulations
#' @param nstart  -- Sometimes there is a true zero sensory evidence stimulus
#'
#' @return list with fields parnames, upper limits, lower limits, variable of whether it is a UGM class of models or DDM class of models, and fakeparams if generated.
#' @export
#'
#' @examples
#' paramsandlims(model='DDM',nds=7,fakePars=FALSE,nstart=1)
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





#' Title
#'
#' @param v drift rates
#' @param eta Standard deviation for the drift rate
#' @param aU upper bound
#' @param aL lower bound
#' @param Ter Non decision time
#' @param intercept Intercept for the UGM
#' @param ieta range of the uniform distribution for the UGM intercept
#' @param st0 range of the uniform distribution for the reaction times
#' @param z Startpoint of the UGM.
#' @param zmin min range of start point
#' @param zmax max range of start point
#' @param nmc number of trials to run
#' @param dt timestep: .001 s for DDM, 1 ms for UGM
#' @param stoch.s a constant
#' @param maxTimeStep - maximum number of timesteps for the simulation
#' @param fitUGM - whether it is a UGM or not and what the modelID is used for back identifying model name. This modifies the timesteps
#' @param timecons- Used only for fixed UGMs
#' @param usign - a constant typically 1 which tells you how the urgency signal grows
#' @param timecons_var 
#' @param usign_var 
#' @param sx - Parameter for the Ditterich urgency function
#' @param sy - Parameter for the Ditterich urgency function
#' @param delay - Delay parameter for the Ditterich urgency function
#' @param lambda - Parameter for the collapsing bounds
#' @param aprime - Parameter for the collapsing bounds
#' @param k - Parameter for the collapsing bounds
#' @param VERBOSE 
#'
#' @return list with rts and responses
#' @export
#'
#' @examples
#' diffusionC(v=0.4, eta=0.05, aU=0.1,aL=0, Ter=0.3,z=0.05,nmc=1000,dt=0.001,stoch.s=0.1,maxTimeStep=4,fitUGM=-12)
diffusionC=function(v,eta,aU,aL,Ter,intercept,ieta,st0, z, zmin, zmax, nmc, dt,stoch.s,
                    maxTimeStep,fitUGM,timecons,usign=1, timecons_var = timecons_var, usign_var = usign_var, 
                    sx=sx, sy=sy, delay=delay, lambda = lambda, aprime = aprime, k = k, VERBOSE=FALSE, FASTRAND=TRUE,
                    nLUT=nLUT, LUT = LUT) 
  # v - 
  # eta - 
  # aU - 
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
  if(FASTRAND)
    dyn.load("chartr-ModelSpecFast.so")
  else
    dyn.load("chartr-ModelSpec.so")
  
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
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep, 
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT))
           rts=out$rt + Ter;
         },
         
         # Same as the vanilla diffusion model, except with a modification of the 
         # model to include some variability in the drift rates.
         DDMSv={
           out=.C("DDMSv",z=z,v=v,eta=eta,aU=aU,aL=aL, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt + Ter;
         },
         
         # DDM Model with urgency gating
         # 
         UGM={
           out=.C("UGM",z=z,v=v,aU=aU,aL=aL, timecons=timecons, usign=usign, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=(out$rt/1000) + Ter;
         },
         
         # DDM model now adding variability of the drift rate
         UGMSv={
           out=.C("UGMSv",z=z,v=v,eta=eta,aU=aU,aL=aL, timecons=timecons, usign=usign, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep, 
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=(out$rt/1000) + Ter;
         },
         
         
         # DDM Eta UGM model with an intercept term for the urgency signal
         bUGMSv={
           out=.C("bUGMSv",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons=timecons,
                  usign=usign,intercept=intercept, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=(out$rt/1000) + Ter;
         },
         
         # Assume that this intercept is variable as well. Assume constant slope though (1 in case of Cisek)
         bUGMSvSb={
           out=.C("bUGMSvSb",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons=timecons,
                  usign=usign,intercept=intercept,ieta=ieta, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=(out$rt/1000) + Ter;
         },
         
         # Now assume a Urgency model where you fit the intercept, variability in this intercept and a variable slope
         uDDMSvSb={
           out=.C("uDDMSvSb",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, intercept=intercept,ieta=ieta,
                  usign_var=usign_var, s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=(out$rt/1000)+Ter;
         }, 
         
         # Now assume a Urgency model where you fit the intercept, and a slope term. 
         uDDMSv={
           out=.C("uDDMSv",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, 
                  intercept=intercept, usign_var=usign_var,s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=(out$rt/1000)+Ter;
         },
         
         
         # -------------------------------- Models without any variable movement time
         
         
         # Same as the model with drift variance but incorporation of a variable
         # time for the non decision time (See Ratcliff and Tuerlinckx, 2002)
         
         DDMSvSt={
           out=.C("DDMSv",z=z,v=v,eta=eta,aU=aU,aL=aL, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # This is a bit of a misnomer. The Ratcliff model always assumes a variable residual movement time
         ratcliff={
           out=.C("ratcliff",zmin=zmin, zmax=zmax,v=v,aU=aU,aL=aL,eta=eta,s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=(out$rt) +runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         DDMSvSz={
           out=.C("DDMSvSz",zmin=zmin, zmax=zmax,v=v,aU=aU,aL=aL,eta=eta,s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt+Ter;
         },
         # Identical to the ratcliffSt model
         DDMSvSzSt={
           out=.C("DDMSvSz",zmin=zmin, zmax=zmax,v=v,aU=aU,aL=aL,eta=eta,s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # This is the ditterich model that assumes a complex shape for the bound collapse
         dDDMSvSt={
           out=.C("dDDMSv",z=z,v=v,eta=eta, delay=delay, sx=sx, sy=sy, aU=aU,aL=aL,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         dDDMSv={
           out=.C("dDDMSv",z=z,v=v,eta=eta, delay=delay, sx=sx, sy=sy, aU=aU,aL=aL,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt+Ter;
           
         },
         
         dDDMSvSzSt={
           out=.C("dDDMSvSz",zmin=zmin,zmax=zmax,v=v,eta=eta, delay=delay, sx=sx, sy=sy, aU=aU,aL=aL,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         cfkDDMSvSt={
           out=.C("cfkDDMSv",z=z,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL,aprime=aprime,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # Replicating model from Hawkins et al. 2015
         cfkDDMSvSzSt={
           out=.C("cfkDDMSvSz",zmin=zmin,zmax=zmax,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL,aprime=aprime,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         cDDMSvSt={
           out=.C("cDDMSv",z=z,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL, aprime = aprime, k=k,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         cDDMSvSzSt={
           out=.C("cDDMSvSz",zmin=zmin, zmax=zmax,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL, aprime = aprime, k=k,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # Now do all the Collapsing bounds without the variabililty in nondecision-time
         
         
         cfkDDMSv={
           out=.C("cfkDDMSv",z=z,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL,aprime=aprime,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt+Ter;
         },
         
         # Replicating model from Hawkins et al. 2015
         cfkDDMSvSz={
           out=.C("cfkDDMSvSz",zmin=zmin,zmax=zmax,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL,aprime=aprime,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt+Ter;
         },
         
         cDDMSv={
           out=.C("cDDMSv",z=z,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL, aprime = aprime, k=k,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt+Ter;
         },
         
         cDDMSvSz={
           out=.C("cDDMSvSz",zmin=zmin, zmax=zmax,v=v,eta=eta, lambda=lambda, aU=aU,aL=aL, aprime = aprime, k=k,
                  s=stoch.s,dt=dt,response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=out$rt+Ter;
         },
         
         
         
         cUGMSvSt={
             out=.C("cUGMSv",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons=timecons,lambda=lambda,
                  k=k, aprime=aprime, s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
             rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);   
           
         },
         
         
         # Now do the urgency model with variable Ter
         UGMSt={
           out=.C("UGM",z=z,v=v,aU=aU,aL=aL, timecons=timecons, usign=usign, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # Consider a UGM with variable drift rate and also variable non decision time. 
         UGMSvSt={
           out=.C("UGMSv",z=z,v=v,eta=eta,aU=aU,aL=aL, timecons=timecons, usign=usign, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # Suggestion by somebody like paul Cisek who suggested adding an intercept to the urgency model to 
         # better describe the RTs
         # The final mega model, adding variability and a slope for the data
         bUGMSvSt={
           out=.C("bUGMSv",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons=timecons,
                  usign=usign,intercept=intercept, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         # Var intercept, Var Ter
         bUGMSvSbSt={
           out=.C("bUGMSvSb",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons=timecons,
                  usign=usign,intercept=intercept,ieta=ieta, s=stoch.s,dt=dt,
                  response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         },
         
         UGMallVar={
           out=.C("UGMSvallVar",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, intercept=intercept,ieta=ieta,timecons_var=timecons_var,
                  usign_var=usign_var, s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
           
         }, 
         
         uDDMSvSbSt={
           out=.C("uDDMSvSb",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, intercept=intercept,ieta=ieta,
                  usign_var=usign_var, s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
           rts=(out$rt/1000)+runif(n=nmc,min=Ter-st0/2,max=Ter+st0/2);
         }, 
         uDDMSvSt={
           out=.C("uDDMSv",z=z,v=v,eta=eta,aU=aU,aL=aL,timecons = timecons, usign=usign, 
                  intercept=intercept, usign_var=usign_var,s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                  rangeLow =as.integer(0), rangeHigh = as.integer(nLUT-1), randomTable = as.double(LUT));
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


# 
#
#
# 
simulateRTs = function(model, ps, nmc=50000, maxiter=10000, nds=7, showAxisLabels = FALSE, plotData=TRUE, FASTRAND=TRUE){
  # model specifies the model to simulate
  # ps passes in parameters
  # nmc and maxiter are parameters for the simulation
  
  bailouttime=5
  trlsteps=seq(0,bailouttime,.001)
  maxTimeStep=as.double(length(trlsteps))
  
  
  # For the future to speed up analyses
  interval=0.00001;
  LUT=qnorm(seq(interval,1-interval,interval));
  nLUT = length(LUT);
  
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
    
    tic(paste("Condition",v))
    tmp = diffusionC(v=ps[v], eta=ps["eta"], aU = ps["aU"], aL = aL, Ter=ps["Ter"], 
                     intercept=ps["intercept"], ieta=ps["ieta"], st0 = ps["st0"], z=z, zmin=ps["zmin"],
                     zmax=ps["zmax"], timecons_var=ps["timecons_var"], usign_var=ps["usign_var"], 
                     sx=ps["sx"], sy=ps["sy"], delay=ps["delay"], lambda=ps["lambda"], 
                     aprime=ps["aprime"], k=ps["k"], timecons=timecons, usign=usign, s=stoch.s,dt=dt,
                     n=nmc,maxTimeStep=maxTimeStep, fitUGM=fitUGM, FASTRAND=FASTRAND, nLUT = nLUT, LUT = LUT)
    toc();
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

