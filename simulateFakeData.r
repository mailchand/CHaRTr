# demo script for generating data from one of the models 
# that will be fit with the other model
source("chartr-helperfunctions.r")
source("simulators.r")
nCoh = 5; # Create data with 7 coherence
nmc = 500; # Take 500 trials for simulations

model = "bUGMSv"
for(nSubj in c(2)){
  fP = paramsandlims(model,nCoh, fakePars = TRUE)
  currParams = fP$fakeParams
  currParams["aU"] = 4900;
  currParams["eta"] = 5;
  currParams["intercept"] = 200 + rnorm(1,0,1);

  dat = simulateRTs(model, currParams , n=nmc, nds=nCoh)
  
  saveFileName=paste("Example4/","Subj",nSubj,sep="");
  save(file=saveFileName,dat, currParams, genModel=model);
}

