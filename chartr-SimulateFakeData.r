# demo script for generating data from one of the models 
# that will be fit with the other model
source("chartr-HelperFunctions.r")

nCoh = 5; # Create data with 7 coherence
nmc = 500; # Take 500 trials for simulations
saveFiles = FALSE;
saveDir = "caseStudy2"
subjectPrefix = "Subj"

model = "bUGMSv"
par(mfrow=c(2,3))
for(nSubj in seq(1,5)){
  fP = paramsandlims(model,nCoh, fakePars = TRUE)
  currParams = fP$fakeParams
  currParams["aU"] = 4900;
  currParams["eta"] = 5;
  currParams["intercept"] = 200 + 20*rnorm(1,0,1);

  dat = simulateRTs(model, currParams , n=nmc, nds=nCoh)
  
  saveFileName=paste(saveDir,subjectPrefix,nSubj,sep="");
  if(saveFiles){
   # save(file=saveFileName,dat, currParams, genModel=model);
  }
}

