rm(list=ls())

source("modelSelection.r")

### Example usage ###
# 1. First load the models into the R workspace. taken from modelSuccess.r
# We could probably make a simpler version of this step, so that the 
# emphasis remains on the second step. 
if(1)
{
  dataDir = "Example2/"
  resultsDir = 'Example2_Fits/'
  subjnam = "Subj1"
}else
{
  dataDir = "colgrid/"
  resultsDir = 'FinalModelFits/'
  subjnam = "Tiberius.Rdata"
}
load(paste(dataDir,subjnam,sep=""))
# fnam='b'   # -> Chand, remind me to tell you about the idea behind fnam 
N = sum(dat$n)

origModel = model

par(mfrow=c(1,2))

allValidModels = returnListOfModels()
modelList = unname(allValidModels$modelNames)
modelOutput=list()
# load each model's output and store in modelOutput
nreps = 5;
allRuns = letters[1:nreps];
tempReObj = seq(1,length(allRuns))
for(b in seq(1,length(modelList)))
{
  model = modelList[b]
  # Pick best fit from the lot
  bestfit=-Inf ; uselet=NULL # Fits cannot be worse than -Inf
  
  for(m in seq(1,length(allRuns)))
  {    
    fnam = allRuns[m];
    fileName = paste(resultsDir,subjnam,"-",model,"-",fnam,sep="") ;
    if(file.exists(fileName))
    {
      load(fileName)
      
      # Pick best model fit from each iteration
      if(out$reobj>bestfit) 
      {
        bestfit=out$reobj ; 
        uselet=fnam;
        modelOutput[[model]]=out;
      }
      # Finish selecting
    }
    else
    {
      print(paste("Model:",model,'-',fnam, " not found",sep=""))
    }
  }
}


#legend(0.8,0.4, legend=c("Orig","Estimated"),col=c("dodgerblue1","orange"),
#      lty=1,lwd=4, bty="n")