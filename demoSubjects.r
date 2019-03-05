rm(list=ls())

source("modelSelection.r")

### Example usage ###
# 1. First load the models into the R workspace. taken from modelSuccess.r
# We could probably make a simpler version of this step, so that the 
# emphasis remains on the second step. 
dataDir = "colgrid/"
resultsDir = 'FinalModelFits/'


allSubjects = dir(dataDir)
# figLayout = rbind(0,c(0,1,0,2,0), c(0,3,0,4,0), c(0,5,0,6,0), c(0,7,0,8,0), c(0,9,0,10,0),0)
# layout(figLayout,h=c(.4,1,.1,1,.32),w=c(.5,1,.1,1,.05))
# layout(figLayout)

par(mfrow=c(2,length(allSubjects)))


# layout.show(10)
for(subjnam in allSubjects)
{
  load(paste(dataDir,subjnam,sep=""))
  fnam='b'   # -> Chand, remind me to tell you about the idea behind fnam 
  N = sum(dat$n)
  
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
    for(m in seq(1,length(allRuns))){    
      
      fnam = allRuns[m];
      
      fileName = paste(resultsDir,subjnam,"-",model,"-",fnam,sep="") ;
      
      bestfit=-Inf ; uselet=NULL
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
  
  # 2. Calculate AIC, BIC, AIC weights, BIC weights
  # Useful for model averaging and model selection.
  ms=modelSelection(models=modelOutput, data=dat)
  
  # simple plot to show differnces between models
  par(mar=c(15,4,4,2))
  x = ms$AIC - ms$AIC[1];
  orderV = order(x);
  currentModels = rownames(ms);
  # barplot(x[orderV],names.arg=currentModels[orderV], las=2)  # if names are too long this won't display correctly
  
  
  # A second example. Let's suppose we want to compare a subset of the models, not all of them; 
  # e.g., Cisek urgency vs Ditterich urgency vs. Ratcliff model, First, create a character vector of the models you 
  # want to compare. The names must exactly match the names as they appear in the modelOutput list.
  # otherwise you will get odd errors
  
  modelsToTest=currentModels[orderV[seq(1,6)]]
  
  # call the modelSelection function while subsetting from the modelOutput list
  ms = modelSelection(models=modelOutput[modelsToTest], data=dat)
  par(mar=c(15,3,3,2))
  barplot(ms$AIC-ms$AIC[1],names.arg=rownames(ms), las=2)  # if names are too long this won't display correctly
  
  # barplot(ms$AIC-ms$AIC[1],names.arg=rownames(ms), las=2)  # if names are too long this won't display correctly)
}