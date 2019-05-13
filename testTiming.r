# Test the amount of time needed for the various models
rm(list=ls())
source("chartr-ModelSelection.r")
VERBOSE = TRUE;

allValidModels = returnListOfModels()
modelList = unname(allValidModels$modelNames)
usemodel = modelList

dataDir = "caseStudy1";
resultsDir = "caseStudy1_Fits/"

subjnam = "Subj2"
allRuns = letters[6:11];
nreps = length(allRuns);

timingResults = data.frame(matrix(0,length(modelList), nreps+1));




rownames(timingResults) = modelList;
validNames = allRuns;
validNames[length(allRuns)+1] = "best";

colnames(timingResults) = validNames;

for(b in seq(1,length(modelList)))
{
  
  model = modelList[b]
  # Pick best fit from the lot
  bestTime=Inf ; uselet=NULL # Fits cannot be worse than -Inf
  
  if(VERBOSE)
    cat(sprintf("\n%15s: ", model))
  
  # cat(paste("\n", "Model:", model,"\t"));
  for(m in seq(1,length(allRuns)))
  {    
    fnam = allRuns[m];
    fileName = paste(resultsDir,subjnam,"-",model,"-",fnam,sep="") ;
    if(file.exists(fileName))
    {
      
      load(fileName)
      
      currTime = unname(out$timings[2])
      timingResults[model, m] = currTime
      
      # Pick best model fit from each iteration
      if(currTime < bestTime)
      {
        # If fit for the current letter is better than the other fit, then 
        # use this letter
        
        bestTime= currTime ; 
        uselet=fnam;
        timingResults[model,"best"] = bestTime;
      }
      # Finish selecting
    }
    else
    {
      print(paste("Model:",model,'-',fnam, " not found",sep=""))
    }
    
  }
  if(VERBOSE)
    cat(paste("choosing ", uselet))
}