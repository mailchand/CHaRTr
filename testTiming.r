# Test the amount of time needed for the various models
rm(list=ls())

library(ggplot2)
source("chartr-ModelSelection.r")
VERBOSE = TRUE;

allValidModels = returnListOfModels()
modelList = unname(allValidModels$modelNames)
usemodel = modelList


dataDir = "caseStudy1";
resultsDir = "caseStudy1_Fits/"

subjnam = "Subj1"
allRuns = letters[7:11];
nreps = length(allRuns);

timingResults = data.frame(matrix(0,length(modelList), nreps+1));




snams = dir(dataDir)

allTimingResults = data.frame(matrix(0,length(modelList),length(snams)));
colnames(allTimingResults) = snams;
for(s in snams)
{
  subjnam = s
  allRuns = letters[7:10];
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

  allTimingResults[,subjnam]=rowMeans(timingResults);
}
rownames(allTimingResults) = rownames(timingResults)

currentModels = rownames(allTimingResults);
x = rowMeans(allTimingResults)/3600;
orderV = order(x)
dataFrame = data.frame(V=x, modelId = as.factor(currentModels));
dataFrame$modelId = factor(dataFrame$modelId, 
                           levels = dataFrame$modelId[order(dataFrame$V)])

p1=ggplot(dataFrame, aes(modelId,V), stat="identity");
p1=p1 + geom_col(alpha=0.6) + geom_hline(yintercept=0, linetype="dashed")
p1=p1 + coord_flip() + theme_minimal()
p1=p1 + ggtitle(paste("All Models, ",subjnam));
p1=p1 + theme(text = element_text(size=14));
p1=p1 + theme(aspect.ratio = 1);
p1=p1 + theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank());
show(p1)
