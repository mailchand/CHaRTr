# Test the amount of time needed for the various models

rm(list=ls())
require(gridExtra);

library(ggplot2)
source("chartr-ModelSelection.r")
VERBOSE = TRUE;

allValidModels = returnListOfModels()
modelList = unname(allValidModels$modelNames)
usemodel = modelList


dataDir = "caseStudy2";
resultsDir = "caseStudy2_FastFits/"

runs = seq(6,10)-5;

nreps = length(runs);

timingResults = data.frame(matrix(0,length(modelList), nreps+1));




snams = dir(dataDir)
snams = c('Subj1','Subj2','Subj3')

allTimingResults = data.frame(matrix(0,length(modelList),length(snams)));
colnames(allTimingResults) = snams;
allNums = data.frame(matrix(0,length(snams),length(runs)+1))
rownames(allNums) = snams;
cnt = 1;

npars = c();

validNames = letters[runs];
validNames[length(validNames)+1] = "best";
colnames(allNums) = validNames;

for(s in snams)
{
  subjnam = s
  allRuns = letters[runs];
  nreps = length(allRuns);
  
  timingResults = data.frame(matrix(0,length(modelList), nreps+1));
  rownames(timingResults) = modelList;

  
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
        
        npars[model] = length(out$pars);
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
  allNums[s,] = colSums(timingResults)
  allTimingResults[,subjnam]=rowMeans(timingResults);
}

data = c()
cnt = 1;
for(s in snams)
{
  for(b in allRuns)
  {
        data[cnt] = allNums[s,b]
        cnt = cnt +1 
  }
}


rownames(allTimingResults) = rownames(timingResults)

allTimingResults["meanV"] = rowMeans(allTimingResults);
allTimingResults = transform(allTimingResults, SEV=apply(allTimingResults/3600,1, sd, na.rm = TRUE)/sqrt(5));
allTimingResults["npars"] = npars;


currentModels = rownames(allTimingResults);
x = allTimingResults["meanV"]/3600
orderV = order(x)
dataFrame = data.frame(V=unname(x),Ve = unname(allTimingResults["SEV"]), npars=allTimingResults["npars"], modelId = as.factor(currentModels));
dataFrame$modelId = factor(dataFrame$modelId, 
                           levels = dataFrame$modelId[order(dataFrame$V)])

p1=ggplot(dataFrame, aes(modelId,V), stat="identity");
p1=p1 + geom_col(alpha=0.3);
# p1 = p1 + geom_segment(aes(y=0,yend=V,x=modelId,xend=modelId));
p1=p1 + geom_errorbar(aes(ymin=V-Ve,ymax=V+Ve), width=0.2, position=position_dodge(.9));
p1=p1 + coord_flip() + theme_minimal()
p1=p1 + ggtitle(paste("Average Time"));
p1=p1 + theme(text = element_text(size=14));
p1=p1 + theme(aspect.ratio = 1) + labs(y = "Run time (Hrs)", x='Model');
p1=p1 + theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank());


p2 = ggplot(dataFrame, aes(npars,V), stat="identity") +   geom_jitter(alpha=0.4, position = position_jitter(w=0.4,h=0.2, seed=1), size=5);
# p2 = p2 +  geom_errorbar(aes(ymin=V-Ve,ymax=V+Ve), width=0.2, position=position_dodge(.9));
p2 = p2 + theme_minimal()  + geom_text(label=rownames(dataFrame), position = position_jitter(w=0.5,h=0,seed=1));
p2 = p2 +  theme(aspect.ratio = 1) + theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank());
p2 = p2 + theme(text = element_text(size=14));
p2 = p2 + theme(axis.line.x = element_line(color="black", size = .5),
      axis.line.y = element_line(color="black", size = .5), axis.ticks = element_line(size=0.5),axis.ticks.length = unit(.25, "cm") )
p2 = p2 +  labs(y = "Run time (Hrs)", x='Number of parameters');
grid.arrange(p1, p2, nrow=2, ncol=2)