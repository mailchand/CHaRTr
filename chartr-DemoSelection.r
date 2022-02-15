rm(list=ls())
source("chartr-ModelSelection.r")


VERBOSE = TRUE;

### Example usage ###
# 1. First load the models into the R workspace. taken from modelSuccess.r
# We could probably make a simpler version of this step, so that the 
# emphasis remains on the second step. 

dataDir = "colgrid/"
resultsDir = 'colgrid_Fits/'
subjnam = "Olaf.Rdata"
# subjnam = "Tiberius.Rdata"

load(paste(dataDir,subjnam,sep=""))
N = sum(dat$n)


allValidModels = returnListOfModels()
modelList = unname(allValidModels$modelNames)


modelList = c('DDM','DDMSv','DDMSvSz',
              'uDDM','uDDMSvSb','uDDMSv','uDDMSvSbSu','uDDMSbSu');
              # 'nluDDM','nluDDMSvSb','nluDDMSbSu')
modelList = c('DDM','DDMSvSz','uDDMSbSu','nluDDMSbSu','cDDMSvSz')

# modelList = c('DDMSvSt','DDMSvSzSt','DDMSt','nluDDMSbSu','uDDMSbSu')

modelOutput=list()
# load each model's output and store in modelOutput
nreps = 15;
allRuns = letters[c(1:nreps)];
tempReObj = seq(1,length(allRuns))

modelResults = data.frame(matrix(0,length(modelList), nreps+1));
rownames(modelResults) = modelList;
validNames = allRuns;
validNames[length(allRuns)+1] = "best";
colnames(modelResults) = validNames;

for(b in seq(1,length(modelList)))
{
  
  model = modelList[b]
  # Pick best fit from the lot
  bestfit=-Inf ; uselet=NULL # Fits cannot be worse than -Inf
  
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
      modelResults[model, fnam] = out$reobj;
      if(VERBOSE)
      {
        cat(paste(fnam, ":", round(out$reobj,0), ","));
        # cat(sprintf("%3.2f",out$pars["v1"]))
      }
      
      # Pick best model fit from each iteration
      if(out$reobj>bestfit)
      {
        # If fit for the current letter is better than the other fit, then 
        # use this letter

        bestfit=out$reobj ; 
        uselet=fnam;
        modelOutput[[model]]=out;
        modelResults[model,"best"] = bestfit;
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

s = order(-modelResults[,"best"])
modelResults = modelResults[s,]

useggplot =TRUE;

if(useggplot)
{
  source("prettyModelPlot.r")
}else
{
  par(mfrow=c(1,2))
  source("standardModelPlot.r")
}

# 
