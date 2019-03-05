# Plot parameters of these models
rm(list=ls())
source("chartr-helperfunctions.r")
VERBOSE = TRUE;

dataDir = "Example2/"
resultsDir = 'Example2_NewFits/'
subjnam = "Subj1"

load(paste(dataDir,subjnam,sep=""))
N = sum(dat$n)

allValidModels = returnListOfModels()
modelList = unname(allValidModels$modelNames)
modelOutput=list()
# load each model's output and store in modelOutput
nreps = 5;
allRuns = letters[1:nreps];
tempReObj = seq(1,length(allRuns))


model = "DDMSvSt"
cat(sprintf("\n%15s: ", model))

# cat(paste("\n", "Model:", model,"\t"));
for(m in seq(1,length(allRuns)))
{    
  fnam = allRuns[m];
  fileName = paste(resultsDir,subjnam,"-",model,"-",fnam,sep="") ;
  if(file.exists(fileName))
  {
    load(fileName)
    if(m==1)
    {
      parFrame = data.frame(matrix(0L, nrow = nreps, ncol = length(out$pars)+1));
      colnames(parFrame) = c(names(out$pars),"fit");
      rownames(parFrame) = allRuns;
    }
    parFrame[fnam,] = c(out$pars, out$reobj);
    

    # Finish selecting
  }
  else
  {
    print(paste("Model:",model,'-',fnam, " not found",sep=""))
  }
}


nParameters = length(out$pars);
nRows = 2;
nCols = ceiling(nParameters/nRows)
par(mfrow=c(nRows, nCols))
parnames = colnames(parFrame)
for(m in seq(1,nParameters))
{
  barplot(parFrame[,parnames[m]], las=2, ylab = parnames[m])

}  





