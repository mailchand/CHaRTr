# Plot parameters of these models
#
#   This file allows you to plot all the 
#
#
#
#
#
# CC, 5th March 2019 - Boston University, Boston, MA

rm(list=ls())
require(ggplot2)
require(gridExtra)


source("chartr-HelperFunctions.r")

dataDir = "caseStudy1/"
resultsDir = 'caseStudy1_Fits/'
subjnam = "Subj1"
modelToPlot = "cDDMSvSt"
VERBOSE = TRUE;



load(paste(dataDir,subjnam,sep=""))
N = sum(dat$n)


allValidModels = returnListOfModels()
modelList = unname(allValidModels$modelNames)
modelOutput=list()

# load each model's output and store in modelOutput
nreps = 5;
allRuns = letters[1:nreps];
tempReObj = seq(1,length(allRuns))

cat(sprintf("\n%15s: ", modelToPlot))

for(m in seq(1,length(allRuns)))
{    
  fnam = allRuns[m];
  fileName = paste(resultsDir,subjnam,"-",modelToPlot,"-",fnam,sep="") ;
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


# Now plot all the parameters associated with the five runs
nParameters = length(out$pars);
nCols = 5;
nRows = ceiling(nParameters/nCols)
par(mfrow=c(nRows, nCols))
parnames = colnames(parFrame)

plots = list();
for(m in seq(1,nParameters))
{
  S = data.frame(data = parFrame[,parnames[m]], runs=seq(1,5));
  plots[[m]] = ggplot(S, aes(runs,data), stat="identity") + xlab('') + ylab('') + 
    geom_col(alpha=0.6)  + coord_flip() + theme_minimal()+ ggtitle(parnames[m]) + theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank()) + 
  theme(axis.line.x = element_line(colour = "gray"),axis.ticks.x = element_line("gray", size=.25), axis.ticks.length = unit(.25,"cm")) + theme(text = element_text(size=14));
  
}  

do.call("grid.arrange", c(plots, ncol=nCols))




