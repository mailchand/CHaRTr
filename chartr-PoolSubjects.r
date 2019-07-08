rm(list=ls())

currwd = getwd();

source("chartr-FitRoutines.r")
source("chartr-PlotUtils.r")
source("chartr-HelperFunctions.r")
source("chartr-ModelSelection.r")

# load C functions for simulating various parameters
dyn.load("chartr-ModelSpecFast.so")
# load RS2002 data sets to find suitable subjects

useAIC = FALSE;


whatToRun = "cs1, fast, 200, 5000"
switch(whatToRun,
       "cs1, slow, 400, 10000"={
         dataDir = "caseStudy1";
         resultsDir = "caseStudy1_Fits/"
         runs = seq(6,10)+1;
       },
       "cs2, slow, 400, 10000"={
         dataDir = "caseStudy2";
         resultsDir = "caseStudy2_Fits/"
         runs = seq(6,10);
       },
       "cs1, fast, 400, 10000"={
         dataDir = "caseStudy1";
         resultsDir = "caseStudy1_FastFits/"
         runs = seq(1,5);
       },
       "cs2, fast, 400, 10000"={
         dataDir = "caseStudy2";
         resultsDir = "caseStudy2_FastFits/"
         runs = seq(1,5);
       },
       "cs1, fast, 200, 10000"={
         dataDir = "caseStudy1";
         resultsDir = "caseStudy1_FastFits/"
         runs = seq(1,5)+5;
       },
       "cs2, fast, 200, 10000"={
         dataDir = "caseStudy2";
         resultsDir = "caseStudy2_FastFits/"
         runs = seq(1,5)+5;
       },
       "cs1, fast, 200, 5000"={
         dataDir = "caseStudy1";
         resultsDir = "caseStudy1_FastFits/"
         runs = seq(1,5)+15;
       },
       "cs2, fast, 200, 5000"={
         dataDir = "caseStudy2";
         resultsDir = "caseStudy2_FastFits/"
         runs = seq(1,5)+15;
       }
)





whichDir =  dataDir;
subjectDir = paste(getwd(),'/', whichDir,sep='')

# set up plot for predicted distributions from the 
# Examples

# Select quantiles to plot
qps=seq(.1,.9,.2) ; 
nq=length(qps)

fnams=dir(subjectDir)
fnams = fnams[c(1,2,3,4,5)]
data=list()

# For each subject in the list of subjects to plot
for(s in fnams) {
  load(paste(subjectDir, s, sep="/"))
  data[[s]]=dat
}
# Loading the data

# 

#
allValidModels = returnListOfModels()
modelList = unname(allValidModels$modelNames)
usemodel = modelList;



# 
snams=names(data) ; 
nsubj=length(data);

# Number of repeats of the fitting

lets=letters[runs];

# for now, only get reobj value (parameters later)
landouts=array(dim=c(nsubj,length(usemodel)),dimnames=list(snams,usemodel))
landIC=array(dim=c(nsubj,length(usemodel)),dimnames=list(snams,usemodel))
landICw=array(dim=c(nsubj,length(usemodel)),dimnames=list(snams,usemodel))


landpars=list(stone=list(),stoneUGM=list())
numpars = array(length(usemodel));
modelOutput=list()

VERBOSE = TRUE;
# Load each and every model, check which model fit is best. Use best fitting model
for(s in snams) {  
  for(mod in usemodel) {
    if(VERBOSE)
      cat(sprintf("\n%15s: ", mod))
    # iterate over jobs per subject to get best fit
    bestfit=-Inf ; uselet=NULL
    for(l in lets) {
      
      fnam=paste(s,mod,l,sep="-")
      printFnam = fnam;
      fnam=paste(resultsDir,fnam,sep='')

      if(!file.exists(fnam)) next else load(fnam)
      if(out$reobj>bestfit) 
      {
        bestfit=out$reobj ; 
        uselet=l
        modelOutput[[mod]]=out;
      }
      if(VERBOSE)
        cat(paste(l, ":", round(out$reobj,0), ","));

      # rm(fnam,out)
    }
    bestFitModel = paste(s,mod,uselet,sep="-");
    bestFitModel = paste(resultsDir,bestFitModel,sep='')
    load(bestFitModel); 
    
    
    landouts[s,mod]=out$reobj
    used=paste("data",dat,sep="-") ; 
    usem=paste("model",mod,sep="-")
    landpars[[mod]]=rbind(landpars[[mod]],out$pars)
    numpars[mod] = length(out$pars)
    IC = calcIC(bestfit, numpars[mod], sum(data[[s]]$n))
    
    if(useAIC)
      landIC[s,mod] = IC$AIC
    else
      landIC[s,mod] = IC$BIC
    
    rm(out)
    if(VERBOSE)
      cat(paste("choosing ", uselet))
    rm(bestfit,uselet)
    
  
    
    # rownames(landpars[[mod]]) = snams[s]
  }
  
}

for(s in snams)
{
  landICw[s,] = pmp(landIC[s,])
}
for(mod in usemodel) {
  rownames(landpars[[mod]]) = snams
}

useggplot = TRUE;
if(useggplot)
{
  library(ggplot2)
  library(gridExtra)
  library(ggthemes)
  
  modelId = colnames(landICw);
  weights = colMeans(landICw);
  # weights = apply(landAICw, 2, FUN=median)
  orderV = order(-weights);
  group.colors = c(A="#3E7655",B="#FD8D00",C="#615857",D="#903194");
  
  S = weights[orderV]
  letList = letters[1:length(modelId)];
  letList[S < 0.001] = "C"
  letList[S > 0.001] = "B"
  letList[1] = "A"
  
  orderV = orderV[1:6];
  letList = letList[orderV]
  
  dataFrame = data.frame(weights=weights[orderV], modelId = as.factor(modelId[orderV]),
                         categories=as.factor(letList))
  dataFrame$modelId = factor(dataFrame$modelId, 
                             levels = dataFrame$modelId[order(dataFrame$weights)])
  
  p2 = ggplot(dataFrame, aes(modelId,weights, fill=categories), stat="identity") + 
       geom_col(alpha=0.6) + coord_flip(ylim=c(0, round(max(1.1*weights),1)));
  p2 = p2 + theme_minimal()+ ggtitle( whatToRun);
  
  p2 = p2 + theme(text = element_text(size=16),
                  axis.text.y = element_text(hjust=1, angle=0,
                  color=group.colors[dataFrame$categories[order(dataFrame$weights)]]));
  
  p2 = p2 + scale_fill_manual(values=group.colors) + guides(fill=FALSE);
  p2 = p2 + theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank());
  p2 = p2 + theme(aspect.ratio = 1)
  p2 = p2 + labs(x="Posterior Model Probability", y="Models") + scale_y_continuous(expand = c(0, 0))
  
  p2 = p2 + theme(axis.line.x = element_line(colour = "gray"), 
                axis.ticks.x = element_line("gray", size=.25), axis.ticks.length = unit(.25,"cm"));
  
  
  grid.arrange(p2,nrow=1,ncol=1)
  
  save(file=paste(whatToRun,"_figure"), out=p2);
  
  
}else
{
  par(mfrow=c(1,2))
  
  par(mar=c(20,8,5,2))
  currentModels = colnames(landICw);
  x = colMeans(landICw);
  orderV = order(-x);
  myColor = rgb(red=69/255, green=139/255, blue=116/255,alpha=0.8)
  barplot(x[orderV],ylim = c(0,1), names.arg=currentModels[orderV], las=2, col=myColor, border=NA, ylab="Posterior Model Probabilities")  # if names are too long this won't display correctly
}

