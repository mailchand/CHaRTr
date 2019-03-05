# 2. Calculate AIC, BIC, AIC weights, BIC weights
# Useful for model averaging and model selection.
ms=modelSelection(models=modelOutput, data=dat)
# simple plot to show differnces between models
par(mar=c(20,8,5,2))
x = ms$BIC - ms$BIC[1];
orderV = order(-x);
currentModels = rownames(ms);
myColor = rgb(red=69/255, green=139/255, blue=116/255,alpha=0.8)
barplot(x[orderV], 
        names.arg=currentModels[orderV], las=1, col=myColor, 
        border=NA, xlab="AIC values",horiz=TRUE,
        xlim = 50*round(1.5*c(min(x), max(x))/50))  # if names are too long this won't display correctly
mtext(side=3,text=subjnam)

# A second example. Let's suppose we want to compare a subset of the models, not all of them; 
# e.g., Cisek urgency vs Ditterich urgency vs. Ratcliff model, First, create a character vector of the models you 
# want to compare. The names must exactly match the names as they appear in the modelOutput list.
# otherwise you will get odd errors

modelsToTest=currentModels[orderV[seq(length(orderV)-5, length(orderV))]]
# modelsToTest=c("stoneEtaVarTer", "ratcliffVarTer","stoneEtaCollapse","stoneEtaDitterich")
# call the modelSelection function while subsetting from the modelOutput list
ms = modelSelection(models=modelOutput[modelsToTest], data=dat)
par(mar=c(20,8,5,2))
myColor = rgb(red=0/255, green=191/255, blue=255/255,alpha=0.8)
barplot(ms$AICw,names.arg=rownames(ms), horiz=TRUE, las=1, col=myColor, 
        border=NA, xlab="AIC weights")  # if names are too long this won't display correctly


par(mfrow=c(1,2))
S = modelOutput$DDMSvSt$pars;
T = rbind(currParams, S);
barplot(T,beside=TRUE, 
        names.arg = colnames(currParams), 
        col=c("gray83","dodgerblue1"),border=NA, las=2, ylim=range(pretty(c(0, T))))


