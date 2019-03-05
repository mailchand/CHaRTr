library(ggplot2)
library(gridExtra)
library(ggthemes)

useAIC = FALSE;


# 2. Calculate AIC, BIC, AIC weights, BIC weights
# Useful for model averaging and model selection.
ms=modelSelection(models=modelOutput, data=dat)
# simple plot to show differnces between models
par(mar=c(20,8,5,2))
if(useAIC){ 
  x = ms$AIC-ms$AIC[1]
}else
   x = ms$BIC-ms$BIC[1]

  orderV = order(-x);
currentModels = rownames(ms);
myColor = rgb(red=69/255, green=139/255, blue=116/255,alpha=0.8)
modelId=rownames(ms);
letList = letters[1:length(modelId)];
letList[1:length(letList)] = "C"
letList[length(letList)] = "A"
letList[length(modelId)-1 - (4:0)] = "B"
I = x[x > 0];
letList[1:length(I)] = "D"
group.colors = c(A="#3E7655",B="#FD8D00",C="#615857",D="#903194");

AIC = x[orderV];
AIC[AIC > 300] = 300;
dataFrame = data.frame(AIC=AIC, modelId = as.factor(currentModels[orderV]), 
                       categories=as.factor(letList));
dataFrame$modelId = factor(dataFrame$modelId, 
                           levels = dataFrame$modelId[order(-dataFrame$AIC)])

# Plot nice ggplot
p1=ggplot(dataFrame, aes(modelId,AIC, fill=categories), stat="identity");
p1=p1 + geom_col(alpha=0.6) + geom_hline(yintercept=0, linetype="dashed")
p1=p1 + coord_flip() + theme_minimal()
p1=p1 + ggtitle(paste("All Models, ",subjnam));
p1=p1 + scale_fill_manual(values=group.colors)+ guides(fill=FALSE);
p1=p1 + theme(text = element_text(size=14));
p1=p1 + theme(aspect.ratio = 1);
p1=p1 + theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank());
p1=p1 + theme(axis.text.y = element_text(color=group.colors[dataFrame$categories]));
p1=p1 + theme(axis.line.x = element_line(colour="gray"), 
              axis.ticks.x = element_line("gray", size=.25), axis.ticks.length = unit(.25,"cm"));
p1=p1 + labs(y="AIC", x="Models")




# A second example. Let's suppose we want to compare a subset of the models, not all of them; 
# e.g., Cisek urgency vs Ditterich urgency vs. Ratcliff model, First, create a character vector of the models you 
# want to compare. The names must exactly match the names as they appear in the modelOutput list.
# otherwise you will get odd errors

modelsToTest=currentModels[orderV[seq(length(orderV)-5, length(orderV))]]
# call the modelSelection function while subsetting from the modelOutput list
ms = modelSelection(models=modelOutput[modelsToTest], data=dat)
if(useAIC){
  x = ms$AICw;
}else
  x = ms$BICw;
  
orderV = order(x);

modelId=rownames(ms);
letList = letters[1:length(modelId)];
letList[1:length(letList)] = "B"
letList[length(letList)] = "A"
group.colors = c(A="#3E7655",B="#FD8D00",C="#615857",D="#903194");
dataFrame = data.frame(weights=x[orderV], modelId = as.factor(modelId[orderV]),
                       categories=as.factor(letList))
dataFrame$modelId = factor(dataFrame$modelId, 
                           levels = dataFrame$modelId[order(dataFrame$weights)])
p2 = ggplot(dataFrame, aes(modelId,weights, fill=categories), stat="identity") + 
  geom_col(alpha=0.6) + coord_flip(ylim=c(0, 1));
p2 = p2 + theme_minimal()+ ggtitle("Model weights");
p2 = p2 + theme(text = element_text(size=16),axis.text.y = element_text(hjust=1, angle=0, 
                                                                        color=group.colors[dataFrame$categories[order(dataFrame$weights)]]));
p2 = p2 + scale_fill_manual(values=group.colors) + guides(fill=FALSE);
p2 = p2 + theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank());
p2 = p2 + theme(aspect.ratio = 1)
p2 = p2 + labs(x="Posterior Model Probability", y="Models") + scale_y_continuous(expand = c(0, 0))

p2 = p2 + theme(axis.line.x = element_line(colour = "gray"), 
                axis.ticks.x = element_line("gray", size=.25), axis.ticks.length = unit(.25,"cm"));

# Show the two plots side by side
grid.arrange(p1, p2, nrow=2, ncol=2)