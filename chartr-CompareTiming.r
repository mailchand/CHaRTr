


library(reshape2)
load("cs2, slow, 400, 10000")
d1 = out$timingData;
d1[,"type"] = "Slow, 400, 10000"
sumData1 = out$allTiming;
sumData1[,"modelId"] = as.factor(rownames(sumData1));
iX= order(sumData1["meanV"])
totalTime = data.frame(c(mean(as.matrix(out$allDurations)), 
                         sd(as.matrix(out$allDurations))/sqrt(length(as.matrix(out$allDurations)))));
colnames(totalTime) = "Slow, 400, 10000"; 

S = data.frame("Slow, 400, 10000" = out$allTiming$meanV)

load("cs2, fast, 400, 10000")
d2 = out$timingData;
d2[,"type"] = "Fast, 400, 10000"
sumData2 = out$allTiming;
sumData2[,"modelId"] = seq(1,nrow(sumData2));

S = cbind(S, "Fast_400_10000" = out$allTiming$meanV)
totalTime[,"Fast, 400, 10000"] = c(mean(as.matrix(out$allDurations)), 
                                   sd(as.matrix(out$allDurations))/sqrt(length(as.matrix(out$allDurations))));

load("cs2, fast, 200, 10000");
d3 = out$timingData;
d3[,"type"] = "Fast, 200, 10000"
sumData3 = out$allTiming;
sumData3[,"modelId"] = as.factor(rownames(sumData3));

S = cbind(S, "Fast_200_10000" = out$allTiming$meanV)

totalTime[,"Fast, 200, 10000"] = c(mean(as.matrix(out$allDurations)), 
                                 sd(as.matrix(out$allDurations))/sqrt(length(as.matrix(out$allDurations))));


load("cs2, Fast, 200, 5000");
d4 = out$timingData;
d4[,"type"] = "Fast, 200, 5000"
sumData4 = out$allTiming;
sumData4[,"modelId"] = as.factor(rownames(sumData4));

S = cbind(S, "Fast_200_5000" = out$allTiming$meanV)
totalTime[,"Fast, 200, 5000"] = c(mean(as.matrix(out$allDurations)), 
                                sd(as.matrix(out$allDurations))/sqrt(length(as.matrix(out$allDurations))));

rownames(S) = rownames(out$allTiming)

d = rbind(d1, d2, d3, d4);
d$value = d$value/3600;
d$value[d$value > 8] = 8;

p3 = ggplot(d, aes(npars, value, color=type), stat="identity");
p3 = p3 + geom_jitter(alpha=0.4, position = position_jitter(w=0.3,h=0.0, seed=1), size=3);
p3 = p3 + ylim(0,8);

p3 = p3 + theme_minimal();
p3 = p3 +  theme(aspect.ratio = 1) + theme(panel.grid = element_blank()) + theme(text = element_text(size=16));
p3 = p3 + theme(axis.line.x = element_line(color="black", size = .5),
                axis.line.y = element_line(color="black", size = .5), axis.ticks = element_line(size=0.5),axis.ticks.length = unit(.25, "cm") )
p3 = p3 +  labs(y = "Run time (Hrs)", x='Number of parameters');
cor.test(formula=~value+npars, data = d)


sumData = cbind(sumData4[iX,c("meanV","modelId")], sumData3[iX, "meanV"], sumData2[iX,"meanV"], sumData1[iX,"meanV"]);
colnames(sumData) = c("Fast, 200, 5000", "modelId", "Fast, 200, 10000", "Fast, 400, 10000", "Slow, 400, 10000");
sumData = melt(sumData,id="modelId")

p1=ggplot(sumData, aes(x=reorder(modelId, value), y=value/3600, fill=variable, order=variable));
p1=p1 + geom_bar(alpha=0.8, stat = "identity", position="dodge") + theme_minimal() + coord_flip();
p1=p1 +  theme(aspect.ratio = 1) + theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank());
p1 = p1 +  labs(y = "Run time (Hrs)", x='Models') + theme(text = element_text(size=16));

rownames(totalTime) = c("mean", "se");
totalTime = data.frame(t(totalTime)/3600);

p2 = ggplot(totalTime, aes(x=reorder(rownames(totalTime), mean), y=mean, fill=factor(rownames(totalTime)))) + geom_bar(stat="identity");
p2 = p2 + theme_minimal() +  theme(aspect.ratio = 1) + theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank());
p2 = p2 +  labs(y = "Run time (Hrs)", x='Variants') + theme(text = element_text(size=16)) + coord_flip();
p2 = p2 + geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=0.2, position=position_dodge(.9))

grid.arrange(p3, p1, p2, nrow=2, ncol=2) 


