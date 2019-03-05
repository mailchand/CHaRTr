load("FinalModelFits/Olaf.Rdata-stoneEtaUrgency-a")
SOlafUGM =apply(out$reobjperpoint,MARGIN=c(1),sum)

load("FinalModelFits/Olaf.Rdata-stoneEtaVarTer-a")
SOlafNonUGM =apply(out$reobjperpoint,MARGIN=c(1),sum)
plot(x=NULL,y=NULL,xlim=c(0, 1),ylim=c(-7000, 7000))
x = seq(from=0.1,to=1, by=0.1)
points(x,SOlafUGM - SOlafNonUGM, col="red3")

X = c(0,0);
X[1] = sum(SOlafNonUGM[1:5] - SOlafUGM[1:5])
X[2] = sum(SOlafNonUGM[6:10] - SOlafUGM[6:10])
barplot(X, xlab="Quantiles", main="Fit profile", names.arg=c("RTs < 50th percentile", "RTs > 50th percentile"), col=c("magenta","cyan"),ylim=c(-10000,10000))


# load("LatestModelFits/Tiberius.Rdata-stoneEtaUGM-a")
# STibsUGM =apply(out$reobjperpoint,MARGIN=c(1),sum)
# 
# load("LatestModelFits/Tiberius.Rdata-stoneEta-a")
# STibsNonUGM =apply(out$reobjperpoint,MARGIN=c(1),sum)
# points(x,STibsUGM - STibsNonUGM, col="green3")
# 
# plot(x=NULL, y=NULL,ylim=c(-10000, 5000))
# X[1] = sum(STibsNonUGM[1:5] - STibsUGM[1:5])
# X[2] = sum(STibsNonUGM[6:10] - STibsUGM[6:10])
# barplot(X, xlab="Quantiles", main="Fit profile", names.arg=c("RTs < 50th percentile", "RTs > 50th percentile"), col=c("magenta","cyan"), ylim=c(-10000,10000))
