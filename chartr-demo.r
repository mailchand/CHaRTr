# Demonstration of some of the simulation capabilities of this toolbox.
graphics.off();

source("simulators.r")
source("diffusion-EAM-UGM.r")
load("RS2002/b")
plotData(dat, ExistingPlot=FALSE)

# Now plot simulated RTs from  a model
X = returnListOfModels()
modelList = unname(X$modelNames)

figLayout = rbind(0,c(0,1,0,2,0,3,0,4,0),0,c(0,5,0,6,0,7,0,8,0),0, c(0,9,0,10,0,11,0,12,0),0)
# layout(figLayout,h=c(.4,1,.1,1,.32),w=c(.5,1,.1,1,.05))
layout(figLayout, h=c(.1,1,0.1,1,.1,1,0.1), w=c(0.15,1,0.15,1,0.15,1,0.15,1,0.15))

# Simulate 2 coherences, 10000 repeats
nCoh = 5
nmc = 500;


# Vanilla DDM model, no variability in any parameter
model = "stone"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
R = simulateRTs(model, currParams , n=nmc, nds=nCoh)

# Vanilla DDM model, variable drift rate
model = "stoneEta"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
R = simulateRTs(model, currParams , n=nmc, nds=nCoh)

# Urgency gating model, variable drift rate
model = "stoneEtaUGM"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
currParams["aU"] = 3000;
R = simulateRTs(model, currParams , n=nmc, nds=nCoh)


# Vanilla UGM model, variable drift rate, variable residual movement time
model = "stoneEtaUGMVarTer"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
currParams["aU"] = 3000;
R = simulateRTs(model, currParams , n=nmc, nds=nCoh)

# UGM model with an intercept and variable residual movement time
model = "stoneEtaUGMinterceptVarTer"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
currParams["aU"] = 15000;
currParams["intercept"] = 1768;
currParams["Ter"] = 0.3;
R = simulateRTs(model, currParams , n=nmc, nds=nCoh)


# Ratcliff model - variability in drift rate and baseline
model = "ratcliff"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
currParams["zmin"] = 0.9*currParams["aU"]/2;
currParams["zmax"] = 1.1*currParams["aU"]/2;
R = simulateRTs(model, currParams , n=nmc, nds=nCoh)


# Ratcliff model - variability in drift rate and baseline
model = "ratcliffVarTer"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
currParams["zmin"] = 0.9*currParams["aU"]/2;
currParams["zmax"] = 1.1*currParams["aU"]/2;
R = simulateRTs(model, currParams , n=nmc, nds=nCoh)

#
model = "stoneEtaUrgency"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
currParams["aU"] = 20000;
currParams["intercept"] = 3;
currParams["ieta"] = 8;
currParams["usign_var"] = 0.1537;
R = simulateRTs(model, currParams , n=nmc, nds=nCoh)

#
model = "stoneEtaUrgencyVarTer"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
currParams["aU"] = 20000;
currParams["intercept"] = 3;
currParams["ieta"] = 8;
currParams["usign_var"] = 0.1537;
R = simulateRTs(model, currParams , n=nmc, nds=nCoh)


model = "stoneEtaDitterich"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
currParams["aU"] = .47;
currParams["st0"] = 0.14;
currParams["sx"] = 8;
currParams["sy"] = 8;
R = simulateRTs(model, currParams , n=nmc, nds=nCoh)


model = "stoneEtaUGMallVar"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
currParams["timecons_var"] = 300;
R = simulateRTs(model, currParams , n=nmc, nds=nCoh)


mtext('Probability Correct', side=1, outer=TRUE, line = -4, cex=1.5)
mtext('Reaction time (s)', side=2, outer=TRUE, line = -4, cex=1.5)

