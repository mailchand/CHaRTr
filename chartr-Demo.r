#---Demo of the code used for simulation of various models
# Dependencies on simulators.r and chartr-helperfunction.r
# Important parameters
# nCoh -- number of coherences
# nmc  -- number of simulated trials
# model 
FASTRAND=FALSE;
source("chartr-HelperFunctions.r")



# Organize 4 rows, provide proper margins
par(mfrow = c(2,2), mar=c(6, 6, 1, 1))
nCoh = 5;

tic ("all")
# Simulate 50000 trials to show clear trends
nmc = 10000;

# Vanilla DDM model, variable drift rate
model = "DDM"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
currParams["aU"] = 0.09
tic("DDM")
R = simulateRTs(model, currParams , n=nmc, nds=nCoh, FASTRAND=FASTRAND)
toc();

# DDM model with variable drift rate, baseline and non-decision time
model = "DDMSvSzSt"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
currParams["zmin"] = 0.9*currParams["aU"]/2;
currParams["zmax"] = 1.1*currParams["aU"]/2;
tic("DDMSvSzSt")
R = simulateRTs(model, currParams , n=nmc, nds=nCoh,FASTRAND=FASTRAND)
toc();


# Urgency gating model, variable drift rate
model = "UGMSv"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
currParams["aU"] = 3000;
tic("UGMSv")
R = simulateRTs(model, currParams , n=nmc, nds=nCoh,FASTRAND=FASTRAND)
toc();

# Ditterich model with variable drift rate.
model = "dDDMSv"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
currParams = fP$fakeParams
currParams["aU"] = 0.09
tic("dDDMSv")
R = simulateRTs(model, currParams , n=nmc, nds=nCoh,FASTRAND=FASTRAND)
toc();

mtext('Probability Correct', side=1, outer=TRUE, line = -5, cex=1.5)
mtext('Reaction time (s)', side=2, outer=TRUE, line = -4, cex=1.5)
toc();