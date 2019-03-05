# demo script for generating data from one of the models 
# that will be fit with the other model
source("simulators.r")
nCoh = 7; # Create data with 7 coherence
nmc = 500; # Take 500 trials for simulations

model = "stoneEtaUrgency"
fP = paramsandlims(model,nCoh, fakePars = TRUE)
dat = simulateRTs(model, currParams , n=nmc, nds=nCoh)

