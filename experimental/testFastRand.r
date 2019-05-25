library(tictoc)

dyn.load("chartr-ModelSpecFast.so")
interval=0.00001
LUT=qnorm(seq(interval,1-interval,interval))
n=length(LUT);
nmc = 10000;
rts <- resps <- numeric(nmc)
z = 0.04;
v = 0.4;
aU = 0.08
aL = 0;
stoch.s = 0.1;
dt = 0.001;
maxTimeStep = 4000;
tic("fast rnd")
out=.C("DDM",z=z,v=v,aU=aU,aL=aL, s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep,
                rangeLow =as.integer(0), rangeHigh = as.integer(n-1), randomTable = as.double(LUT));
L1 = mean(out$rt)
S1 = sum(out$response==1)
toc()

tic("slowrnd")
out=.C("DDMs",z=z,v=v,aU=aU,aL=aL, s=stoch.s,dt=dt, response=resps,rt=rts,n=nmc,maxTimeStep=maxTimeStep);
L2 = mean(out$rt)
S2 = sum(out$response==1)
toc()


