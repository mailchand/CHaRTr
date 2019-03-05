source("diffusion-EAM-UGM.r")
subjnam = "Olaf.Rdata"


load('colgrid/Olaf.Rdata')

N = sum(dat$n)
fnam='a'

# modelList =   c("stone",                      # -1
#                 "stoneEta",                   # -2
#                 "stoneEtaVarTer",             # -3
#                 "ratcliff",                   # -4
#                 "ratcliffVarTer",             # -5
#                 "stoneEtaDitterich",          # -6
#                 "stoneUGM",                   #  1
#                 "stoneUGMVarTer",             #  2 
#                 "stoneEtaUGM",                #  3
#                 "stoneEtaUGMVarTer",          #  4  
#                 "stoneEtaUGMslope",           #  5       
#                 "stoneEtaUGMslopeVarTer",     #  6       
#                 "stoneEtaUGMVarslope",        #  7       
#                 "stoneEtaUGMVarslopeVarTer",  #  8
#                 "stoneEtaUGMallVar",           #  9
#                 "stoneEtaUrgency"
#                 ) 
allValidModels = returnListOfModels()
modelList = unname(allValidModels$modelNames)

modelList = c("stoneEta","stoneEtaVarTer","stoneEtaUGMintercept","stoneEtaUGMallVar",
              "stoneEtaUrgency","stoneEtaDitterich")

objectives = seq(1,length(modelList))
nPars = objectives
BIC = objectives
for(b in seq(1,length(modelList)))
{
  model = modelList[b]  
  saveFileName=paste("~/code/EAM-UGM-code/Hawkins-EAM-UGM/FinalModelFits/",subjnam,"-",model,"-",fnam,sep="")  
  load(path.expand(saveFileName))
  print(paste("model:",model,out$reobj))
  objectives[b] = 2*out$reobj; 
  nPars[b] = length(out$pars)
  BIC[b] = log(N)*length(out$pars) - 2*out$reobj
}
  
