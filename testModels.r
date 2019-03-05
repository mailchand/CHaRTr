load('colgrid/Olaf.Rdata')

N = sum(dat$n)

load("modelFits/Olaf.Rdata-stoneEtaUGM-a")
logL1 = out$obj
BIC1 = log(N)*length(out$pars) - 2*logL1

load("modelFits/Olaf.Rdata-stoneEta-a")
logL2 = out$obj

BIC2 = log(N)*length(out$pars) - 2*logL2
