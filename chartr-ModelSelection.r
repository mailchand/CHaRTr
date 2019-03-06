# Transform model output to the Akaike Information Criterion (AIC) and the 
# Bayesian Information Criterion (BIC), as well as their respective transformations 
# to Akaike weights and approximat posterior model probabilities.

# For details on Akaike Weights, see Wagenmakers, E.-J., & Farrell, S. (2004). 
# Model selection using Akaike weights. Psychonomic Bulletin & Review, 11, 192-196.
# For details on the BIC-based approximation to posterior model probabilities, 
# see Wasserman, L. (2000). Bayesian model selection and averaging. 
# Journal of Mathematical Psychology, 44, 92-107. 

source("chartr-helperfunctions.r")
# Function to calculate model weights
#
# 
pmp=function(IC) {
  # IC is the information criteria (AIC or BIC) for a set of models under comparison
  x=IC-min(IC)
  # PMP is the posterior model probability
  pmp=exp(-.5*x)/sum(exp(-.5*x)) 
  pmp       
}

calcIC = function(ll, npars, N)
{
  
  # Calculate the information criteria for the models
  # AIC just penalizes number of parameters
  AIC = 2*npars - 2*ll;
  
  # BIC also penalizes by number of parameters,but also includes 
  # a penalty of the number of trials used for computing the penalty
  BIC = log(N)*npars - 2*ll;
  
  data.frame(AIC=AIC, BIC=BIC, LL=-2*ll, PenA = 2*npars,PenB=log(N)*npars);
}

# Calculate AIC/BIC and their conversion to model weights 
#
#
modelSelection=function(models,data) {
  # models is a list where each element holds the output from a model fit
  # data is a single subjects data informations
  npars=sapply(models,function(x) length(x$pars))
  ll=sapply(models,function(x) x$reobj)
  N=sum(dat$n)
  AIC=2*npars - 2*ll
  logL = -2*ll
  BIC=log(N)*npars - 2*ll
  AICw=pmp(AIC)
  BICw=pmp(BIC)
  logLw=pmp(logL);
  data.frame(AIC,BIC,AICw,BICw, N, logL,logLw, npars)
}

# This formats columns of data frames with different columns having different numbers of digits if needed.
# 
#
# 
formatColumns = function(data, digits)
{
    "%,%" <- function(x,y)paste(x,y,sep="")
    nms <- names(data)
    nc <- ncol(data)
    nd <- length(digits)
    if(nc!=nd) 
      stop("Argument 'digits' must be vector of length " %,% 
             nc %,% ", the number of columns in 'data'.")
    out <- as.data.frame(sapply(1:nc, 
                                FUN=function(x, d, Y)
                                  format(Y[,x], digits=d[x]), Y=tbl, d=digits))
    if(!is.null(nms)) names(out) <- nms
    out
}
