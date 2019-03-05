# Compute a set of variables from the raw data.
Trials = read.csv(paste("RawData/Tiberius.csv"));
saveFileName="RawData/Tiberius.Rdata";
cohValues = unique(Trials$coherence);
cohValues = sort(cohValues,decreasing=FALSE)
# Now count number of trials for each condition
# Should be much easier than this but I am struggling :)

dNameRows = c("cor", "err");
dNameCols = cohValues;
quantileNames = c(seq(from=0.1,to=0.9,by=0.1),0.975)

quantileNames = c(seq(from=0.1,to=0.9,by=0.1))

                  
counts = matrix(data=NA, nrow=2,ncol=length(cohValues), dimnames=list(dNameRows, dNameCols));
accuracy = c(cohValues);
quantiles = array(data=NA, dim = c(length(quantileNames),2,length(cohValues)), dimnames=list(quantileNames, dNameRows, dNameCols))

pb = array(data=NA, dim = c(10,2,length(cohValues)), dimnames=list(1:10, dNameRows, dNameCols))


plot(y=NULL,x=NULL,ylim=c(0.3, 1.0),xlim=c(0,1),ylab="",xlab="")


whichQ = seq(1,10,2)

cnt = 1;
print("Writing down different coherence values")
for(cid in c(4,10,20,31,40,60,90)){
  # print(cid)
  iX = Trials$coherence==cid;
  # print(sum(Trials$correct[iX]==1))
  temp = sum(Trials$correct[iX]==1);
  counts[1,cnt] = temp;
  
  temp = sum(Trials$correct[iX]==0);
  counts[2,cnt] = temp;

    
  accuracy[cnt] = counts[1,cnt]/(counts[1,cnt] + counts[2,cnt]);
  
  ixCorrect = Trials$correct==1 & Trials$coherence==cid;
  ixWrong = Trials$correct==0 & Trials$coherence==cid;

  
  quantiles[1:length(quantileNames),1,cnt] = quantile(Trials$rt[ixCorrect],quantileNames);
  quantiles[1:length(quantileNames),2,cnt] = quantile(Trials$rt[ixWrong],quantileNames);
  
  pb[1:10,1,cnt] = counts[1,cnt]/10; # Divide by 10 because you have quantiles :)!
  pb[1:10,2,cnt] = counts[2,cnt]/10;
  
  
  
  points(rep(accuracy[cnt],1,length(whichQ)), quantiles[whichQ,1,cnt],col="green3", pch=4)
  points(1-rep(accuracy[cnt],1,length(whichQ)), quantiles[whichQ,2,cnt],col="red3",pch=4)
  cnt = cnt + 1;
  
  # Get percentiles for the correct and incorrect values for each value of coherence
  
  }
names(accuracy)<-cohValues

p = accuracy;
q = quantiles;
n = counts;
x = subset(Trials,select=c("coherence","correct","rt"));

dat = list(p=p,n=n,q=q,pb=pb,x=x);
save(file=saveFileName,dat);


# Now create the pb