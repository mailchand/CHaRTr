dataSummary <- function(data, varname, groupnames){
  require(plyr)
  summaryFunc <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  dataSum<-ddply(data, groupnames, .fun=summaryFunc,
                  varname)
  dataSum <- rename(dataSum, c("mean" = varname))
  return(data_sum)
}