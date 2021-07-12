genUrgency = function(x, intercept)
{
  lambda = 0.126
  k = 2.27
  y = intercept + 10*(1-exp(-(x/lambda)^k))
  y
}

x = seq(0,1,0.001)
output = matrix(ncol=5, nrow=length(x))
for(i in seq(1,5))
{
  output[,i] = genUrgency(x, runif(1));
}

df = data.frame(output)
p1 = ggplot(data=df, aes(x=x)) + geom_line(aes(y=y1)) + theme_minimal()
p1 = p1 + geom_line(aes(y=y2))
show(p1)

