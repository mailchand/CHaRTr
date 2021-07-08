x = seq(0,1,0.001)
lambda = 0.278
k = 1.59
y= 15 + 5.55*(1-exp(-(x/lambda)^k))
df = data.frame(x = x, y = y)

p1 = ggplot(data=df, aes(x=x, y=y)) + geom_line() + theme_minimal()
p1
