x = seq(0,4,0.05);
a = 2;
lambda = 3
k = 2
aprime = 0;
y1 = a*(1 - exp(-(x/lambda)^k))*(1-aprime)-a
y2 = -y1;

df = data.frame(xv = x, yv1=y1, yv2 = y2)

ggplot(df, aes(x)) + 
  geom_line(aes(y = yv1, colour = "yv1")) + 
  geom_line(aes(y = yv2, colour = "yv2"))

