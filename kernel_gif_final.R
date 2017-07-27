library(ggplot2)
library(pracma)

set.seed(1)
x <- 1:100
y <- x^2*sin(2*pi*x/100) + 500*rnorm(length(x))

df <- data.frame(x, y)
h <- 12
smoother <- data.frame(ksmooth(x, y, "normal", bandwidth = h, n.points = 100))

ggplot(df, aes(x,y)) + geom_point() + geom_line(data=smoother, aes(x,y))

scale <- abs((erfinv(-1/2)*(2^0.5)*4/h)^-1)
wt <- dnorm(x-50, 0, scale)

ggplot(df, aes(x,y, col=wt)) + 
  geom_point(size=pmax(100*wt, 1)) + 
  geom_line(data = smoother, aes(x, y), col = "black") + 
  geom_point(data = smoother[x==50,], aes(x, y), size=3,
             col = "black", shape = 21, fill = "white") +
  scale_color_gradient(low="dark blue", high="red")+
  theme(legend.position="none")

for (i in x){
  wt <- dnorm(x-i, 0, scale)
  plt <- ggplot(df, aes(x,y, col=wt)) + 
    geom_point(size=pmax(100*wt, 1)) + 
    geom_line(data = smoother[x<=i,], aes(x, y), col = "black") + 
    geom_point(data = smoother[x==i,], aes(x, y), size=3,
               col = "black", shape = 21, fill = "white") +
    scale_color_gradient(low="dark blue", high="red")+
    theme(legend.position="none")
  ggsave(plt,filename=paste("kernel_",i,".png",sep=""))
}