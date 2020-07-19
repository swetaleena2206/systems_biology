library(deSolve)
library(scatterplot3d)

x =  seq(1,100,1)
k = 20

f1 <- 1.3 * x * (1-(x/k)) - (x/(1+x))
f2 <- 0.3 * x * (1-(x/k)) - (x/(1+x))
f3 <- 0.1 * x * (1-(x/k)) - (x/(1+x))

plot(x,f1, type = "l", xlab = "x", ylab = "f(x)")
lines (x, f2, col = "red")
lines (x, f3, col = "blue")




