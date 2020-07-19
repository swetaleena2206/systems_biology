##Dynamic models of reaction networks

# dA/dt = k1 - (k2*A) - (k3*A*B) 
# dB/dt = (k2*A) - (k3*A*B)
# dC/dt = (k3*A*B) - k4*C
# dD/dt = (k3*A*B) - k5*D

library(deSolve)
library(rootSolve)



model = function(t, init, para)
{
  A <- init[1]
  B <- init[2]
  C <- init[3]
  D <- init[4]
  k1 <- para[1]
  k2 <- para[2]
  k3 <- para[3]
  k4 <- para[4]
  k5 <- para[5]
 dA = k1 - (k2*A) - (k3*A*B) 
 dB = (k2*A) - (k3*A*B)
 dC = (k3*A*B) - k4*C
 dD = (k3*A*B) - k5*D
 
  list(c(dA,dB,dC,dD)) 
}

# Give initial values - A(t=0), B(t=0), C(t=0), D(t=0)
init <- c(0, 0, 0, 0)

# Give parameter value -  k1, k2,k3, k4, k5
para <- c(3,2,2.5,3,4)

# Time range for solution 
t <- seq(0, 4, 0.01) 

# Calling ODE function
out <- ode(y = init, times = t, func = model, parms = para)

# Plotting solution 
plot(out[,1], out[,2], type = "l", xlab = "t", col = "blue", ylim = c(0,1), ylab = "A(t)")
par(new=TRUE)
plot(out[,1], out[,3], type = "l", xlab = "t", col = "red",  ylim = c(0,1), ylab = "B(t)")
par(new=TRUE)
plot(out[,1], out[,4], type = "l", xlab = "t", col = "green", ylim = c(0,1), ylab = "C(t)")
par(new=TRUE)
plot(out[,1], out[,5], type = "l", xlab = "t", col = "purple", ylim = c(0,1), ylab = "D(t)")

legend( "topright", c("A(t)", "B(t)", "C(t)", "D(t)"), 
text.col=c("blue", "red", "green", "purple") )



