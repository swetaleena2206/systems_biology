# anaesthesia in th muscle,blood,liver
# muscle M, blood B and liver L
# parameter e is the efflux from the muscle, c is the clearance from the blood, and δ is the degradation in the liver
# 3 simultaneous equations:

# dM/dt = -e*M
# dB/dt = e*M - c*B
# dL/dt = c*B - delta*L

# Parameters: e,c,delta



library(deSolve)
library(scatterplot3d)

anaesode <- function(t, init, para){

      M= init["M"]
      B = init["B"]
      L= init["L"]
 
  e = para["e"]
  c = para["c"]
  delta = para["delta"]
  
  dM = -e*M
  dB = e*M - c*B
  dL = c*B - delta*L
  
  list(c(dM, dB, dL))
  # function automatically returns dy as a list
}

# Define initial values
init = c(M = 315, B = 0, L = 0)

# Define parameters
para = c(e = 0.5, c = 0.3, delta = 0.4)

# Define time interval
t = seq(0, 100, 0.1)

# Calling ODE function
out <- ode(y = init, times = t, func = anaesode, parms = para)

#To check the contents of the out matrix
head(out)


###Question1###
#To get graph for Muscle vs Time
y=out[,1]
x=out[,2]
plot(x,y,xlab="M(t) in mg",ylab="Time(hours)")

#To get graph for Blood vs Time
y=out[,1]
x=out[,3]
plot(x,y,xlab="B(t) in mg",ylab="Time(hours)")


###Question2###
plot (out)
y=out[,1]
x=out[,3]
plot(x,y,xlab="B(t) in mg",ylab="Time(hours)")
abline(v=1.4)

