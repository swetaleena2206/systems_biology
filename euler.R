# Euler equation for rigid bodies
# y1, y2, y3 are coordinates of the rotation vector
# I1, I2, I3 are moments of inertia along 3 axes
# 3 simultaneous equations:

# dy1/dt = ((I2 - I3) / I1) * y2 * y3
# dy2/dt = ((I3 - I1) / I2) * y3 * y1
# dy3/dt = ((I1 - I2) / I3) * y1 * y2

# Parameters: I1, I2, I3
# Initial conditions: y1, y2, y3

library(deSolve)
library(scatterplot3d)

rigidmode <- function(t, y, para){
  I1 = para[1]
  I2 = para[2]
  I3 = para[3]
  
  dy1 = ((I2 - I3) / I1) * y[2] * y[3]
  dy2 = ((I3 - I1) / I2) * y[3] * y[1]
  dy3 = ((I1 - I2) / I3) * y[1] * y[2]
  
  list(c(dy1, dy2, dy3))
  # function automatically returns dy as a list
}

# Define initial values
y = c(y1 = 1, y2 = 0, y3 = 0.9)

# Define parameters
para = c(I1 = 0.5, I2 = 2.0, I3 = 3.0)

# Define time interval
t = seq(0, 20, 0.01)

# Calling ODE function
out <- ode(y = y, times = t, func = rigidmode, parms = para)

# Plotting the solution
X11(scatterplot3d(out[,-1])) # minus sign means remove - here remove 1st column
plot(out) # will cause function overloading