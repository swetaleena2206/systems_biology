## Logistic Growth Equation
# dN/dt = r.N(1-(N/k))

# Analytical solution
# N(t) = k.N(theta).exp(r.t) / (k + N(0).(exp(r.t) - 1))

library(deSolve)

# Define model function with time, y, parameters

model = function(t, y, para){
  N <- y
  r <- para[1]
  k <- para[2]
  dN <- r * N * (1 - (N/k)) # dN here refers to dN/dt
  list(dN) # function automatically returns dN as a list
}

# Give initial value
y <- 0.1

# Give parameter values - r, k
para <- c(0.1, 10)

# Time range for solution
t <- seq(0, 100, 1)

# Calling ODE function
out <- ode(y = y, times = t, func = model, parms = para)

# Plotting solution
plot(out, xlab = "Time", ylab = "N(t)")

#Print summary
print(out)