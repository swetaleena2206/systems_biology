## Equations For Hill equation

# dS1/dt = (k1 / (1 + ((S2/M2)^n1))) - (k3 * S1)
# dS2/dt = (k2 / (1 + ((S1/M1)^n2))) - (k4 * S2)

library(deSolve)
library(rootSolve)

# Define Hill equation function with t, initial values, parameter

hill = function(t, init, para){
  S1 <- init[1]
  S2 <- init[2]
  n1 <- para[1]
  n2<- para[2]
  k1 <- para[3]
  k2 <- para[4]
  M1 <- para[5]
  M2 <- para[6] 
  k3 <- para[7]
  k4 <- para[8]
  dS1 = (k1 / (1 + ((S2/M2)^n1))) - (k3 * S1) # dS1/dt
  dS2 = (k2 / (1 + ((S1/M1)^n2))) - (k4 * S2) # dS2/dt
  list(c(dS1, dS2)) # function automatically returns dS1, dS2 as a list
}

# Give initial values - S1(t=0), S2(t=0)
init <- c(3, 1)

# Give parameter value -  n1, n2, k1, k2, M1, M2, k3, k4
para <- c(2, 2, 20, 20, 1, 1, 5, 5)

# Time range for solution - till 6.7 - found out when calculated till 100
t <- seq(0, 6.7, 0.01) 

# Calling ODE function
out <- ode(y = init, times = t, func = hill, parms = para)

# Plotting solution for S1(t)
plot(out[,1], out[,2], type = "l", xlab = "t", ylab = "S1(t)")

# Plotting solution for S2(t)
plot(out[,1], out[,3], type = "l", xlab = "t", ylab = "S2(t)")

# Plotting S1(t) vs S2(t)
plot(out[,2], out[,3], type = "l", xlab = "S1(t)", ylab = "S2(t)")

#Print summary - top few results
head(out)

# Printing Eigen Values
lambdas <- eigen(jacobian.full(y = init, func = hill,
                         parms = para))$values
print(lambdas)
if(is.complex(lambdas) == FALSE && lambdas[1] < 0 && lambdas[2] < 0){
  print("Stable node - system is stable for given initial conditions.")
  } else if(is.complex(lambdas) == FALSE && lambdas[1] > 0 && lambdas[2] > 0){
  print("Unstable node - system is unstable for given initial conditions.")
  } else if(is.complex(lambdas) == FALSE && lambdas[1] > 0 && lambdas[2] < 0){
    print("Unstable node - saddle point - system is unstable for given initial conditions.")
  } else if(is.complex(lambdas) == FALSE && lambdas[1] < 0 && lambdas[2] > 0){
    print("Unstable node - saddle point - system is unstable for given initial conditions.")
  } else if(is.complex(lambdas) == TRUE && Re(lambdas[1]) > 0 && Re(lambdas[2]) > 0){
    print("Unstable focus - system is oscillatory and unstable.")
  } else if(is.complex(lambdas) == TRUE && Re(lambdas[1]) < 0 && Re(lambdas[2]) < 0){
    print("Stable focus - system is oscillatory and stable.")
  }




model = function(x){
  F1 <- (20 / (1 + ((x[2]/1)^2))) - (5 * x[1])
  F2 <- (20 / (1 + ((x[1]/1)^2))) - (5 * x[2]) 
 
  c(F1 = F1, F2 = F2)
}
ss = multiroot(f=model, start=c(3,1))
print(ss)
