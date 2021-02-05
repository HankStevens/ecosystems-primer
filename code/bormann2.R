bormann2 <- function(t, y, p) {
  # time, vector of state variables and parameters must be in this order
  # we can use as.list for both the state variables and parameters
  # a1 = uptake
  # a2 = loss from veg to avail
  # a3 = loss from veg to bound
  # a4 = net mineralization
  # a5 = export from avail
  # a6 = export from bound
  with( as.list( c(y, p) ), {
    dV.dt <- (a1 * A  - a2  - a3) * V * (1-V/K)
    dA.dt <- i1 + a2 * V + a4 * B - a1 * A * V - a5 * A 
    dB.dt <- i2 + a3 * V - a4 * B - a6 * B 
    # Here we return a list whose first element is the vector of
    # rates of change for the state variables. The first element must be these rates,
    # in the same order as the state variables in y
    # The second element is the total N in the system
    return(list( c(dV.dt, dA.dt, dB.dt), 
                 total = V + A + B 
    )  )})
}