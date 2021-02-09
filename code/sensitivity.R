#######
## This function uses code from Soetart and Herman (2009) to 
## measure sensitivities in systems of ODEs. 
## variable is the state variable whose sensitivity we are measuring.
## func is the system of ODEs written for use with ode().
## times is the vector of times for which the user wants solutions of the state variables.
## y.initial is a named vector the starting point for the ODEs.
## burnin is the number of initial time steps to throw away. NULL causes the function to throw away the first half. Otherwise set an actual number (integer).
## parms is the vector of parameter values used in func.
## tiny is the proportional deviation to the parameter; can be positive or negative.
## (0.1 = 10% increase, -0.1 = 10% decrease).
## dev.type refers to the type of sensitivity, either the 
### sensitivity, 'sensitivity' (dy / dp * p),
### elasticity, 'elasticity' (d(log[y])/d(log(p)))
### 'normalized' ( [dy/y] / [dp/p]) (default), or
### simple 'deviate' (dy), 
### approximate 'derivative' (dy/dp), 
## summary.type refers to how the time series of deviates are summarized.
### They include
### arithmetic_mean = colMeans( x ) (default),
### mean_absolute = colMeans( abs(x) )

sensitivity <- function(y.initial, times, func, parms, burnin=NULL, tiny = 1e-3, dev.type="normalized", summary.type="arithmetic_mean") {
  ## number of state variables
  n.state.vars <- length(y.initial)
  ## Create a baseline
  out <- as.data.frame(
    ode(y = y.initial, times=times, fun=func, parms = parms) 
  )
  
  ## throw away the first 'burnin' rows of the baseline run
  if( is.null(burnin) ) { 
    n <- round( length(times)/2 ) 
    } else { n <- burnin }
  reference <- out[-(1:n),] # all the data
  ref.state <- reference[,-1] # remove time

  ## collect the dimensions
  n.all.vars <- ncol(ref.state) # includes derived variables (e.g. total)
  n.ref.rows <- nrow(ref.state)
  
  # redefine time steps
  new.times <- 1:n.ref.rows
  
  ## creates a reference set of parameters
  pp <- unlist(parms) 
  p <- pp
  
  ## number of parameters
  n.par    <- length(parms)
  
  ## make a vector that contains the small changes to each parameter
  dp <- pp * tiny
  
  ## Create a holder for our results
  ## A list with one component for each state variable.
  sens.list <-vector(mode="list", length=n.all.vars)
  
  ## Into this list, we will put an empty matrix 
  ## with rows = n.ref.rows, and a column for each parameter that will 
  ## hold deviates 
  ## one row for each time step in the reference
  ## one column for each parameter
  for(i in 1:n.all.vars) {
    a <- matrix(data=NA, nrow=n.ref.rows, ncol=n.par)
    colnames(a) <- names(pp)
    sens.list[[i]] <- a
  }
  names(sens.list) <- colnames(ref.state)
  
  ## FOR each parameter i, do the following ...
  for (i in 1:n.par)
  {
    dval <- pp[i] + dp[i] # create a new value of p[i]
    p[i] <- dval #...and stick it in the new parameter set
    
    init.state.vars <- as.numeric( 
      ref.state[1,1:n.state.vars] # the starting point of the baseline
      ) 
    names(init.state.vars ) <- names(y.initial)
    ## rerun, with new p[i]
    ## state variables, y, that are perturbed
    pert.state  <- as.data.frame(
      ode(init.state.vars, # the new initial state
          new.times, # the new times
          fun = func, parms=p) 
      )[,-1] 
    
    ## Quantify the change in y per unit change in p[i], 
    ## expressed in units of the p[i]
    for(j in 1:n.all.vars) {
      devs <- pert.state[,j]-ref.state[,j]
      log.devs <- log(pert.state[,j]) - log(ref.state[,j])
      log.dp.i <- log(p[i]) - log(pp[i])
    sens.list[[j]][,i] <- switch(dev.type,
                                deviate = devs,
                        sensitivity = devs/dp[i], 
                        elasticity = log.devs/log.dp.i,
                        normalized = (devs)/ref.state[,j]/tiny,
                       derivative = (devs)/dp[i],
                       )
    }
    
    ## reset p to original
    p[i] <- pp[i]
  }
  
  #########
  ## summarize time series of deviates 
  #########
  
  ## average absolute perturbation
sum.devs <- sapply(sens.list, function(x) { 
    switch(summary.type,
           arithmetic_mean = colMeans( x ),
           mean_absolute = colMeans( abs(x) ),
           RMSE = sqrt( colSums( x * x ) / n.ref.rows )
    )
  })
sum.devs <- data.frame(parameters=names(p), sum.devs, row.names=NULL)
  ## return output as list
out <- list(deviation.summary = sum.devs, # summaries
     dev.type=dev.type, # type of deviate
     summary.type=summary.type, # type of summary
     burnin=NULL,
     tiny=tiny, # size of proportional perturbation to parameters
     parms=parms, # original parameter set
     sens.list = sens.list # original time series
) 
class(out) <- c("sensitivity", class(out))
  return(out)

}

print.sensitivity <- function(X){
  print(X$deviation.summary)
}

