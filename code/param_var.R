#######
# This function uses code from Soetart and Hermann (2009) to measure sensitivities
# in systems of ODEs. 
# variable is the state variable whose sensitivity we are measuring.
# func is the system of ODEs written for use with ode().
# times is the vector of times for which the user wants solutions of the state variables.
# y.initial is the starting point for the ODEs.
# burnin is the number of initial time steps to throw away. NULL means that it will throw away the first half. Otherwise set an actual number (integer).
# parms is the vector of parameter values used in func.
# tiny is the proportional deviation to the parameter (0.1 = 10% increase, -0.1 = 10% decrease).


param_var <- function(variable, y.initial, times, func, parms, alt.parms, burnin=NULL, tiny = 1e-2) {
  
  out <- as.data.frame(
    ode(y = y.initial, times=times, fun=func, parms = parms) 
  )
  
  # throw away the first 'burnin' rows
  if( is.null(burnin) ) { 
    n <- round( length(times)/2 ) 
    } else { n <- burnin }
  reference <- out[-(1:n),]
  
  # look at the dimensions
  dim(reference)
  
  # look at the top of the data set
  head(out)
  
  # creates a reference set of parameters
  pp <- unlist(parms) 
  p <- pp
  # create a reference set of the state varialbes
  # state   <- y # unneeded
  
  # reference of our variable of interest
  yRef    <- reference[,variable]
  
  # number of time steps kept
  nout    <- length(yRef)
  
  # number of parameters
  npar    <- length(parms)
  
  
  # make a variable that is the small changes to each parameter
  dp      <- pp * tiny
  
  Sens    <- matrix(nrow=nout, ncol=npar, NA)
  i <- 1
  # FOR each parameter i, do ...
  for (i in 1:npar)
  {
    dval    <- pp[i] + dp[i] # create a new value of p[i]
    p[i] <- dval 
    
    state   <- y.initial
    # rerun, with new p[i]
    yPert   <- as.data.frame(ode(state, times, 
                                 fun = func, parms=p))[,variable][-(1:n)] 
    
    # Quantify the change in U per unit change in p[i], 
    # expressed in units of the p[i]
    Sens[,i] <- (yPert-yRef)/dp[i] * pp[i]
    
    # reset p to original
    p[i] <- pp[i]
  }
  
  # format the output
  colnames(Sens) <- names(p)
  rownames(Sens) <- times[-(1:n)]
  
  colm <- colMeans( Sens) 
  mabs <- colMeans( abs(Sens) ) # or apply( Sens, 2, function(x) mean( abs( x ) ) )
  rmse <- sqrt( colSums( Sens * Sens ) / nout ) 
  
  return(sensitivities = data.frame(parameters=names(p), mean=colm, mean.abs=mabs, rmse=rmse))
}

