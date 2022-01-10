#######
# This function uses code from Soetart and Hermann (2009) to mesaure sensitivities
# in systems of ODEs. 
# func is the system of ODEs written for use with ode().
# times is the vector of times for which the user wants solutions of the state variables.
# y.initial is the starting point for the ODEs.
# burnin is the number of initial time steps to throw away. NULL means that it will throw away the first half. Otherwise set an actual number (integer).
# parms is the vector of parameter values used in func.
# tiny is the proportional deviation to the parameter (0.1 = 10% increase, -0.1 = 10% decrease).
# variable is the state variable whose sensitivity we are measuring.

sens_fig <- function(variable, y.initial, times, func, parms, burnin=NULL, 
                     tiny = 1e-3, relative=TRUE) {
library(tidyr)
  y <- y.initial
  t <- times
  outr <- as.data.frame(
    ode(y = y, times=times, fun=func, parms = parms) 
  )
  
  # throw away the first n rows
  if( is.null(burnin) ) { 
    n <- round( length(times)/2 ) 
    } else { n <- burnin }
  reference <- outr[-(1:n),]
  
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
 
  out.all = NULL
  
  # FOR each parameter i, do ...
  for (i in 1:npar)
  {
    print(i)
    dval    <- pp[i] + dp[i] # create a new value of p[i]
    p[i] <- dval 
   
      
      # run the model with a bigger p[i]
      outp <- as.data.frame(
        ode(y = y, times=t, fun=func, parms = p) 
      )[ -(1:n), ]
      
      # run the model with a smaller p[i]
      dval    <- pp[i] - dp[i] # create a new value of p[i]
      p[i] <- dval
      
      outm <- as.data.frame(
        ode(y = y, times=t, fun=func, parms = p) 
      )[ -(1:n), ]
      
    p.name <- rep(names(p)[i], nout)
    # reset p to original
    p[i] <- pp[i]
    
if(relative){
    rr <- rep(0, length(yRef))
    spr <- (outm[, variable]-yRef)/yRef/tiny
    lpr <- (outp[, variable]-yRef)/yRef/tiny
} else{
    rr <- yRef
    spr <- outm[, variable]
    lpr <- outp[, variable]
}
    
    out.loop <- data.frame(Parameter = p.name, 
                           Time = outm[,"time"],
                           Reference = rr, 
                           small.p = spr, 
                           large.p = lpr )
    
    out.all <- rbind(out.all, out.loop) 
    out.alls <- gather(out.all, key=param, value=y, -c(Parameter,Time) )
    out.alls$Time <- as.numeric( as.character( out.alls$Time ) )
    names(out.alls) <- c("Parameter", "Time", "Perturbation", "y") 
  }
  
  return(out.alls)
}
