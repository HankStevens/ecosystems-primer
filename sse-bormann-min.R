sse.bormann.mineralization <- function(params, data) {
  ## an objective function whose arguments are 
  ## params - the parameter of interest
  ## data - data from Bormannm et al.
  
  ## Assign a new value of the parameter to our set of parameters
  p['a23'] <- params
  
  ## run the model 
  out <- ode(y = y, times = t, func = bormann.logistic, parms = p)
  
  ## store the last values of the state variables
  nr <- nrow(out)
  model.output <- out[nr,c("V", "A", "B")]
  
  ## Calculate the sum of the squared differences
  ## squaring the differences makes them positives and 
  ## weights big differences even more heavily
  diffs <- model.output - data
  diffs2 <- diffs^2 
  sse <- sum( diffs2 )
  
  ## Return the SSE
  sse
}