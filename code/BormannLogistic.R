#########################################
## the ODE model 
## Bormann with logistic vegetation
#########################################

bormann.logistic <- function(t, y, p) {
  
  with(as.list( c(y, p) ), {
    ### ODEs
    ## We hypothesize that the growth function is all that Bormann et al. included. 
    ## Therefore, our logistic inhibition term acts on the entire forest.
    ## Growth = Gains - Losses
    ## Rate of change = Growth x Self-inhibition
    ## I1 - deposition into the inorganic available pool
    ## I2 - N fixation
    ## a1 - plant uptake
    ## a2 - root exudate into the available pool
    ## a3 - litter fall
    ## a4 - mineralization
    ## a5 - is loss from Available to the stream
    ## a6 - is loss from the bound pool to the stream
    
   
    dV.dt <- (a1 * A  - a2 - a3) * V * (1 - V/K )
    ## 
    
    ## A still loses N to vegetation, so we leave the loss function the same.
    ## No self-inhibition
    ## Rate opf change  = Gains - Losses 
    dA.dt <- ( I1 + a4 * B ) - ( a1 * V * A + a5 * A)
    
    ## No self-inhibition
    ## Rate of change  = Gains - Losses 
    dB.dt <- ( I2 + a3 * V ) - ( a4 * B + a6 * B )
    loss <- a5*A + a6*B
    # returning a list whose first (and only) element 
    # is a vector of the ODE values
    return( 
      list(
        c( dV.dt, dA.dt, dB.dt ), loss=loss
           )
    )
    }
  )
}
