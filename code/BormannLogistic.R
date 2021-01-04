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
    dV.dt <- (a12 * A * V  - a21 * V - a31 * V) * (1 - V/K )
    ## 
    
    ## A still loses N to vegetation, so we leave the loss function the same.
    ## No self-inhibition
    ## Rate opf change  = Gains - Losses 
    dA.dt <- ( a20 + a23 * B ) - ( a12 * V * A + a02 * A)
    
    ## No self-inhibition
    ## Rate of change  = Gains - Losses 
    dB.dt <- ( a30 + a31 * V ) - ( a23 * B + a03 * B )
    loss <- a02*A + a03*B
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