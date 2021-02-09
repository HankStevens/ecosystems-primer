###############################################
## An NPZD model (Nutrient, Phytoplankton,   ##
## Zooplankton, Detritus)                    ##
##                                           ##
## Soetaert and Herman (2008)                ##
## A practical guide to ecological modelling ##
## Chapter 2. Model formulation              ##
## 2.9.1                                     ##
###############################################

# load package with the integration routine:

library(deSolve)

#----------------------#
# the model equations: #
#----------------------#

NPZD<-function(t,state,parameters)
 {
 with( as.list(c(state,parameters)), {  # unpack the state variables, parameters
    # Light, a sine function; 50% of light is PAR
    PAR <- 0.5*(540+440*sin(2*pi*t/365-1.4))
    # curve(540 + 440*sin(2*pi*x/365-1.4), 0, 365) 
    din            <- max(0,DIN) # to avoid errors when DIN becomes slightly negative..
    Nuptake <- maxUptake * PAR/(PAR+ksPAR) * din/(din+ksDIN)*PHYTO

    Grazing        <- maxGrazing * PHYTO/(PHYTO + ksGrazing)*ZOO 
    Faeces         <- pFaeces * Grazing
    Excretion      <- excretionRate * ZOO
    Mortality      <- mortalityRate * ZOO * ZOO
    Mineralisation <- mineralisationRate * DETRITUS
    Chlorophyll    <- chlNratio * PHYTO
    TotalN         <- PHYTO + ZOO + DETRITUS + DIN   
  
    dPHYTO    <- Nuptake - Grazing
    dZOO      <- Grazing - Faeces - Excretion - Mortality
    dDETRITUS <- Mortality - Mineralisation + Faeces
    dDIN      <- Mineralisation + Excretion - Nuptake
    
    # the output, packed as a list
    list( 
      c(dPHYTO,dZOO,dDETRITUS,dDIN),  # the rate of change
      c(Chlorophyll = Chlorophyll, PAR=PAR, TotalN= TotalN)
      )   # the ordinary output variables
    })
  }  # end of model

