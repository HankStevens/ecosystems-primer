library(deSolve)
library(tidyverse)
## This script loads a function called, oddly enough, "sensitivity"
source("sensitivity.R")
## ask for the arguments of the function
args("sensitivity")


rm( list = ls() ) # delete (remove) everything in the workspace.


## I put the script in a folder called 'code' that resides inside
## my working directory. The following line of code runs the code in 
## the script, and the script is located at code/bormann2.R

source("bormann2.R")
args(bormann2)

## if you want to make sure that it loaded the right thing, type
## bormann2 on your command line, or highlight the word, and run it with
## Control-Enter

## Earlier we loaded code to calculate the sensitivities. 
## Here we load code to create a time series figure of the deviations.
source("sens_fig.R") # graph time series of sensitivity of one variable
args( sens_fig )


params <- c( 
i1 = 6.5, # precip
i2 = 14.2, # fixation
a1 = 79.6 / (26 * 532), # uptake
a2 = (6.6 + 0.8) / 532, # throughfall and inorganic exudates
a3 = (54.2 + 2.7 + 0.1 + 6.2 ) / 532, # litter, throughfall, organic exudates
a4 = 69.6 / 4700, # net mineralization
a5 = 3.9 /26, # export from available
a6 = 0.1/4700, #export from bound
K=600
) # close parentheses


initial.state <- c(V = 532, A = 26, B = 4700)
t <- seq(from = 0, to = 500, by = 1)



### Without burn-in
out <- sens_fig("A", y.initial=initial.state, 
                times=0:1000, func=bormann2, parms=params,
                burnin=0, tiny=0.01, relative=FALSE )

ggplot(out, aes(Time, y, colour=Perturbation)) + geom_line() + facet_wrap(~Parameter, scales="free_y")

### with burn-in
out <- sens_fig("A", y.initial=initial.state, 
                times=0:1000, func=bormann2, parms=params,
                burnin=500, tiny=0.01, relative=TRUE )

ggplot(out, aes(Time, y, colour=Perturbation)) + geom_line() + facet_wrap(~Parameter, scales="free_y")


b1 <- sensitivity(y.initial=initial.state, times=t, 
           func=bormann2, parms=params, 
           dev.type='normalized', 
           summary.type="arithmetic_mean")


## Peak at the structure of the output
str(b1)


b1


b1L <- b1$deviation.summary %>%  
  pivot_longer(cols="V":"total")

ggplot(data=b1L, aes(x=parameters, y=value)) + geom_col() + 
  facet_wrap(~name, scales="free_y")


rmse.SV <- b1$deviation.summary %>% 
   select(V:B) %>% 
   apply(MARGIN=1, function(x) sqrt( x*x / 3 ))

SV <- b1$deviation.summary %>% 
   select(V:B)

rmse.SV <- t( apply(SV, MARGIN=1, function(x) {
      m <- mean(x)
      sq <- (x-m)^2
      sqrt( mean( sq ))
   }) )

rmse <- data.frame(p=b1$deviation.summary$parameters, rmse=as.numeric(rmse.SV))

qplot(p, rmse, data=rmse, geom="col")

ggplot(data=rmse, aes(x=p, y=rmse)) + geom_col()


## load the model
source("NPZD.R")

#-----------------------#
# define the model parameters: #
#-----------------------#

parameters<-c(maxUptake          =1.0,       # /day
              ksPAR              =140,       # muEinst/m2/s
              ksDIN              =0.5,       # mmolN/m3
              maxGrazing         =1.0,       # /day
              ksGrazing          =1.0,       # mmolN/m3
              pFaeces            =0.3,       # -
              excretionRate      =0.1,       # /day
              mortalityRate      =0.4,       # /(mmolN/m3)/day 
              mineralisationRate =0.1,       # /day
              chlNratio          =1)         # mgChl/mmolN

#-------------------------#
# the initial conditions: #
#-------------------------#
 
state     <-c(PHYTO   =1,   # state variable initial conditions, units mmolN/m3
              ZOO     =0.1,
              DETRITUS=5.0,
              DIN     =5.0)



times <- 0:730
out <- ode(state, times, NPZD, parameters)
out %>% 
  as.data.frame() %>%
  pivot_longer(cols=-time, names_to="State.var", values_to="value") %>%
ggplot(aes(time, value)) + geom_line() + facet_wrap(~State.var, scale="free_y")


outf <- sens_fig("DIN", y.initial=state, 
                times=0:1000, func=NPZD, parms=parameters,
                burnin=0, tiny=0.01, relative=TRUE )

ggplot(outf, aes(Time, y, colour=Perturbation)) + 
   geom_line() + facet_wrap(~Parameter, scales="free_y")


# args(sensitivity)
outd <- sensitivity(state, times, NPZD, parameters,
                    dev.type = "normalized", 
                    summary.type = "arithmetic_mean")

outd$deviation.summary %>%  
  pivot_longer(cols=-parameters) %>%
ggplot(aes(x=parameters, y=value)) + geom_col() + 
  facet_wrap(~name, ncol=2, scales="free") + 
   coord_flip() # Flip x and y so we can see the parameter names


ksGr <- 1
## let Phytoplankton be x, and vary between 0 and 6 mmolN/m3
## because that is the approximate range we see in the dynamics
curve(x/(ksGr + x), 0, 6)


ksGr <- c(.5, 1, 2)
# let Phytoplankton vary more
curve(x/(ksGr[1] + x), 0, 10)
curve(x/(ksGr[2] + x), 0, 10, add=TRUE, lty=2)
curve(x/(ksGr[3] + x), 0, 10,  add=TRUE, lty=3)
legend("bottomright", legend=paste(ksGr), lty=1:3)


## reference run
## make a copy of the parameters
myP <- parameters
## integrate and make a data frame
out_1 <- as.data.frame( ode(state, times, NPZD, myP) )

## add identifier
out_1 <- out_1 %>%
   ## make a new column
   mutate(ksGrazing = 1.0) # code as text

## Change the half saturation constant
myP["ksGrazing"] <- 0.5
##rerun and make a data frame
out_0.5 <- as.data.frame( ode(state, times, NPZD, myP) )
## add identifier
out_0.5 <- out_0.5 %>%
   ## make a new column
   mutate(ksGrazing = 0.5) # code as text

## Change the half saturation constant
myP["ksGrazing"] <- 2
## rerun and make a dataframe
out_2<- as.data.frame( ode(state, times, NPZD, myP) )
## add identifier
out_2 <- out_2 %>%
   ## make a new column
   mutate(ksGrazing = 2.0) # code as text

out.all <- rbind(out_0.5, out_1, out_2)


out.all %>% 
  pivot_longer(cols=PHYTO:TotalN, names_to="State.var", values_to="value") %>%
ggplot(aes(time, value, colour=as.factor(ksGrazing))) + geom_line() + facet_wrap(~State.var, scale="free_y")








