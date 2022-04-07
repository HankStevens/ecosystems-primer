## ---------------------------------------------------------------------------
# rm( list = ls() ) # clean the workspace, with rm()
## load the libraries we need
## install these if you have not already done so.
library(tidyverse) # data management and graphing
library(deSolve) # ODE solver
library(bbmle) # maximum likelihood estimation (Ben Bolker, MLE)


## ---------------------------------------------------------------------------
# The next line of code loads and runs a file. It requires that this script
#   is in R's working directory.
# Alternatively, you could include the entire path in the file name.
# getwd() # will tell you what the working directory is.

source("code/BormannLogistic.R")
# bormann.logistic


## ----  fig.width = 7, fig.height = 4, fig.cap = "Our model's output."-------
p <- c( I1 = 6.5, # deposition
        I2 = 14.2, # N fixation
        a1 = 79.6/(532*26), # uptake by vegetation (V)
        a2 = (6.6 + 0.8)/532, # root exudation into available pool
        a3 = (54.2 + 2.7 + 6.2 + 0.1)/532, # litter fall, etc.
        a4 = 69.6/4700, # net mineralization
        a5 = 3.9/26, # export from available pool to stream
        a6 = 0.1/4700, # export from bound pool to stream
        K = 600 # carrying capacity of vegetation
        )
## Initial values
y0 <- c(V = 532, A = 26, B = 4700)

## times that we want R to return
t <- seq(from = 0, to = 10, by = .1)

## Integrate
out <- ode(y = y0, times = t, func = bormann.logistic, parms = p)

## rearrange to long format and plot
out2 <- out %>% as.data.frame() %>% 
  pivot_longer(cols=-time)

ggplot(out2, aes(time, value)) + geom_line() +
  facet_wrap(~ name, scales="free", nrow=1)


## ---------------------------------------------------------------------------
m <- 10; SD <- 1
## calculating the probability density for all values 
## between 7 and 13 
curve(dnorm(x, m=m, sd=SD), 7, 13)


## ---------------------------------------------------------------------------
dnorm(9, m=m, sd=SD)


## ----obj.f1-----------------------------------------------------------------
nll.bormann.mineralization <- function(a4, y0, t, p, our.data) {
  ## an objective function whose arguments includes 
  ## a4 - the parameter of interest
  
  ## Assign a new value of the parameter to our set of parameters
  p['a4'] <- a4

  ## run the model 
  ## When you use this objective function later, 
  ## you will need to make sure that 
  ## -- y0 is what you intend
  ## -- t has the values that you need, where the end point is
  ## the endpoint you want,
  ## -- the parameters, p, are the original, or intended, set.
  out <- ode(y = y0, times = t, func = bormann.logistic, parms = p)

  ## store the last values of the state variables
  ## determine the index for last row of output
  nr <- nrow(out)
  ## extract the last row of the state variables you want
  model.output <- out[nr,c("V", "A", "B")]
  
  ## Calculate the sum of the negative log-likelihoods
  ## first the deviates
  diffs <- model.output - our.data
  ## next the standard dev of the deviates.
  SD <- sqrt( sum(diffs^2)/length(diffs) )
  ## neg log likelihood
  nll <- -sum( dnorm(our.data, m=model.output, sd=SD, log=TRUE))
  ## return the value
  nll
}



## ---------------------------------------------------------------------------
observed.data = c(V = 532, A=26, B=4700)


## ---------------------------------------------------------------------------
## specify times we want.
t <- c(0, 500)
fit0 <- optim(par=c(a4=0.015),
              fn = nll.bormann.mineralization,
              our.data=observed.data, y0=y0, t=t, p=p,
              method="BFGS"
              )


## ---------------------------------------------------------------------------
fit0


## ---------------------------------------------------------------------------
p.opt <- p
p.opt['a4'] <- fit0$par[1]


## ---------------------------------------------------------------------------
out.original <- as.data.frame( ode(y = y0, times = 0:500, func = bormann.logistic, parms = p))
out.opt <- as.data.frame( ode(y = y0, times = 0:500, func = bormann.logistic, parms = p.opt))


## ----fig.show="hold"--------------------------------------------------------
out.original <- out.original %>%
  mutate(run=rep("original", nrow(out.original)))
out.opt <- out.opt %>%
  mutate(run=rep("optimized", nrow(out.opt)) )
out.b <- rbind(out.original, out.opt)
out.b.long <- out.b %>%
  pivot_longer(cols=V:loss, names_to="Variable", values_to="value")

out.b.long <- out.b.long %>% 
  mutate(
    our.data=case_when(Variable=="V" ~ observed.data['V'],
                       Variable=="A" ~ observed.data['A'],
                       Variable=="B" ~ observed.data['B'])
  )

ggplot(out.b.long, aes(x=time, y=value, colour=run)) +
  geom_line() + geom_line(aes(x=time, y=our.data), linetype=2) +
  facet_wrap(~Variable, scales="free") 



## ---------------------------------------------------------------------------
fit.mle <- mle2(minuslogl = nll.bormann.mineralization,
     start=list(a4=0.015),
                data=list(our.data=observed.data, 
                          y0=y0, t=t, p=p),
              method="BFGS" )
summary(fit.mle)


## ---------------------------------------------------------------------------
llh.profile <- profile(fit.mle)
plot(llh.profile)
confint(llh.profile)


## ----HBdata, fig.cap="Hubbard Brook Watershed 6 N export.", out.width="80%"----
## we skip the first 7 lines because those contain metadata
HB.df <- read.csv("data/env_data_HB.csv", skip=7)
ggplot(data=HB.df, aes(x=year, y=value)) + geom_line() +
  facet_wrap(~factor, scales='free')


## ---------------------------------------------------------------------------
Ne.df <- filter(HB.df, factor=="ann.exp.TN.kg.y")


## ---------------------------------------------------------------------------
## load our sensitivity function
source("code/sensitivity.R") # sensitivities of all variables
b1 <- sensitivity(y.initial=y0, times=0:100, 
           func=bormann.logistic, parms=p, 
           dev.type='normalized', 
           summary.type="arithmetic_mean")

b1$deviation.summary


## ---------------------------------------------------------------------------
nll.bormann.a5.traj <- function(y0, t, p, a5, export) {
  ## an objective function whose arguments are 
  
  ## Assign a new value of the parameter to our set of parameters
  p['a5'] <- a5

  ## run the model 
  ## starting with 'initial.values' and t for time
  ## t must be the same time points for which we have data.
  out <- ode(y = y0, times = t, func = bormann.logistic, parms = p)
  model.output <- out[,"loss"]

  ## Calculate the negative log-likelihood
  SD <- sqrt( sum( (export - model.output)^2 )/length(export) )
  nll <- - sum( dnorm(export, mean=model.output, sd=SD))
  nll
}


## ----traj_match1------------------------------------------------------------
year.subset <- 1975:2014
#
data.subset <- Ne.df %>% filter(year %in% year.subset) 
 ##View(data.subset)

t <- data.subset$year-1975
y0 <- c(V = 532, A = 26, B = 4700)
p2 <- p
fit.a5 <- optim(f = nll.bormann.a5.traj, 
                 par=c(a5 = 0.15),
                 y0=y0, t=t, p=p2, 
                 export=data.subset$value,
                 method="Brent", 
                 lower = 0.01, upper=0.5,
                 hessian=TRUE)

# list(est=fit.a5$par, 
#      SD=sqrt( solve(fit.a5$hessian) ) )

fit.a5.mle2 <- mle2(nll.bormann.a5.traj, start=list(a5=0.05),
                     data=list(export=data.subset$value, 
                               y0=y0, t=t, p=p2),
                     method="BFGS")
summary(fit.a5.mle2)
bp <- profile(fit.a5.mle2)
plot(bp)
confint(bp)


## ---------------------------------------------------------------------------
fit.a5

## ---------------------------------------------------------------------------
## original value
p['a5']
p.a5 <- p
p.a5['a5'] <- fit.a5$par[1]


## ---------------------------------------------------------------------------
out.orig <- as.data.frame( 
  ode(y = y0, times = 0:39, func = bormann.logistic, parms = p)
  )
out.a5 <- as.data.frame(
  ode(y = y0, times = 0:39, func = bormann.logistic, parms = p.a5)
  )


## ---------------------------------------------------------------------------
plot(out.orig$time, out.orig$loss, 
     type='l', lty=3, ylim=c(0,10))
lines(out.orig$time, out.a5$loss, lty=2)
lines(out.orig$time, data.subset$value, lty=1)
legend('topright', 
       legend=c('original', 'fitted', 'data'),
       lty=c(3,2,1))


## ---------------------------------------------------------------------------
## or the deviations over time (fitted - data)
devs <- out.a5$loss - data.subset$value
qplot(x=out.a5$time, y=devs) + geom_point() + geom_smooth() 


## ----fig.show="hold"--------------------------------------------------------
## add a new column
## this will repeat the label for all rows of the data frame
out.orig$version <- "Original" 
out.a5$version <- "Fitted" 
names(out.orig)
## combine ('bind') all rows of both data frames...
out2 <- rbind(out.orig, out.a5) %>% 
  ## ... and then stack up all of the state variables,
  ## from V to loss
  pivot_longer(cols=V:loss, names_to="variable", values_to="value")

## plot what we want
ggplot(data=out2, aes(x=time, y=value, colour=version)) + 
  ## line graphs for time series
  geom_line() +
  ## separate each state variable in its own subfigure with its own scale
  facet_wrap(~variable, scales = "free")


## ----a54--------------------------------------------------------------------
nll.bormann.a5.4.traj <- function(a5, a4, data, y0, t, p) {
  ## an objective function whose arguments are 
  ## params - the parameter of interest
  ## data - data from Bormannm et al.
  
  ## Assign a new value of the parameter to our set of parameters
p['a5'] <- a5
p['a4'] <- a4
## run the model 
## starting with 'initial.values' and t for time
out <- ode(y = y0, times = t, func = bormann.logistic, parms = p)

model.output <- out[,"loss"]

## Negative log-likelihood
## assuming normally distributed observation error
SD <- sqrt(sum( (data - model.output)^2)/length(data) )
nll <- - sum( dnorm(data, mean=model.output, sd=SD, log=TRUE))

}



## ----fit2parms--------------------------------------------------------------
fit2.mle <- mle2(minuslogl = nll.bormann.a5.4.traj,
                 start=list(a5=0.05, a4=0.015),
                 data=list(data=data.subset$value,
                           y0=y0, t=t, p=p),
                 method="L-BFGS-B", lower=0)
summary(fit2.mle)


## ---------------------------------------------------------------------------
## original value
p2 <- p
p2['a5'] <- coef(fit2.mle)[1]
p2['a4'] <- coef(fit2.mle)[2]


## ---------------------------------------------------------------------------
out.orig <- as.data.frame( 
  ode(y = y0, times = 0:39, func = bormann.logistic, parms = p)
  )
out.2 <- as.data.frame(
  ode(y = y0, times = 0:39, func = bormann.logistic, parms = p2)
  )


## ---------------------------------------------------------------------------
plot(out.orig$time, out.orig$loss, 
     type='l', lty=3, ylim=c(0,10))
lines(out.orig$time, out.2$loss, lty=2)
lines(out.orig$time, data.subset$value, lty=1)
legend('topright', 
       legend=c('original', 'fitted', 'data'),
       lty=c(3,2,1))


## ---------------------------------------------------------------------------
sse.bormann.mineralization3 <- function(a4, data, vars) {
  ## an objective function whose arguments are 
  ## params - the parameter of interest
  ## data - point estimates of the observed state variables from Bormannm et al.
  ## vars - observed variances of the data.
  
  ## Assign a new value of the parameter to our set of parameters
p['a4'] <- a4

## run the model 
out <- ode(y = y0, times = t, func = bormann.logistic, parms = p)

## store the last values of the state variables
nr <- nrow(out)
model.output <- out[nr,c("V", "A", "B")]

## Calculate the sum of the squared differences
## squaring the differences makes them positives and 
## weights big differences even more heavily
diffs <- model.output - data
diffs2 <- diffs^2 
wdiffs2 <- diffs2/vars
 sse <- sum( wdiffs2 )
 
## Return the SSE
 sse
}


## ---------------------------------------------------------------------------
vars <- c(532/10, 26/10, 4700)


## ---------------------------------------------------------------------------
fitv <- optim(par=c(a4=0.014),
              fn = sse.bormann.mineralization3, 
              data=observed.data, vars=vars,
              method="BFGS")


## ---------------------------------------------------------------------------
fit0$par; fitv$par


## ---------------------------------------------------------------------------
p.v <- p
p.v["a4"] <- fitv$par
out.original <- as.data.frame( 
  ode(y = y0, times = 0:500, func = bormann.logistic, 
      parms = p))
out.v <- as.data.frame( 
  ode(y = y0, times = 0:500, func = bormann.logistic, 
      parms = p.v))

out.original <- out.original %>%
  mutate(run=rep("original", nrow(out.original)))
out.v <- out.opt %>%
  mutate(run=rep("var.wt", nrow(out.opt)) )
out.b <- rbind(out.original, out.v)
out.b.long <- out.b %>%
  pivot_longer(cols=V:loss, names_to="Variable", values_to="value")

ggplot(out.b.long, aes(x=time, y=value, colour=run)) +
  geom_line() + 
  facet_wrap(~Variable, scales="free") 

