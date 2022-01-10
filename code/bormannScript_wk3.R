### Make sure 'env_data_HB.csv' is in your working dir.
###
rm( list = ls() )
### read in, and reshape data
env <- read.csv("data/env_data_HB.csv")
env <- subset(env, year >=1979 & year <= 2012)
a <- reshape(env, idvar="year", timevar="factor",  direction="wide")
env_spr <- a
names(env_spr)[-1] <- substr(names(a)[-1], 7,100)
rm(list="a")

### create function for approximating N deposition
ndep <- subset(env_spr, !is.na(kgN.ha))
ndep$time <- ndep$year - min(ndep$year)
# create a function to approximate temperature for an arbitrary time point.
ndep.approx <- approxfun(x = ndep[,"time"], y = ndep[,"kgN.ha"], method = "linear", rule = 2)

### create function for approximating temperature
temps <- subset(env_spr, !is.na(meandegC))
temps$time <- temps$year - min(temps$year)
# create a function to approximate temperature for an arbitrary time point.
temp.approx <- approxfun(x = temps[,"time"], y = temps[,"meandegC"], method = "linear", rule = 2)


### create function for approximating precip
precip <- subset(env_spr, !is.na(ppt))
precip$time <- precip$year - min(precip$year)
# create a function to approximate temperature for an arbitrary time point.
ppt.approx <- approxfun(x = temps[,"time"], y = temps[,"ppt"], method = "linear", rule = 2)


### ODEs
bormannODE <- function(t, y, p) {  
  with( as.list(c(y, p)), {
    ## sens.i1=1, sens.a4 = 1 allow us to calculate sensitivities
    
    ## N deposition
    new.i1 <- ndep.approx(t) * sens.ndep
    
    ## temperature
    T <- temp.approx(t)
    
    TempFactor <- exp( (T-Tref)/10 * log(Q10) )
    new.a4 <- TempFactor * a4 * sens.temps
    
    dV.dt <- ( a1 * A  - a2 - a3 ) * V * (1-V/K)
    dA.dt <- new.i1 + a2 * V + new.a4 * B - a1 * V * A - a5 * A
    dB.dt <- i2 + a3 * V - new.a4 * B - a6 * B 
    
    return(list( c(dV.dt, dA.dt, dB.dt), # first element 
                 totalN = V + A + B,
                 export = a6*B + a5*A,
                 N.dep=new.i1,
                 temp=T) )
  })
  
}


### What if denitrification competes with mineralization?
## Let denitrification be about 16.7 kg N/ha/y,
## which would create a steady state N budget.
## the rate would be 
a.d = 16.7/4700 
## whilte N min = a4 =  69.6 / 4700
## so these fluxes out of B = (a.d + a4)
## above ave ppt increases denit  but decreases n min
## below ave ppt decreases denit but i
bormannODE.D <- function(t, y, p) {  
  with( as.list(c(y, p)), {
    ## sens.i1=1, sens.a4 = 1 allow us to calculate sensitivities
    
    ## N deposition
    new.i1 <- ndep.approx(t) * sens.ndep
    
    ## temperature
    T <- temp.approx(t)
    
    TempFactor <- exp( (T-Tref)/10 * log(Q10) )
    new.a4 <- TempFactor * a4 * sens.temps
    precip <- ppt.approx(t)
    
    dV.dt <- ( a1 * A  - a2 - a3 ) * V * (1-V/K)
    dA.dt <- new.i1 + a2 * V + new.a4 * B - a1 * V * A - a5 * A
    dB.dt <- i2 + a3 * V - new.a4 * B - a6 * B 
    
    return(list( c(dV.dt, dA.dt, dB.dt), # first element 
                 totalN = V + A + B,
                 export = a6*B + a5*A,
                 N.dep=new.i1,
                 temp=T) )
  })
  
}

#########################
### Parameters

params <- c( a1 =  79.6 / (26 * 532), a2 = (6.6 + 0.8) / 532,
             a3 =  (54.2 + 2.7 + 0.1 + 6.2 ) / 532,
             a4 =  69.6 / 4700, 
              a5 = 3.9 /26, a6 = 0.1/4700, i1 = 6.5, i2 = 14.2,
	      K=600, # vegetation carrying capacity
            Tref = 4, # reference temperature
            Q10 = 2, 
	      sens.ndep=1, # a constant
	      sens.temps=1 # a constant
             )

### sens.i1 is a constant = 1 that allows us to measure the sensitivity
### of a state variable to N deposition, which we incorporate in i1

### sens.a4 is a constant = 1 that allows us to measure the sensitivity
### of a state variable to observed temperature, which we incoporate
### as TempFactor * a4.

### initial conditions
initial.state.vars <- c( V = 532, A = 26, B = 4700)


objs <- ls()
cat("This script generates these objects:")
cat("\n")

df <- data.frame(object.class = unlist( sapply(objs, function(x) class( eval(as.name(x))  ) ) )
                 )

print(df)
