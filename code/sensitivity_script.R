###
## Investigate sensitivity

source("sens.R")

out <- sens(variable="V", y = initial.state, times=time, func=bormann1, parms = params)
str(out)
out.long <- out$sens.sum %>%  pivot_longer(cols = mean:rmse)
ggplot(out.long, aes(parameters, value)) + geom_point() + facet_wrap(~name, scales="free_y")
