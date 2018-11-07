

############################
## Create the "True" Data ##
############################

## Model
## y_i(t) = mu(t) + b_gp(i)(t) + e_i(t) for t in [0,1]

## Values
t = 0:200/200     ## Evaluation time points
g = 3             ## Number of groups
N = 10            ## Number of curves per group, N = 10 or 60

## Coefficients from the paper
mu.t = 0.4*atan(10*t - 5) + 0.6
b1.t = -0.5*exp(-10*t) - 0.04*sin(8*t) - 0.3*t + 0.5
b2.t = -(t - 0.5)^2 - 0.15*sin(13*t)
b3.t = -b1.t - b2.t

## Mean functions by group
mean_1 = mu.t + b1.t
mean_2 = mu.t + b2.t
mean_3 = mu.t + b3.t

## Create the "true" dataset
true.data = data.frame(Y_True = c(mean_1, mean_2, mean_3),
                       Time = rep(t, 3),
                       Group = as.factor(c(rep(1, length(t)), rep(2, length(t)), rep(3, length(t)))) )





################################
## Save the True Data Objects ##
################################

save(list = ls(), file = "data/truedata.rda")




