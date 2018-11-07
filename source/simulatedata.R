


###########################
## Load/Install Packages ##
###########################

pkgs = c("MASS")
for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p, dependencies = TRUE)}
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))
rm(p, pkgs)





################################
## Load the True Data Objects ##
################################

load("data/truedata.rda")





#############################
## Generate Simulated Data ##
#############################

## Create a stacked dataframe for the simulated data
dat = data.frame(Y_True = c(rep(mean_1, N), rep(mean_2, N), rep(mean_3, N)),
                 Time = rep(t, N*g),
                 Group = as.factor( c(rep(1, N*length(t)), rep(2, N*length(t)), rep(3, N*length(t))) ),
                 ID = factor( rep(1:(N*g), length(t))[ order(rep(1:(N*g), length(t))) ] ) )
rm(mean_1, mean_2, mean_3)

## Define values
sigma_1 = 0.15    ## Simulations used 0.05 and 0.15
sigma_2 = 0.05    ## 0.05 for all simulations

## Calculate the covariance matrix
grid = expand.grid(t, t)
cov.matrix = apply(X = grid, MARGIN = 1,
                   FUN = function(x){ ((sigma_1^2)*0.15^(abs(x[1] - x[2]) )) + (sigma_2^2)*as.numeric(x[1] == x[2]) } )
cov.matrix = matrix(data = cov.matrix, ncol = length(t), nrow = length(t))
rm(grid)

## Generate errors
seed = 87654321
set.seed(seed)
errors = mvrnorm(n = N*g, mu = rep(0, length(t)), Sigma = cov.matrix)

## Add to the dataframe
dat$Errors = c( errors[1:(N*g),] )

## Calculate simulated Y's
dat$Y_Sim = (dat$Y_True + dat$Errors)
rm(cov.matrix, errors, sigma_1, sigma_2)

## Pre-smooth data
dat$Y_Smooth = rep(NA, N*g*length(t))
for(i in 1:(N*g)){
  start = (i-1)*length(t) + 1
  end = i*length(t)
  dat$Y_Smooth[start:end] = with(dat, smooth.spline(Time[start:end], Y_Sim[start:end], nknots = 20))$y
}
rm(i, start, end, t, mu.t)

## Calculate higher-order time terms
dat$Time_2 = dat$Time^2
dat$Time_3 = dat$Time^3
dat$Time_4 = dat$Time^4
dat$Time_5 = dat$Time^5





###########################
## Save This Environment ##
###########################

save(list = ls(), file = "data/simulateddata.rda")



