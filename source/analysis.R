


###########################
## Load/Install Packages ##
###########################

pkgs = c("nlme","rjags")
for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p, dependencies = TRUE)}
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))
rm(p, pkgs)





#############################
## Load the Simualted Data ##
#############################

## Load the simulated data
load(file = "data/simulateddata.rda")

## Remove un-needed objects
rm(b1.t, b2.t, b3.t, g, N, seed)








