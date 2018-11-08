


###########################
## Install/Load Packages ##
###########################

pkgs = c("ggplot2","RColorBrewer","nlme","lme4","rjags")
for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p, dependencies = TRUE)}
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))
rm(p, pkgs)





#############################
## Load the Simulated Data ##
#############################

load(file = "data/simulateddata.rda")





##################################
## Figure 1: The Simulated Data ##
##################################

figure1 = ggplot() +
  geom_line(data = true.data, aes(x = Time, y = Y_True, color = Group), size = 1.25) +
  geom_point(data = dat, aes(x = Time, y = Y_Sim, color = Group), alpha = 0.3, size = 0.9) +
  theme_bw() +
  ylab("Y") +
  #ggtitle("Simulated Data with True Functions") +
  scale_color_brewer(palette = "Set1")





#############################################
## Figure 2: The Smoothed Simulated Curves ##
#############################################

figure2 = ggplot(data = dat, aes(x = Time, y = Y_Smooth, group = ID, color = Group)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  xlab("Time") +
  ylab("Y") +
  ggtitle("Subject-Specific Curves")





##############################################
## Figure 3: The Group-Level Mean Functions ##
##############################################

## Find the mean heights at each time for each gender
group.means = aggregate(dat$Y_Smooth ~ dat$Time + dat$Group, FUN = mean)
names(group.means) = c("Time","Group","Y")

## Find the gender-specific SD
group.sds = aggregate(dat$Y_Smooth ~ dat$Time + dat$Group, FUN = sd)
names(group.sds) = c("Time","Group","SD")

## Combine means and sds
group.summ = merge(x = group.means, y = group.sds, by = c("Time","Group"), sort = FALSE)
rm(group.means, group.sds)

## Add 1 SD band
group.summ$Lower = group.summ$Y - 2*group.summ$SD
group.summ$Upper = group.summ$Y + 2*group.summ$SD

## Plot the gender-specific curves
figure3 = ggplot(data = group.summ, aes(x = Time, y = Y, fill = Group)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  xlab("Time") +
  ylab("Y") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.4) +
  scale_fill_brewer(palette = "Set1")
rm(group.summ)





########################
## Analyze Using lm() ##
########################

## 5th-Order Linear Model With Time-by-Group and $\text{Time}^2$-by-Group Interactions

## Fit the model
mod = lm(Y_Smooth ~ Group + Time + Time_2 + Time_3 + Time_4 + Time_5 + Group*Time + Group*Time_2, data = dat)

## Calculate the fitted values
dat$Y_Smooth_Fitted_lm = fitted(mod)

## Plot the individual curves
figure4 = ggplot(data = dat, aes(x = Time, y = Y_Smooth_Fitted_lm, group = ID, color = Group)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  xlab("Time") +
  ylab("Y")
rm(mod)




###############################################
## Analyze Using Penalized Splines and lme() ##
###############################################

## Penalized basis functions
K = 24
qtiles = seq(0, 1, length = K + 2)[-c(1, K + 2)] 
knots = quantile(dat$Time, qtiles)
random.basis = cbind(sapply(knots, function(k) I((dat$Time - k)^2)*(dat$Time - k > 0)))
rm(qtiles)

## Define the fixed basis
fixed.basis = cbind(1, as.numeric(dat$Group == "2"), as.numeric(dat$Group == "3"), dat$Time, dat$Time_2)

## Partition the random.basis matrix
random.basis.1 = as.numeric(dat$Group == "1")*random.basis
random.basis.2 = as.numeric(dat$Group == "2")*random.basis
random.basis.3 = as.numeric(dat$Group == "3")*random.basis

## Define Y
Y = dat$Y_Smooth

## Fit the model
group.1 = group.2 = group.3 = rep(1, nrow(dat))
mod = lme(Y ~ fixed.basis - 1, random = list(group.1 = pdIdent(~ random.basis.1 - 1),
                                             group.2 = pdIdent(~ random.basis.2 - 1),
                                             group.3 = pdIdent(~ random.basis.3 - 1)) )
rm(group.1, group.2, group.3)

## Extract the model coefficients
coefs = c( unlist(mod$coefficients$fixed), unlist(mod$coefficients$random) )

## Extract the lambda estimates
#var.e2 = mod$sigma^2
#var.u2 = getVarCov(mod)[1,1]

## Calculate the fitted values
dat$Y_Smooth_Fitted_lme = as.vector( cbind(fixed.basis, random.basis.1, random.basis.2, random.basis.3) %*% coefs )
rm(fixed.basis, mod, random.basis, coefs, knots, Y)

## Plot the penalized splines
figure5 = ggplot(data = dat, aes(x = Time, y = Y_Smooth_Fitted_lme, group = ID, color = Group)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  xlab("Time") +
  ylab("Y")





########################
## Analyze Using JAGS ##
########################

## 5th-Order Linear Model With Time-by-Group and $\text{Time}^2$-by-Group Interactions

## Define values
n = nrow(dat)
Y = dat$Y_Smooth
X = dat$Time
X_2 = dat$Time_2
X_3 = dat$Time_3
X_4 = dat$Time_4
X_5 = dat$Time_5
G.2 = as.numeric(dat$Group == "2")
G.3 = as.numeric(dat$Group == "3")

## Create the model
model_string <- "model{

# Likelihood
for(i in 1:n){
Y[i]   ~ dnorm(mu[i], inv.var)
mu[i] <- beta[1] + beta[2]*X[i] + beta[3]*X_2[i] + beta[4]*X_3[i] + beta[5]*X_4[i] + beta[6]*X_5[i] + beta[7]*G.2[i] + beta[8]*G.3[i] + beta[9]*X[i]*G.2[i] + beta[10]*X[i]*G.3[i] + beta[11]*X_2[i]*G.2[i] + beta[12]*X_2[i]*G.3[i]
}

# Prior for beta
for(j in 1:12){
beta[j] ~ dnorm(0,0.0001)
}

# Prior for the inverse variance
inv.var   ~ dgamma(0.01, 0.01)
sigma     <- 1/sqrt(inv.var)

}"

## Compile the model
model = jags.model(textConnection(model_string),
                   data = list(Y = Y, n = n, X = X, X_2 = X_2, X_3 = X_3, X_4 = X_4, X_5 = X_5, G.2 = G.2, G.3 = G.3),
                   n.chains = 3,
                   n.adapt  = 1000)

## Burn-in samples
update(model, 1000)

## Draw additional samples
samp = coda.samples(model,
                    variable.names = c("beta","sigma"),
                    thin = 5,
                    n.iter = 15000)

## Investigate the model
summary(samp)
#plot(samp)

## Save the coefficients from the model
coefs = data.frame( summary(samp)$statistics )

## Extract the beta estimates
betas = coefs$Mean[ row.names(coefs) %in% paste0( paste0("beta[",1:12), "]") ]

## Calculate the fitted values
dat$Y_Smooth_Fitted_JAGS_5 = c(t(betas) %*% t(cbind(1, X, X_2, X_3, X_4, X_5, G.2, G.3, X*G.2, X*G.3, X_2*G.2, X_2*G.3)) )
rm(model, samp, coefs, model_string, n, X_3, X_4, X_5, betas)

## Plot the functions
figure6 = ggplot(data = dat, aes(x = Time, y = Y_Smooth_Fitted_JAGS_5, group = ID, color = Group)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  xlab("Time") +
  ylab("Y")





##############################################
## Analyze Using Penalized Splines and JAGS ##
##############################################

## Define values
n = nrow(dat)

## Create the model
model_string <- "model{

# Likelihood
for(i in 1:n){
Y[i]   ~ dnorm(mu[i], inv.var)
mu[i] <- beta[1] + beta[2]*G.2[i] + beta[3]*G.3[i] + beta[4]*X[i] + beta[5]*X_2[i] + alpha[1]*random.basis.1[i,1] + alpha[2]*random.basis.1[i,2] + alpha[3]*random.basis.1[i,3] + alpha[4]*random.basis.1[i,4] + alpha[5]*random.basis.1[i,5] + alpha[6]*random.basis.1[i,6] + alpha[7]*random.basis.1[i,7] + alpha[8]*random.basis.1[i,8] + alpha[9]*random.basis.1[i,9] + alpha[10]*random.basis.1[i,10] + alpha[11]*random.basis.1[i,11] + alpha[12]*random.basis.1[i,12] + alpha[13]*random.basis.1[i,13] + alpha[14]*random.basis.1[i,14] + alpha[15]*random.basis.1[i,15] + alpha[16]*random.basis.1[i,16] + alpha[17]*random.basis.1[i,17] + alpha[18]*random.basis.1[i,18] + alpha[19]*random.basis.1[i,19] + alpha[20]*random.basis.1[i,20] + alpha[21]*random.basis.1[i,21] + alpha[22]*random.basis.1[i,22] + alpha[23]*random.basis.1[i,23] + alpha[24]*random.basis.1[i,24] + alpha[25]*random.basis.2[i,1] + alpha[26]*random.basis.2[i,2] + alpha[27]*random.basis.2[i,3] + alpha[28]*random.basis.2[i,4] + alpha[29]*random.basis.2[i,5] + alpha[30]*random.basis.2[i,6] + alpha[31]*random.basis.2[i,7] + alpha[32]*random.basis.2[i,8] + alpha[33]*random.basis.2[i,9] + alpha[34]*random.basis.2[i,10] + alpha[35]*random.basis.2[i,11] + alpha[36]*random.basis.2[i,12] + alpha[37]*random.basis.2[i,13] + alpha[38]*random.basis.2[i,14] + alpha[39]*random.basis.2[i,15] + alpha[40]*random.basis.2[i,16] + alpha[41]*random.basis.2[i,17] + alpha[42]*random.basis.2[i,18] + alpha[43]*random.basis.2[i,19] + alpha[44]*random.basis.2[i,20] + alpha[45]*random.basis.2[i,21] + alpha[46]*random.basis.2[i,22] + alpha[47]*random.basis.2[i,23] + alpha[48]*random.basis.2[i,24] + alpha[49]*random.basis.3[i,1] + alpha[50]*random.basis.3[i,2] + alpha[51]*random.basis.3[i,3] + alpha[52]*random.basis.3[i,4] + alpha[53]*random.basis.3[i,5] + alpha[54]*random.basis.3[i,6] + alpha[55]*random.basis.3[i,7] + alpha[56]*random.basis.3[i,8] + alpha[57]*random.basis.3[i,9] + alpha[58]*random.basis.3[i,10] + alpha[59]*random.basis.3[i,11] + alpha[60]*random.basis.3[i,12] + alpha[61]*random.basis.3[i,13] + alpha[62]*random.basis.3[i,14] + alpha[63]*random.basis.3[i,15] + alpha[64]*random.basis.3[i,16] + alpha[65]*random.basis.3[i,17] + alpha[66]*random.basis.3[i,18] + alpha[67]*random.basis.3[i,19] + alpha[68]*random.basis.3[i,20] + alpha[69]*random.basis.3[i,21] + alpha[70]*random.basis.3[i,22] + alpha[71]*random.basis.3[i,23] + alpha[72]*random.basis.3[i,24]
}

# Prior for beta
for(j in 1:5){
beta[j] ~ dnorm(0,0.0001)
}

# Prior for alpha
for(k in 1:(3*K)){
alpha[k] ~ dnorm(0,0.0001)
}

# Prior for the inverse variance
inv.var    ~ dgamma(0.01, 0.01)
sigma     <- 1/sqrt(inv.var)

}"

## Compile the model
model = jags.model(textConnection(model_string),
                   data = list(Y = Y, n = n, X = X, X_2 = X_2, G.2 = G.2, G.3 = G.3, K = K, random.basis.1 = random.basis.1, random.basis.2 = random.basis.2, random.basis.3 = random.basis.3),
                   n.chains = 3,
                   n.adapt  = 1000)

## Burn-in samples
update(model, 1000)

## Draw additional samples
samp = coda.samples(model,
                    variable.names = c("beta","alpha","sigma"),
                    thin = 5,
                    n.iter = 15000)

## Investigate the model
summary(samp)
#plot(samp)

## Save the coefficients from the model
coefs = data.frame( summary(samp)$statistics )

## Save the lambda estimates
#var.e4
#var.u4

## Extract the parameter estimates and order them c(fixed, random)
params = c( coefs$Mean[ row.names(coefs) %in% paste0( paste0("beta[",1:5),"]") ], coefs$Mean[ row.names(coefs) %in% paste0( paste0("alpha[",1:72),"]") ] )

## Calculate the fitted values
dat$Y_Smooth_Fitted_JAGS_Spline = c(t(params) %*% t(cbind(1, G.2, G.3, X, X_2, random.basis.1, random.basis.2, random.basis.3)) )
rm(coefs, model, samp, model_string, random.basis.1, random.basis.2, random.basis.3, n, G.2, G.3, K, params)

## Plot the functions
figure7 = ggplot(data = dat, aes(x = Time, y = Y_Smooth_Fitted_JAGS_Spline, group = ID, color = Group)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  xlab("Time") +
  ylab("Y")





###############################
## Save the Analysis Results ##
###############################

## Remove un-needed objects
rm(X, X_2, Y)

## Save the remaining objects
save(list = ls(), file = "output/analysisresults.rda")


