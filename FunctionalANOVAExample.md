---
title: "Functional ANOVA Example"
date: 'November 13, 2018'
output:
  html_document:
    keep_md: true
  pdf_document: default
---











<br>
<br>

## Simulate Three-Group One-Way Functional ANOVA Data

Three-group one-way functional ANOVA data were simulated using methods described in the paper "Fast Function-on-Scalar Regression with Penalized Basis Expansions" from Reiss et al:

$$
\begin{aligned}
y_{i}(t) &= \mu(t) + \beta_{gp(i)}(t) + \epsilon_{i}(t) \\
\mu(t) &= 0.4\arctan(10x - 5) + 0.6 \\
\beta_{1}(t) &= -0.5 e^{-10t} -0.04\sin(8t) - 0.3t + 0.5 \\
\beta_{2}(t) &= -(t - 0.5)^{2} - 0.15 \sin(13t) \\
\beta_{3}(t) &= -\beta_{1}(t) - \beta_{2}(t)
\end{aligned}
$$
where $t = m/200$ for $m=0,\dots,200$ and the error functions, $\epsilon_{i}(t)$, were simulated from a mean-zero Gaussian process with coveriance $V(s,t) = \sigma_{1} 0.15^{|s-t|} + \sigma_{2} \delta_{st}$ where $\delta_{st}=1$ if $s=t$ and $0$ otherwise. For the simulations, N=10 curves were simulated for each of the 3 groups, $\sigma_{1} = 0.15$, and $\sigma_{2} = 0.05$. So, we have 6030 observations from 30 curves with 10 curves per group.





<br>
<br>


```r
###############################
## Create the "True" Dataset ##
###############################

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
                       Group = as.factor(c(rep(1, length(t)),
                                           rep(2, length(t)),
                                           rep(3, length(t)))) )
```





<br>
<br>

Note that the X variable, Time, was centered and scaled.

```r
#############################
## Generate Simulated Data ##
#############################

## Create a stacked dataframe for the simulated data
dat = data.frame(Y_True = c(rep(mean_1, N),
                            rep(mean_2, N),
                            rep(mean_3, N)),
                 Time = rep(t, N*g),
                 Group = as.factor( c(rep(1, N*length(t)),
                                      rep(2, N*length(t)),
                                      rep(3, N*length(t))) ),
                 ID = factor( rep(1:(N*g),
                                  length(t))[ order(rep(1:(N*g), length(t))) ] ) )
rm(mean_1, mean_2, mean_3)

## Define values
sigma_1 = 0.15    ## Simulations used 0.05 and 0.15
sigma_2 = 0.05    ## 0.05 for all simulations

## Calculate the covariance matrix
grid = expand.grid(t, t)
cov.matrix = apply(X = grid, MARGIN = 1,
                   FUN = function(x){ ((sigma_1^2)*0.15^(abs(x[1] - x[2]) )) +
                       (sigma_2^2)*as.numeric(x[1] == x[2]) } )
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
  dat$Y_Smooth[start:end] = with(dat,
                                 smooth.spline(Time[start:end], Y_Sim[start:end],
                                               nknots = 20))$y
}
rm(i, start, end, t, mu.t)

## Center and scale the time variable
dat$Time_Standardized = scale(dat$Time)

## Calculate higher-order time terms
dat$Time_Stan_2 = dat$Time_Standardized^2
dat$Time_Stan_3 = dat$Time_Standardized^3
dat$Time_Stan_4 = dat$Time_Standardized^4
dat$Time_Stan_5 = dat$Time_Standardized^5
```










<br>
<br>
\pagebreak

#### Figure 1: The true effect functions along with the simulated data.
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />





<br>
<br>
\pagebreak

#### Figure 2: The smoothed subject-specific curves from the simulated data.
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-19-1.png" style="display: block; margin: auto;" />





<br>
<br>
\pagebreak

#### Figure 3: The group-level mean functions for the smoothed simulated data $\pm$ 2 point-wise standard deviations.
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-20-1.png" style="display: block; margin: auto;" />





<br>
<br>
\pagebreak

## Analyze Using lm()

Fit a 5th-order linear model with time-by-group and $\text{Time}^2$-by-group interactions using lm():

```r
## Fit the model
mod = lm(Y_Smooth ~ Group + Time_Standardized + Time_Stan_2 + Time_Stan_3 +
           Time_Stan_4 + Time_Stan_5 +
           Group*Time_Standardized + Group*Time_Stan_2, data = dat)

## Calculate the fitted values
dat$Y_Smooth_Fitted_lm = fitted(mod)
```












<br>
<br>

#### Figure 4: The group-level mean functions from lm() using a 5th-order linear model with time-by-group and $\text{Time}^2$-by-group interactions.
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-23-1.png" style="display: block; margin: auto;" />





<br>
<br>
\pagebreak

## Analyze Using Penalized Splines and lme()

Fit a penalized spline using lme():

```r
## Penalized basis functions
K = 24
qtiles = seq(0, 1, length = K + 2)[-c(1, K + 2)] 
knots = quantile(dat$Time_Standardized, qtiles)
random.basis = cbind(sapply(knots,
                            function(k){
                              I((dat$Time_Standardized - k)^2)*(dat$Time_Standardized - k > 0)
                              }
                            )
                     )
rm(qtiles)

## Define the fixed basis
fixed.basis = cbind(1,
                    as.numeric(dat$Group == "2"),
                    as.numeric(dat$Group == "3"),
                    dat$Time_Standardized,
                    dat$Time_Stan_2)

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
dat$Y_Smooth_Fitted_lme = as.vector( cbind(fixed.basis,
                                           random.basis.1,
                                           random.basis.2,
                                           random.basis.3) %*% coefs )

## Remove R objects
rm(fixed.basis, random.basis, coefs, knots, Y)
```











<br>
<br>
\pagebreak

#### Figure 5: The group-level mean functions using penalized splines and lme().
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-26-1.png" style="display: block; margin: auto;" />





<br>
<br>
\pagebreak

## Analyze Using JAGS

Fit a 5th-order linear model with time-by-group and $\text{Time}^2$-by-group interactions using JAGS:

```r
## Define values
n = nrow(dat)
Y = dat$Y_Smooth
X = c(dat$Time_Standardized)
X_2 = c(dat$Time_Stan_2)
X_3 = c(dat$Time_Stan_3)
X_4 = c(dat$Time_Stan_4)
X_5 = c(dat$Time_Stan_5)
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
                   data = list(Y = Y,
                               n = n,
                               X = X,
                               X_2 = X_2,
                               X_3 = X_3,
                               X_4 = X_4,
                               X_5 = X_5,
                               G.2 = G.2,
                               G.3 = G.3),
                   n.chains = 3,
                   n.adapt  = 1000)
```

```
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
## Graph information:
##    Observed stochastic nodes: 6030
##    Unobserved stochastic nodes: 13
##    Total graph size: 50880
## 
## Initializing model
```

```r
## Burn-in samples
update(model,
       1000,
       progress.bar = "none")

## Draw additional samples
samp = coda.samples(model,
                    variable.names = c("beta","sigma"),
                    thin = 3,
                    n.iter = 5000,
                    progress.bar = "none")
```











<br>
<br>

Investigate the fit of the model and extract the fitted values:

```r
## Investigate the model
summary(samp)
```

```
## 
## Iterations = 1003:5998
## Thinning interval = 3 
## Number of chains = 3 
## Sample size per chain = 1666 
## 
## 1. Empirical mean and standard deviation for each variable,
##    plus standard error of the mean:
## 
##                Mean        SD  Naive SE Time-series SE
## beta[1]   0.9498163 0.0028781 4.071e-05      1.356e-04
## beta[2]   0.8129251 0.0042336 5.988e-05      3.283e-04
## beta[3]  -0.0860853 0.0042950 6.075e-05      2.611e-04
## beta[4]  -0.3800378 0.0050499 7.143e-05      5.315e-04
## beta[5]   0.0002134 0.0014844 2.100e-05      8.198e-05
## beta[6]   0.0740816 0.0014806 2.094e-05      1.537e-04
## beta[7]  -0.3666056 0.0036570 5.173e-05      1.165e-04
## beta[8]  -0.7537312 0.0037231 5.266e-05      1.208e-04
## beta[9]   0.0463574 0.0024975 3.533e-05      4.793e-05
## beta[10] -0.0154825 0.0025100 3.550e-05      4.510e-05
## beta[11] -0.0066716 0.0027151 3.841e-05      8.734e-05
## beta[12]  0.2675859 0.0027305 3.862e-05      8.515e-05
## sigma     0.0785751 0.0007005 9.908e-06      9.907e-06
## 
## 2. Quantiles for each variable:
## 
##               2.5%        25%        50%       75%     97.5%
## beta[1]   0.944052  0.9479298  0.9498256  0.951747  0.955491
## beta[2]   0.804890  0.8100657  0.8128815  0.815835  0.821151
## beta[3]  -0.094607 -0.0889633 -0.0860828 -0.083150 -0.077918
## beta[4]  -0.389897 -0.3835498 -0.3799169 -0.376516 -0.370042
## beta[5]  -0.002614 -0.0007777  0.0002054  0.001208  0.003158
## beta[6]   0.071102  0.0730756  0.0740686  0.075115  0.076906
## beta[7]  -0.373597 -0.3690784 -0.3666754 -0.364101 -0.359461
## beta[8]  -0.761008 -0.7562844 -0.7538003 -0.751202 -0.746320
## beta[9]   0.041567  0.0446359  0.0463062  0.048068  0.051265
## beta[10] -0.020303 -0.0172137 -0.0154819 -0.013845 -0.010498
## beta[11] -0.012031 -0.0084243 -0.0067044 -0.004871 -0.001247
## beta[12]  0.262099  0.2657705  0.2676139  0.269386  0.272910
## sigma     0.077257  0.0780892  0.0785684  0.079055  0.079966
```

```r
plot(samp)
```

<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-29-1.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-29-2.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-29-3.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-29-4.png" style="display: block; margin: auto;" />

```r
## Gelman-Rubin convergence diagnostic
gelman.diag(x = samp,
            confidence = 0.95,
            transform = FALSE, 
            autoburnin = FALSE)
```

```
## Potential scale reduction factors:
## 
##          Point est. Upper C.I.
## beta[1]        1.01       1.02
## beta[2]        1.01       1.04
## beta[3]        1.01       1.03
## beta[4]        1.01       1.04
## beta[5]        1.01       1.04
## beta[6]        1.01       1.04
## beta[7]        1.00       1.01
## beta[8]        1.00       1.00
## beta[9]        1.00       1.00
## beta[10]       1.00       1.00
## beta[11]       1.00       1.00
## beta[12]       1.00       1.00
## sigma          1.00       1.00
## 
## Multivariate psrf
## 
## 1.02
```

```r
## Save the coefficients from the model
coefs = data.frame( summary(samp)$statistics )

## Extract the beta estimates
betas = coefs$Mean[ row.names(coefs) %in% paste0( paste0("beta[",1:12), "]") ]

## Calculate the fitted values
dat$Y_Smooth_Fitted_JAGS_5 = c(t(betas) %*% t(cbind(1, X, X_2, X_3, X_4, X_5,
                                                    G.2, G.3, X*G.2, X*G.3,
                                                    X_2*G.2, X_2*G.3)) )

## Remove R objects
rm(model, samp, coefs, model_string, n, X_3, X_4, X_5, betas)
```





<br>
<br>

#### Figure 6: The group-level mean functions from JAGS using a 5th-order linear model with time-by-group and $\text{Time}^2$-by-group interactions.
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-30-1.png" style="display: block; margin: auto;" />





<br>
<br>
\pagebreak

## Analyze Using Penalized Splines and JAGS

Fit a penalized spline using JAGS:

```r
# Define values
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
                   data = list(Y = Y,
                               n = n,
                               X = X,
                               X_2 = X_2,
                               G.2 = G.2,
                               G.3 = G.3,
                               K = K,
                               random.basis.1 = random.basis.1,
                               random.basis.2 = random.basis.2,
                               random.basis.3 = random.basis.3),
                   n.chains = 3,
                   n.adapt  = 1000)
```

```
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
## Graph information:
##    Observed stochastic nodes: 6030
##    Unobserved stochastic nodes: 78
##    Total graph size: 472577
## 
## Initializing model
```

```r
## Burn-in samples
update(model,
       1000,
       progress.bar = "none")

## Draw additional samples
samp = coda.samples(model,
                    variable.names = c("beta","alpha","sigma"),
                    thin = 3,
                    n.iter = 5000,
                    progress.bar = "none")
```











<br>
<br>

Investigate the fit of the model and extract the fitted values:

```r
## Investigate the model
summary(samp)
```

```
## 
## Iterations = 1003:5998
## Thinning interval = 3 
## Number of chains = 3 
## Sample size per chain = 1666 
## 
## 1. Empirical mean and standard deviation for each variable,
##    plus standard error of the mean:
## 
##                Mean        SD  Naive SE Time-series SE
## alpha[1]   0.100930  0.027966 0.0003956       0.011751
## alpha[2]   0.078798  0.036500 0.0005163       0.009715
## alpha[3]   0.056604  0.037357 0.0005284       0.004359
## alpha[4]  -0.005973  0.033341 0.0004716       0.014493
## alpha[5]  -0.096058  0.057185 0.0008089       0.017235
## alpha[6]  -0.108279  0.048129 0.0006808       0.022255
## alpha[7]  -0.064669  0.049943 0.0007064       0.010239
## alpha[8]  -0.044576  0.080929 0.0011447       0.006145
## alpha[9]   0.002667  0.069234 0.0009793       0.034596
## alpha[10]  0.144729  0.129189 0.0018274       0.039553
## alpha[11]  0.016338  0.102333 0.0014475       0.050450
## alpha[12]  0.045884  0.133919 0.0018943       0.020413
## alpha[13] -0.310127  0.129826 0.0018364       0.047317
## alpha[14] -0.320618  0.165999 0.0023481       0.050504
## alpha[15] -0.657418  0.305633 0.0043232       0.122745
## alpha[16] -0.145183  0.300685 0.0042532       0.028571
## alpha[17]  0.134189  0.794625 0.0112399       0.124146
## alpha[18]  1.242452  0.992237 0.0140352       0.196046
## alpha[19] -0.119729  0.618490 0.0087485       0.276359
## alpha[20]  0.078660  0.916351 0.0129617       0.353865
## alpha[21] -1.342243  1.368320 0.0193548       0.275301
## alpha[22]  2.240483  1.943260 0.0274873       0.553770
## alpha[23] -1.428283  2.334249 0.0330179       0.480262
## alpha[24]  0.284780  3.075654 0.0435050       0.325066
## alpha[25]  0.113287  0.032062 0.0004535       0.012360
## alpha[26]  0.121718  0.034058 0.0004818       0.007494
## alpha[27]  0.162829  0.024198 0.0003423       0.006956
## alpha[28]  0.056885  0.015515 0.0002195       0.002846
## alpha[29] -0.012244  0.042356 0.0005991       0.017527
## alpha[30] -0.118000  0.077256 0.0010928       0.035564
## alpha[31] -0.262968  0.120106 0.0016989       0.033255
## alpha[32] -0.264538  0.102763 0.0014536       0.027745
## alpha[33] -0.421042  0.039709 0.0005617       0.011429
## alpha[34] -0.254550  0.090651 0.0012823       0.037159
## alpha[35]  0.023060  0.178190 0.0025205       0.095078
## alpha[36]  0.171307  0.251137 0.0035523       0.121035
## alpha[37]  0.663850  0.212615 0.0030074       0.081267
## alpha[38]  0.499978  0.178612 0.0025265       0.054399
## alpha[39]  0.440863  0.325187 0.0045998       0.172547
## alpha[40] -0.479081  0.762401 0.0107841       0.388678
## alpha[41] -1.441449  0.613626 0.0086797       0.224399
## alpha[42] -0.532920  0.986692 0.0139567       0.438576
## alpha[43] -0.029578  1.998136 0.0282636       1.222098
## alpha[44]  1.524112  1.743863 0.0246669       0.640876
## alpha[45] -0.601900  4.442419 0.0628379       4.035196
## alpha[46] -1.592208  6.153887 0.0870465       3.230529
## alpha[47]  2.706728  6.396442 0.0904774       2.152973
## alpha[48] -1.472881  6.177511 0.0873807       1.467427
## alpha[49] -0.235699  0.059984 0.0008485       0.023033
## alpha[50] -0.234486  0.050199 0.0007101       0.019423
## alpha[51] -0.198749  0.040514 0.0005731       0.016795
## alpha[52] -0.080874  0.041274 0.0005838       0.010447
## alpha[53]  0.086781  0.067793 0.0009589       0.030841
## alpha[54]  0.337788  0.114112 0.0016141       0.052327
## alpha[55]  0.490446  0.134226 0.0018986       0.059109
## alpha[56]  0.811950  0.160773 0.0022741       0.063624
## alpha[57]  0.732141  0.115570 0.0016347       0.033020
## alpha[58]  0.458148  0.161818 0.0022889       0.064282
## alpha[59]  0.011864  0.286445 0.0040518       0.143303
## alpha[60] -0.869500  0.483225 0.0068352       0.217742
## alpha[61] -1.885156  0.485883 0.0068728       0.201628
## alpha[62] -2.186916  0.221571 0.0031341       0.060569
## alpha[63] -1.251156  0.876006 0.0123911       0.506570
## alpha[64]  0.771431  1.449645 0.0205052       0.707802
## alpha[65]  3.361628  1.250984 0.0176951       0.458808
## alpha[66]  3.071022  1.877901 0.0265628       1.096287
## alpha[67] -1.309549  4.208079 0.0595231       2.226257
## alpha[68] -4.778068  3.421383 0.0483953       1.326012
## alpha[69]  3.204080 10.345038 0.1463302       7.957917
## alpha[70]  3.096795 13.611125 0.1925289       9.859651
## alpha[71] -5.784650 12.788305 0.1808901       4.873187
## alpha[72]  4.087709 11.976748 0.1694107       3.263571
## beta[1]    0.648118  0.026778 0.0003788       0.003462
## beta[2]   -0.467545  0.006271 0.0000887       0.001124
## beta[3]   -0.153593  0.027350 0.0003869       0.010484
## beta[4]    0.536610  0.042813 0.0006056       0.006721
## beta[5]    0.178873  0.018003 0.0002546       0.003483
## sigma      0.059361  0.010073 0.0001425       0.004041
## 
## 2. Quantiles for each variable:
## 
##                 2.5%       25%       50%      75%     97.5%
## alpha[1]    0.047293   0.08004  0.101578  0.12354  0.146093
## alpha[2]    0.029944   0.05307  0.068540  0.10624  0.146159
## alpha[3]   -0.008068   0.01860  0.065396  0.08714  0.111874
## alpha[4]   -0.076524  -0.02794 -0.002424  0.01883  0.054605
## alpha[5]   -0.197435  -0.13819 -0.095931 -0.05789  0.007078
## alpha[6]   -0.182902  -0.14822 -0.117389 -0.06732 -0.019769
## alpha[7]   -0.157272  -0.10543 -0.051672 -0.03100  0.019220
## alpha[8]   -0.174667  -0.14077 -0.025656  0.01549  0.070274
## alpha[9]   -0.125676  -0.04398 -0.011758  0.04337  0.161633
## alpha[10]  -0.074681   0.05117  0.141211  0.24661  0.403021
## alpha[11]  -0.217362  -0.06706  0.049812  0.10160  0.151017
## alpha[12]  -0.142933  -0.04109 -0.012227  0.17839  0.313697
## alpha[13]  -0.647852  -0.38407 -0.287852 -0.20824 -0.132210
## alpha[14]  -0.599348  -0.45037 -0.335141 -0.20704 -0.001277
## alpha[15]  -1.206294  -0.92601 -0.603356 -0.44497 -0.035589
## alpha[16]  -0.514887  -0.39059 -0.256471  0.19543  0.385954
## alpha[17]  -0.996223  -0.66020  0.004191  0.83392  1.609224
## alpha[18]  -0.209923   0.16176  1.287847  1.94061  2.939496
## alpha[19]  -0.983770  -0.57641 -0.290983  0.29516  1.166444
## alpha[20]  -1.362377  -0.68416  0.050612  0.81082  1.693354
## alpha[21]  -3.778726  -2.35493 -1.314154 -0.31006  0.981806
## alpha[22]  -0.841118   0.66463  1.902209  3.71770  6.002177
## alpha[23]  -6.625244  -2.90501 -1.196316  0.11384  2.672067
## alpha[24]  -5.690882  -1.81326  0.238137  2.32893  6.464818
## alpha[25]   0.041168   0.09576  0.116262  0.13325  0.174553
## alpha[26]   0.060789   0.09664  0.120278  0.13676  0.187569
## alpha[27]   0.102637   0.14866  0.165401  0.17998  0.198580
## alpha[28]   0.032816   0.04416  0.054491  0.06932  0.088677
## alpha[29]  -0.112833  -0.03444 -0.004672  0.01272  0.059906
## alpha[30]  -0.271205  -0.16486 -0.123564 -0.05647  0.009928
## alpha[31]  -0.526574  -0.32440 -0.230806 -0.19008 -0.055224
## alpha[32]  -0.433950  -0.33742 -0.258345 -0.19285 -0.048811
## alpha[33]  -0.465962  -0.44495 -0.432805 -0.41290 -0.308446
## alpha[34]  -0.367381  -0.32871 -0.286615 -0.18711 -0.051783
## alpha[35]  -0.251769  -0.14065  0.025786  0.19709  0.297711
## alpha[36]  -0.280661  -0.04205  0.176956  0.37829  0.616169
## alpha[37]   0.131475   0.54131  0.778938  0.81276  0.852291
## alpha[38]   0.174785   0.39393  0.519614  0.61222  0.823250
## alpha[39]  -0.272606   0.15436  0.539956  0.69763  0.902438
## alpha[40]  -1.489112  -1.13476 -0.683639  0.12227  1.040851
## alpha[41]  -2.233545  -1.93548 -1.535154 -1.12488  0.051466
## alpha[42]  -2.159974  -1.30106 -0.572243  0.14997  1.494590
## alpha[43]  -3.575204  -1.77074  0.459928  1.75632  2.624507
## alpha[44]  -2.370310   0.20247  1.868497  2.97527  4.168005
## alpha[45]  -5.988655  -4.47707 -1.717184  2.98640  8.058024
## alpha[46] -13.285008  -6.62497 -1.098225  4.26474  6.953267
## alpha[47] -11.686552  -1.75901  2.917651  7.73879 13.184019
## alpha[48] -11.370401  -6.10222 -1.735193  2.08637 13.947003
## alpha[49]  -0.347141  -0.27583 -0.241262 -0.19435 -0.119678
## alpha[50]  -0.319150  -0.28059 -0.233899 -0.19898 -0.139042
## alpha[51]  -0.265241  -0.22665 -0.205235 -0.17389 -0.100241
## alpha[52]  -0.140907  -0.11782 -0.088868 -0.04530 -0.014693
## alpha[53]  -0.031882   0.03240  0.089503  0.14423  0.214799
## alpha[54]   0.113684   0.24160  0.357336  0.43653  0.488653
## alpha[55]   0.239875   0.38977  0.501778  0.60166  0.689175
## alpha[56]   0.465805   0.70898  0.843856  0.92840  1.064332
## alpha[57]   0.429807   0.69308  0.745316  0.80761  0.912119
## alpha[58]   0.120193   0.34561  0.477784  0.57070  0.724619
## alpha[59]  -0.490775  -0.22733  0.010368  0.27412  0.515033
## alpha[60]  -1.523015  -1.29333 -0.951825 -0.50611  0.105058
## alpha[61]  -2.419548  -2.24961 -2.082613 -1.57211 -0.766763
## alpha[62]  -2.517001  -2.34725 -2.209168 -2.08798 -1.620869
## alpha[63]  -2.475773  -2.06951 -1.381585 -0.43689  0.249158
## alpha[64]  -2.080971  -0.41342  1.068498  2.03974  2.721522
## alpha[65]   0.024690   2.98236  3.793321  4.29084  4.686094
## alpha[66]  -0.367334   1.51554  3.581731  4.75391  5.446358
## alpha[67]  -6.676257  -4.71151 -2.619764  2.21040  6.749500
## alpha[68]  -9.543722  -7.94542 -5.346663 -1.94895  1.867646
## alpha[69] -15.062277  -7.12924  7.574215 12.27541 14.837350
## alpha[70] -16.538591 -10.53068  2.876286 15.87447 24.006679
## alpha[71] -24.373764 -17.79959 -6.592537  5.24215 19.083462
## alpha[72] -26.207740  -3.40406  5.929935 14.09868 20.538086
## beta[1]     0.614863   0.62543  0.640737  0.67161  0.699686
## beta[2]    -0.477223  -0.47183 -0.468662 -0.46436 -0.452254
## beta[3]    -0.214704  -0.17209 -0.148033 -0.13036 -0.117599
## beta[4]     0.484343   0.50431  0.523144  0.57483  0.622693
## beta[5]     0.154080   0.16643  0.174332  0.19026  0.215633
## sigma       0.046948   0.05064  0.056811  0.06646  0.081868
```

```r
plot(samp)
```

<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-1.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-2.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-3.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-4.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-5.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-6.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-7.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-8.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-9.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-10.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-11.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-12.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-13.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-14.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-15.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-16.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-17.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-18.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-19.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-33-20.png" style="display: block; margin: auto;" />

```r
## Save the coefficients from the model
coefs = data.frame( summary(samp)$statistics )

## Gelman-Rubin convergence diagnostic
gelman.diag(x = samp,
            confidence = 0.95,
            transform = FALSE, 
            autoburnin = FALSE)
```

```
## Potential scale reduction factors:
## 
##           Point est. Upper C.I.
## alpha[1]        1.12       1.36
## alpha[2]        2.99       6.52
## alpha[3]        5.49      10.94
## alpha[4]        1.30       1.85
## alpha[5]        1.78       2.98
## alpha[6]        1.04       1.13
## alpha[7]        3.48       6.99
## alpha[8]        7.05      14.55
## alpha[9]        1.23       1.98
## alpha[10]       1.85       3.21
## alpha[11]       1.09       1.17
## alpha[12]       3.81       8.82
## alpha[13]       1.55       2.52
## alpha[14]       2.10       3.71
## alpha[15]       1.71       2.75
## alpha[16]       5.46      11.09
## alpha[17]       4.71       8.91
## alpha[18]       4.02      11.01
## alpha[19]       2.14       4.64
## alpha[20]       1.88       3.56
## alpha[21]       2.63       4.80
## alpha[22]       1.49       2.37
## alpha[23]       1.12       1.39
## alpha[24]       1.04       1.09
## alpha[25]       1.47       2.20
## alpha[26]       2.81       5.35
## alpha[27]       1.64       2.69
## alpha[28]       2.16       3.79
## alpha[29]       1.44       2.63
## alpha[30]       1.33       2.09
## alpha[31]       2.20       4.25
## alpha[32]       2.44       4.42
## alpha[33]       1.04       1.07
## alpha[34]       1.44       2.38
## alpha[35]       1.04       1.11
## alpha[36]       1.09       1.26
## alpha[37]       1.02       1.08
## alpha[38]       1.59       2.47
## alpha[39]       1.07       1.15
## alpha[40]       1.02       1.03
## alpha[41]       1.15       1.47
## alpha[42]       1.46       2.29
## alpha[43]       1.03       1.10
## alpha[44]       1.09       1.29
## alpha[45]       1.09       1.27
## alpha[46]       1.05       1.16
## alpha[47]       1.02       1.08
## alpha[48]       1.01       1.04
## alpha[49]       1.46       2.26
## alpha[50]       1.35       2.10
## alpha[51]       1.14       1.42
## alpha[52]       2.73       6.08
## alpha[53]       1.12       1.35
## alpha[54]       1.00       1.01
## alpha[55]       1.01       1.04
## alpha[56]       1.19       1.54
## alpha[57]       1.69       2.70
## alpha[58]       1.79       3.01
## alpha[59]       1.03       1.09
## alpha[60]       1.03       1.09
## alpha[61]       1.03       1.11
## alpha[62]       1.22       1.66
## alpha[63]       1.02       1.06
## alpha[64]       1.01       1.03
## alpha[65]       1.05       1.15
## alpha[66]       1.01       1.03
## alpha[67]       1.03       1.12
## alpha[68]       1.08       1.25
## alpha[69]       1.01       1.01
## alpha[70]       1.00       1.01
## alpha[71]       1.00       1.00
## alpha[72]       1.00       1.00
## beta[1]         3.75       7.52
## beta[2]         1.03       1.09
## beta[3]         1.01       1.05
## beta[4]         3.61       7.54
## beta[5]         2.79       5.65
## sigma           1.00       1.00
## 
## Multivariate psrf
## 
## 43.4
```

```r
## Save the lambda estimates
#var.e4
#var.u4

## Extract the parameter estimates and order them c(fixed, random)
params = c( coefs$Mean[ row.names(coefs) %in% paste0( paste0("beta[",1:5),"]") ],
            coefs$Mean[ row.names(coefs) %in% paste0( paste0("alpha[",1:72),"]") ] )

## Calculate the fitted values
dat$Y_Smooth_Fitted_JAGS_Spline = c(t(params) %*% t(cbind(1,
                                                          G.2,
                                                          G.3,
                                                          X,
                                                          X_2,
                                                          random.basis.1,
                                                          random.basis.2,
                                                          random.basis.3)) )
rm(coefs, model, samp, model_string, random.basis.1, random.basis.2,
   random.basis.3, n, G.2, G.3, K, params)
```





<br>
<br>

#### Figure 7: The group-level mean functions using penalized splines and JAGS.
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-34-1.png" style="display: block; margin: auto;" />





<br>
<br>
\pagebreak

#### Figure 8: A comparison of the fitted effect functions from each model.
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-35-1.png" style="display: block; margin: auto;" />











<br>
<br>
\pagebreak

## Session Info

A summary of the R session used for the analysis.

```
## R version 3.5.1 (2018-07-02)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1
## 
## Matrix products: default
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] rjags_4-8          coda_0.19-2        lme4_1.1-18-1     
## [4] Matrix_1.2-15      nlme_3.1-137       RColorBrewer_1.1-2
## [7] ggplot2_3.1.0      knitr_1.20         MASS_7.3-51.1     
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.0       pillar_1.3.0     compiler_3.5.1   nloptr_1.2.1    
##  [5] plyr_1.8.4       bindr_0.1.1      tools_3.5.1      digest_0.6.18   
##  [9] evaluate_0.12    tibble_1.4.2     gtable_0.2.0     lattice_0.20-38 
## [13] pkgconfig_2.0.2  rlang_0.3.0.1    rstudioapi_0.8   yaml_2.2.0      
## [17] bindrcpp_0.2.2   stringr_1.3.1    withr_2.1.2      dplyr_0.7.7     
## [21] rprojroot_1.3-2  grid_3.5.1       tidyselect_0.2.5 glue_1.3.0      
## [25] R6_2.3.0         rmarkdown_1.10   minqa_1.2.4      purrr_0.2.5     
## [29] magrittr_1.5     backports_1.1.2  htmltools_0.3.6  scales_1.0.0    
## [33] splines_3.5.1    assertthat_0.2.0 colorspace_1.3-2 labeling_0.3    
## [37] stringi_1.2.4    lazyeval_0.2.1   munsell_0.5.0    crayon_1.3.4
```


