---
title: "Functional ANOVA Example"
date: 'November 09, 2018'
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
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />





<br>
<br>
\pagebreak

#### Figure 2: The smoothed subject-specific curves from the simulated data.
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />





<br>
<br>
\pagebreak

#### Figure 3: The group-level mean functions for the smoothed simulated data $\pm$ 2 point-wise standard deviations.
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />





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
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />





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
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />





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
## beta[1]   0.9498530 0.0028624 4.049e-05      1.231e-04
## beta[2]   0.8130485 0.0045886 6.491e-05      3.993e-04
## beta[3]  -0.0861237 0.0043869 6.205e-05      2.691e-04
## beta[4]  -0.3802486 0.0057118 8.079e-05      7.336e-04
## beta[5]   0.0002175 0.0015192 2.149e-05      8.042e-05
## beta[6]   0.0741531 0.0016719 2.365e-05      1.758e-04
## beta[7]  -0.3666674 0.0037397 5.290e-05      1.148e-04
## beta[8]  -0.7537375 0.0036498 5.163e-05      1.093e-04
## beta[9]   0.0463140 0.0024855 3.516e-05      4.571e-05
## beta[10] -0.0154927 0.0024697 3.493e-05      4.429e-05
## beta[11] -0.0066248 0.0027505 3.891e-05      8.438e-05
## beta[12]  0.2676217 0.0027170 3.843e-05      7.992e-05
## sigma     0.0785640 0.0007043 9.963e-06      9.963e-06
## 
## 2. Quantiles for each variable:
## 
##               2.5%        25%        50%       75%     97.5%
## beta[1]   0.944450  0.9478468  0.9497829  0.951798  0.955547
## beta[2]   0.804406  0.8098595  0.8129109  0.816134  0.822332
## beta[3]  -0.094411 -0.0891581 -0.0860574 -0.083044 -0.077776
## beta[4]  -0.392637 -0.3838615 -0.3799867 -0.376185 -0.370139
## beta[5]  -0.002703 -0.0008168  0.0002297  0.001256  0.003116
## beta[6]   0.071138  0.0729761  0.0740815  0.075170  0.077818
## beta[7]  -0.374038 -0.3691921 -0.3666630 -0.364093 -0.359528
## beta[8]  -0.760865 -0.7562031 -0.7537311 -0.751345 -0.746446
## beta[9]   0.041412  0.0446403  0.0463454  0.048022  0.051082
## beta[10] -0.020284 -0.0171763 -0.0155193 -0.013820 -0.010615
## beta[11] -0.012071 -0.0084820 -0.0066064 -0.004763 -0.001228
## beta[12]  0.262236  0.2658085  0.2676328  0.269467  0.272924
## sigma     0.077219  0.0780860  0.0785452  0.079038  0.079985
```

```r
plot(samp)
```

<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-18-1.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-18-2.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-18-3.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-18-4.png" style="display: block; margin: auto;" />

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
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-19-1.png" style="display: block; margin: auto;" />





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
##                 Mean        SD  Naive SE Time-series SE
## alpha[1]   9.843e-02  0.031042 0.0004391       0.011388
## alpha[2]   9.612e-02  0.034465 0.0004875       0.009807
## alpha[3]   1.577e-02  0.051072 0.0007224       0.007766
## alpha[4]   1.031e-05  0.049358 0.0006982       0.016303
## alpha[5]  -1.917e-02  0.056470 0.0007988       0.015754
## alpha[6]  -9.023e-02  0.062454 0.0008834       0.015533
## alpha[7]  -1.220e-01  0.053317 0.0007542       0.009565
## alpha[8]  -2.440e-02  0.065134 0.0009213       0.012931
## alpha[9]  -3.232e-02  0.057137 0.0008082       0.016546
## alpha[10]  1.034e-01  0.117013 0.0016551       0.053333
## alpha[11]  1.384e-01  0.114631 0.0016215       0.036047
## alpha[12] -6.784e-02  0.081508 0.0011529       0.039496
## alpha[13] -2.445e-01  0.084890 0.0012008       0.032490
## alpha[14] -3.461e-01  0.221411 0.0031319       0.080247
## alpha[15] -5.558e-01  0.294054 0.0041594       0.091507
## alpha[16] -4.420e-01  0.141864 0.0020067       0.017208
## alpha[17]  6.439e-01  0.509028 0.0072002       0.081737
## alpha[18]  5.620e-01  0.635577 0.0089902       0.114913
## alpha[19]  7.883e-01  0.483354 0.0068370       0.202265
## alpha[20] -1.127e+00  0.831864 0.0117667       0.281105
## alpha[21] -1.565e-01  1.229285 0.0173882       0.296409
## alpha[22]  1.591e+00  1.957286 0.0276857       0.483493
## alpha[23] -1.452e+00  2.521813 0.0356710       0.423094
## alpha[24]  8.373e-01  3.235315 0.0457634       0.285319
## alpha[25]  1.151e-01  0.039157 0.0005539       0.008784
## alpha[26]  1.384e-01  0.044317 0.0006269       0.010712
## alpha[27]  1.583e-01  0.026920 0.0003808       0.007876
## alpha[28]  3.545e-02  0.028517 0.0004034       0.008428
## alpha[29]  2.301e-02  0.046601 0.0006592       0.005675
## alpha[30] -1.374e-01  0.102178 0.0014453       0.040201
## alpha[31] -2.229e-01  0.072937 0.0010317       0.021629
## alpha[32] -3.490e-01  0.086766 0.0012273       0.038113
## alpha[33] -3.628e-01  0.062772 0.0008879       0.011870
## alpha[34] -1.971e-01  0.093044 0.0013161       0.042532
## alpha[35] -1.014e-01  0.203660 0.0028808       0.081103
## alpha[36]  3.038e-01  0.290174 0.0041045       0.110327
## alpha[37]  5.713e-01  0.396563 0.0056094       0.094272
## alpha[38]  6.382e-01  0.299359 0.0042344       0.059158
## alpha[39]  2.264e-01  0.441886 0.0062505       0.222008
## alpha[40] -6.575e-01  0.965769 0.0136608       0.383211
## alpha[41] -5.728e-01  0.875500 0.0123839       0.170884
## alpha[42] -1.659e+00  0.791028 0.0111891       0.417609
## alpha[43]  7.146e-01  1.697434 0.0240101       0.967107
## alpha[44]  1.554e+00  1.557699 0.0220336       0.574244
## alpha[45] -1.404e+00  3.962382 0.0560478       3.855793
## alpha[46] -3.818e-01  5.264520 0.0744664       2.567326
## alpha[47]  1.518e+00  6.088521 0.0861219       1.938404
## alpha[48] -4.396e-01  6.404307 0.0905887       1.586652
## alpha[49] -2.451e-01  0.060578 0.0008569       0.025795
## alpha[50] -2.400e-01  0.051328 0.0007260       0.020606
## alpha[51] -1.537e-01  0.025290 0.0003577       0.007387
## alpha[52] -9.159e-02  0.027938 0.0003952       0.009343
## alpha[53]  1.106e-01  0.078471 0.0011100       0.033695
## alpha[54]  2.779e-01  0.118450 0.0016755       0.055119
## alpha[55]  5.544e-01  0.142232 0.0020119       0.061078
## alpha[56]  7.402e-01  0.150928 0.0021349       0.064126
## alpha[57]  7.727e-01  0.126162 0.0017846       0.037575
## alpha[58]  5.300e-01  0.168718 0.0023865       0.067590
## alpha[59] -9.253e-03  0.298290 0.0042193       0.145329
## alpha[60] -9.471e-01  0.470810 0.0066596       0.211649
## alpha[61] -1.964e+00  0.514542 0.0072782       0.224045
## alpha[62] -1.983e+00  0.257641 0.0036443       0.081409
## alpha[63] -1.335e+00  0.851061 0.0120382       0.463153
## alpha[64]  6.972e-01  1.437555 0.0203342       0.724291
## alpha[65]  3.274e+00  1.285856 0.0181884       0.480196
## alpha[66]  3.649e+00  1.897094 0.0268343       1.097241
## alpha[67] -2.094e+00  4.246683 0.0600692       2.350179
## alpha[68] -4.223e+00  3.549397 0.0502061       1.271504
## alpha[69]  2.764e+00 10.572825 0.1495522       7.472435
## alpha[70]  3.572e+00 13.771839 0.1948022      10.086637
## alpha[71] -6.074e+00 12.843285 0.1816678       5.172560
## alpha[72]  4.009e+00 11.960810 0.1691852       3.297710
## beta[1]    6.176e-01  0.031240 0.0004419       0.003670
## beta[2]   -4.695e-01  0.007443 0.0001053       0.001100
## beta[3]   -1.533e-01  0.028412 0.0004019       0.010707
## beta[4]    4.856e-01  0.044755 0.0006331       0.008735
## beta[5]    1.588e-01  0.017379 0.0002458       0.004390
## sigma      5.937e-02  0.009868 0.0001396       0.004016
## 
## 2. Quantiles for each variable:
## 
##                2.5%       25%       50%      75%     97.5%
## alpha[1]    0.04663   0.06861  0.098159  0.12535  0.153137
## alpha[2]    0.04954   0.07181  0.089146  0.11034  0.163560
## alpha[3]   -0.06689  -0.01949  0.003067  0.07330  0.092149
## alpha[4]   -0.10790  -0.02954  0.011985  0.03923  0.069800
## alpha[5]   -0.13128  -0.06644 -0.013308  0.02565  0.071561
## alpha[6]   -0.20459  -0.14049 -0.088449 -0.03219  0.004029
## alpha[7]   -0.23466  -0.16080 -0.103890 -0.08605 -0.044610
## alpha[8]   -0.12434  -0.07963 -0.030080  0.02673  0.092134
## alpha[9]   -0.14004  -0.07882 -0.037381  0.02288  0.056659
## alpha[10]  -0.08285   0.02075  0.092035  0.17810  0.343441
## alpha[11]  -0.11392   0.05197  0.142196  0.23568  0.313911
## alpha[12]  -0.28713  -0.10853 -0.065903 -0.01691  0.082360
## alpha[13]  -0.43629  -0.30287 -0.228011 -0.17640 -0.123541
## alpha[14]  -0.87323  -0.45795 -0.318244 -0.16121 -0.035874
## alpha[15]  -1.13187  -0.76649 -0.461493 -0.33537 -0.058776
## alpha[16]  -0.72563  -0.55212 -0.407344 -0.33465 -0.208547
## alpha[17]  -0.24191   0.29791  0.599906  1.15844  1.472414
## alpha[18]  -0.63107  -0.07552  0.777810  0.98911  1.615424
## alpha[19]  -0.03633   0.45737  0.677843  1.25844  1.565237
## alpha[20]  -2.93639  -1.76220 -0.905652 -0.53073  0.197836
## alpha[21]  -2.23653  -1.17731 -0.525831  0.94335  2.140953
## alpha[22]  -2.42600   0.43351  1.813705  2.71821  5.997911
## alpha[23]  -7.34184  -2.88122 -1.431865 -0.01846  3.372787
## alpha[24]  -5.31283  -1.35568  0.769107  2.90565  7.455977
## alpha[25]   0.04095   0.08302  0.117336  0.13715  0.190428
## alpha[26]   0.05020   0.10685  0.138520  0.17749  0.209353
## alpha[27]   0.10141   0.13873  0.166770  0.17577  0.197889
## alpha[28]  -0.01558   0.01263  0.037406  0.06299  0.078672
## alpha[29]  -0.04504  -0.02040  0.018533  0.06935  0.095434
## alpha[30]  -0.29680  -0.22267 -0.136971 -0.05766  0.077562
## alpha[31]  -0.35257  -0.26970 -0.226440 -0.17308 -0.083600
## alpha[32]  -0.48143  -0.41315 -0.375421 -0.27746 -0.169935
## alpha[33]  -0.46732  -0.42107 -0.373164 -0.30124 -0.260702
## alpha[34]  -0.35560  -0.26177 -0.206475 -0.12153 -0.031954
## alpha[35]  -0.40942  -0.26390 -0.139726  0.04886  0.324820
## alpha[36]  -0.15817   0.07404  0.306866  0.44800  0.883120
## alpha[37]  -0.09634   0.26833  0.526626  0.88627  1.222698
## alpha[38]   0.07133   0.39448  0.681175  0.90689  1.042314
## alpha[39]  -0.79517   0.01033  0.300906  0.59954  0.766891
## alpha[40]  -2.43572  -1.38862 -0.466607  0.03950  1.008151
## alpha[41]  -2.02523  -1.49252 -0.501753  0.08404  1.001847
## alpha[42]  -2.75973  -2.29888 -1.858349 -1.11341  0.030027
## alpha[43]  -2.16064  -0.70868  1.143112  2.08346  3.072543
## alpha[44]  -1.77528   0.46192  1.979921  2.89471  3.559893
## alpha[45]  -6.57902  -4.92629 -2.356818  2.28272  5.714244
## alpha[46]  -8.66932  -5.37476 -0.054048  4.25893  7.359732
## alpha[47] -12.46651  -2.28471  2.126039  6.48471 10.551319
## alpha[48]  -9.71828  -4.85220 -1.515029  2.08357 16.026171
## alpha[49]  -0.34570  -0.29158 -0.254160 -0.20004 -0.120427
## alpha[50]  -0.32756  -0.27805 -0.242952 -0.20545 -0.133752
## alpha[51]  -0.19648  -0.17530 -0.153255 -0.13077 -0.106925
## alpha[52]  -0.13180  -0.10863 -0.098070 -0.08272 -0.024470
## alpha[53]  -0.01240   0.04870  0.100606  0.18833  0.237482
## alpha[54]   0.07706   0.17669  0.281454  0.38341  0.467586
## alpha[55]   0.26133   0.45319  0.562979  0.67307  0.773012
## alpha[56]   0.41044   0.65007  0.788711  0.86508  0.917345
## alpha[57]   0.46798   0.71206  0.770709  0.84889  0.951568
## alpha[58]   0.11089   0.42457  0.557100  0.69323  0.724420
## alpha[59]  -0.48793  -0.28110 -0.020054  0.28205  0.488791
## alpha[60]  -1.59839  -1.34445 -1.034555 -0.57760 -0.004594
## alpha[61]  -2.53982  -2.37781 -2.123068 -1.59161 -0.831264
## alpha[62]  -2.38172  -2.16930 -2.022675 -1.83790 -1.337913
## alpha[63]  -2.49883  -2.15816 -1.453446 -0.56981  0.195250
## alpha[64]  -1.98808  -0.47583  1.051043  2.04433  2.464756
## alpha[65]  -0.15323   2.83754  3.775097  4.16653  4.606148
## alpha[66]  -0.07740   2.00607  4.127849  5.22122  6.444419
## alpha[67]  -7.76371  -5.88942 -3.423868  1.62531  5.874761
## alpha[68]  -9.25523  -7.12542 -4.674843 -1.56688  3.022104
## alpha[69] -14.80658  -7.68490  6.421266 11.76594 15.286022
## alpha[70] -18.12600 -10.47601  3.158583 17.04130 24.432461
## alpha[71] -24.62559 -18.02484 -6.703078  5.15544 20.145486
## alpha[72] -28.44636  -3.28266  5.756948 13.69716 20.663693
## beta[1]     0.55886   0.59511  0.625725  0.64390  0.658727
## beta[2]    -0.48351  -0.47430 -0.469876 -0.46501 -0.453365
## beta[3]    -0.21431  -0.17333 -0.147423 -0.12888 -0.116554
## beta[4]     0.40066   0.46348  0.494966  0.52068  0.546075
## beta[5]     0.12382   0.15311  0.162005  0.17117  0.184236
## sigma       0.04731   0.05078  0.057113  0.06642  0.081680
```

```r
plot(samp)
```

<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-1.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-2.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-3.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-4.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-5.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-6.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-7.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-8.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-9.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-10.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-11.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-12.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-13.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-14.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-15.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-16.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-17.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-18.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-19.png" style="display: block; margin: auto;" /><img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-22-20.png" style="display: block; margin: auto;" />

```r
## Save the coefficients from the model
coefs = data.frame( summary(samp)$statistics )

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
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-23-1.png" style="display: block; margin: auto;" />





<br>
<br>
\pagebreak

#### Figure 8: A comparison of the fitted effect functions from each model.
<img src="FunctionalANOVAExample_files/figure-html/unnamed-chunk-24-1.png" style="display: block; margin: auto;" />











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
## [1] rjags_4-8          coda_0.19-2        lme4_1.1-18-1      Matrix_1.2-14      nlme_3.1-137       RColorBrewer_1.1-2 ggplot2_3.1.0     
## [8] knitr_1.20         MASS_7.3-51       
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.19     nloptr_1.2.1     pillar_1.3.0     compiler_3.5.1   plyr_1.8.4       bindr_0.1.1      tools_3.5.1     
##  [8] digest_0.6.18    evaluate_0.12    tibble_1.4.2     gtable_0.2.0     lattice_0.20-35  pkgconfig_2.0.2  rlang_0.3.0.1   
## [15] rstudioapi_0.8   yaml_2.2.0       xfun_0.4         bindrcpp_0.2.2   stringr_1.3.1    withr_2.1.2      dplyr_0.7.7     
## [22] rprojroot_1.3-2  grid_3.5.1       tidyselect_0.2.5 glue_1.3.0       R6_2.3.0         rmarkdown_1.10   minqa_1.2.4     
## [29] purrr_0.2.5      magrittr_1.5     splines_3.5.1    backports_1.1.2  scales_1.0.0     htmltools_0.3.6  rsconnect_0.8.8 
## [36] assertthat_0.2.0 colorspace_1.3-2 labeling_0.3     tinytex_0.9      stringi_1.2.4    lazyeval_0.2.1   munsell_0.5.0   
## [43] crayon_1.3.4
```


