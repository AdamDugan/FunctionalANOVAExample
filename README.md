# FunctionalANOVAExamples

First, we'll simulate data using the methods proposed by Reiss et al in their paper *Fast Function-on-Scalar Regression with Penalized Basis Expansions*.

Then, analyze the data using these four models:
* A 5th-order linear model with interactions using lm().
* A penalized spline model using lme().
* A 5th-order linear model with interactions using JAGS.
* A penalized spline model using JAGS.
