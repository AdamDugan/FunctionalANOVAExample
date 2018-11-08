---
title: "Functional ANOVA: Analyze Simulated Data Using JAGS"
date: 'November 08, 2018'
output:
  html_document:
    keep_md: true
  pdf_document: default
---











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
\pagebreak

#### Figure 1: The true effect functions along with the simulated data.
<img src="03-markdown_files/figure-html/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />




<br>
<br>
\pagebreak

#### Figure 2: The smoothed subject-specific curves from the simulated data.
<img src="03-markdown_files/figure-html/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />




<br>
<br>
\pagebreak

#### Figure 3: The group-level mean functions for the smoothed simulated data $\pm$ 2 point-wise standard deviations.
<img src="03-markdown_files/figure-html/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />





<br>
<br>
\pagebreak

## Analyze Using lm()

#### Figure 4: The group-level mean functions from lm() using a 5th-order linear model with time-by-group and $\text{Time}^2$-by-group interactions.
<img src="03-markdown_files/figure-html/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />





<br>
<br>
\pagebreak

## Analyze Using Penalized Splines and lme()

#### Figure 5: The group-level mean functions using penalized splines and lme().
<img src="03-markdown_files/figure-html/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />





<br>
<br>
\pagebreak

## Analyze Using JAGS

#### Figure 6: The group-level mean functions from JAGS using a 5th-order linear model with time-by-group and $\text{Time}^2$-by-group interactions.
<img src="03-markdown_files/figure-html/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />





<br>
<br>
\pagebreak

## Analyze Using Penalized Splines and JAGS

#### Figure 7: The group-level mean functions using penalized splines and JAGS.
<img src="03-markdown_files/figure-html/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />





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
## [4] Matrix_1.2-14      nlme_3.1-137       RColorBrewer_1.1-2
## [7] ggplot2_3.1.0      knitr_1.20        
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.19     nloptr_1.2.1     pillar_1.3.0     compiler_3.5.1  
##  [5] plyr_1.8.4       bindr_0.1.1      tools_3.5.1      digest_0.6.18   
##  [9] lattice_0.20-35  evaluate_0.12    tibble_1.4.2     gtable_0.2.0    
## [13] pkgconfig_2.0.2  rlang_0.3.0.1    rstudioapi_0.8   yaml_2.2.0      
## [17] xfun_0.4         bindrcpp_0.2.2   withr_2.1.2      dplyr_0.7.7     
## [21] stringr_1.3.1    rprojroot_1.3-2  grid_3.5.1       tidyselect_0.2.5
## [25] glue_1.3.0       R6_2.3.0         rmarkdown_1.10   minqa_1.2.4     
## [29] purrr_0.2.5      magrittr_1.5     MASS_7.3-51      splines_3.5.1   
## [33] scales_1.0.0     backports_1.1.2  htmltools_0.3.6  rsconnect_0.8.8 
## [37] assertthat_0.2.0 colorspace_1.3-2 labeling_0.3     tinytex_0.9     
## [41] stringi_1.2.4    lazyeval_0.2.1   munsell_0.5.0    crayon_1.3.4
```



