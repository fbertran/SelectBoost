<!-- README.md is generated from README.Rmd. Please edit that file -->



# SelectBoost <img src="man/figures/logo.png" align="right" width="200"/>

# A General Algorithm to Enhance the Performance of Variable Selection Methods in Correlated Datasets
## Frédéric Bertrand and Myriam Maumy-Bertrand

<https://doi.org/10.32614/CRAN.package.SelectBoost>

<!-- badges: start -->
[![DOI](https://img.shields.io/badge/doi-10.32614/CRAN.package.SelectBoost-blue.svg)](https://doi.org/10.32614/CRAN.package.SelectBoost)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/fbertran/SelectBoost/workflows/R-CMD-check/badge.svg)](https://github.com/fbertran/SelectBoost/actions)
[![Codecov test coverage](https://codecov.io/gh/fbertran/SelectBoost/branch/master/graph/badge.svg)](https://app.codecov.io/gh/fbertran/SelectBoost?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/SelectBoost)](https://cran.r-project.org/package=SelectBoost)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/SelectBoost)](https://cran.r-project.org/package=SelectBoost)
[![GitHub Repo stars](https://img.shields.io/github/stars/fbertran/SelectBoost?style=social)](https://github.com/fbertran/SelectBoost)
<!-- badges: end -->


The SelectBoost package implements SelectBoost: a general algorithm to enhance the performance of variable selection methods <https://doi.org/10.1093/bioinformatics/btaa855>, F. Bertrand, I. Aouadi, N. Jung, R. Carapito, L. Vallat, S. Bahram, M. Maumy-Bertrand (2015), 


With the growth of big data, variable selection has become one of the major challenges in statistics. Although many methods have been proposed in the literature their performance in terms of recall and precision are limited in a context where the number of variables by far exceeds the number of observations or in a high correlated setting. 


Results: This package implements a new general algorithm which improves the precision of any existing variable selection method. This algorithm is based on highly intensive simulations and takes into account the correlation structure of the data. Our algorithm can either produce a confidence index for variable selection or it can be used in an experimental design planning perspective.


This website and these examples were created by F. Bertrand and M. Maumy-Bertrand.

## Installation

You can install the released version of SelectBoost from [CRAN](https://CRAN.R-project.org) with:


``` r
install.packages("SelectBoost")
```

You can install the development version of SelectBoost from [github](https://github.com) with:


``` r
devtools::install_github("fbertran/SelectBoost")
```

If you are a Linux/Unix or a Macos user, you can install a version of SelectBoost with support for `doMC` from [github](https://github.com) with:


``` r
devtools::install_github("fbertran/SelectBoost", ref = "doMC")
```


## Examples

### First example: Simulated dataset 

#### Simulating data
Create a correlation matrix for two groups of variable with an intragroup correlation value of $cor\_group$.

``` r
library(SelectBoost)
group<-c(rep(1:2,5))
cor_group<-c(.8,.4)
C<-simulation_cor(group,cor_group)
C
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#>  [1,]  1.0  0.0  0.8  0.0  0.8  0.0  0.8  0.0  0.8   0.0
#>  [2,]  0.0  1.0  0.0  0.4  0.0  0.4  0.0  0.4  0.0   0.4
#>  [3,]  0.8  0.0  1.0  0.0  0.8  0.0  0.8  0.0  0.8   0.0
#>  [4,]  0.0  0.4  0.0  1.0  0.0  0.4  0.0  0.4  0.0   0.4
#>  [5,]  0.8  0.0  0.8  0.0  1.0  0.0  0.8  0.0  0.8   0.0
#>  [6,]  0.0  0.4  0.0  0.4  0.0  1.0  0.0  0.4  0.0   0.4
#>  [7,]  0.8  0.0  0.8  0.0  0.8  0.0  1.0  0.0  0.8   0.0
#>  [8,]  0.0  0.4  0.0  0.4  0.0  0.4  0.0  1.0  0.0   0.4
#>  [9,]  0.8  0.0  0.8  0.0  0.8  0.0  0.8  0.0  1.0   0.0
#> [10,]  0.0  0.4  0.0  0.4  0.0  0.4  0.0  0.4  0.0   1.0
```

Simulate predictor dataset witn $N=100$ observations.

```r
N<-100
X<-simulation_X(N,C)
head(X)
#>            [,1]       [,2]       [,3]        [,4]       [,5]       [,6]
#> [1,] -1.5321046 -1.4697218 -1.0037681  0.27502077 -0.9495274 -2.0829749
#> [2,]  0.2144978 -1.0002400  0.5538293  0.09626492  0.2995111  0.1413112
#> [3,] -0.3969052  1.6329261  0.4687922  0.40198830 -0.3523466  1.6470825
#> [4,] -0.8909846  0.2583202 -0.5393219  0.46851301 -0.2908467  0.5266052
#> [5,] -0.8543071 -0.6790359 -0.2004707 -2.95957734  0.0948163 -1.8173478
#> [6,] -1.4011010 -0.8882664 -1.3439198  0.06230701 -0.9166235  1.2120350
#>             [,7]       [,8]        [,9]     [,10]
#> [1,] -1.50111614  1.1691130 -0.08175974 -1.561240
#> [2,] -0.60316385  0.1585346  0.46090450  0.293033
#> [3,] -0.33970295  0.2716709  0.47533410  1.656613
#> [4,] -0.05853370  1.7014325 -0.50885571  1.635436
#> [5,] -0.01191556 -1.6059544  0.13570387 -1.464627
#> [6,] -1.50579752  0.3249712 -1.07033488  0.117601
```

$supp$ set the predictors that will be used to simulate the response (=true predictors). $minB$ and $maxB$ set the minimum and maximum absolute value for a $\beta$ coefficient used in the model for the (true) predictors. $stn$ is a scaling factor for the noise in the response.

```r
supp<-c(1,1,1,0,0,0,0,0,0,0)
minB<-1
maxB<-2
stn<-500
DATA_exemple<-simulation_DATA(X,supp,minB,maxB,stn)
str(DATA_exemple)
#> List of 6
#>  $ X      : num [1:100, 1:10] -1.532 0.214 -0.397 -0.891 -0.854 ...
#>  $ Y      : num [1:100] -3.535 -2.042 1.768 -0.181 -2.121 ...
#>  $ support: num [1:10] 1 1 1 0 0 0 0 0 0 0
#>  $ beta   : num [1:10] 1.37 1.73 -1.09 0 0 ...
#>  $ stn    : num 500
#>  $ sigma  : num [1, 1] 0.0795
#>  - attr(*, "class")= chr "simuls"
```

#### Selectboost analysis

By default `fastboost` performs $B=100$ resamplings of the model. As a result, we get a matrix with the proportions of selection of each variable at a given resampling level $c_0$. The resampling are designed to take into account the correlation structure of the predictors. The correlation used by default is the Pearson Correlation but any can be passed through the `corrfunc` argument. The $c_0$ value sets the minimum level for which correlations between two predictors are kept in the resampling process. The correlation structure is used to group the variables. Two groups functions `group_func_1`, grouping by thresholding the correlation matrix, and `group_func_2`, grouping using community selection, are available but any can be provided using the `group` argument of the function. The `func` argument is the variable selection function that should be used to assess variable memberships. It  defaults to `lasso_msgps_AICc` but many others, for instance for lasso, elastinet, logistic glmnet and network inference with the [Cascade package](https://fbertran.github.io/Cascade/), are provided:

* lasso_cv_glmnet_bin_min(X, Y)
* lasso_cv_glmnet_bin_1se(X, Y)
* lasso_glmnet_bin_AICc(X, Y)
* lasso_glmnet_bin_BIC(X, Y)
* lasso_cv_lars_min(X, Y)
* lasso_cv_lars_1se(X, Y)
* lasso_cv_glmnet_min(X, Y)
* lasso_cv_glmnet_min_weighted(X, Y, priors)
* lasso_cv_glmnet_1se(X, Y)
* lasso_cv_glmnet_1se_weighted(X, Y, priors)
* lasso_msgps_Cp(X, Y, penalty = "enet")
* lasso_msgps_AICc(X, Y, penalty = "enet")
* lasso_msgps_GCV(X, Y, penalty = "enet")
* lasso_msgps_BIC(X, Y, penalty = "enet")
* enetf_msgps_Cp(X, Y, penalty = "enet", alpha = 0.5)
* enetf_msgps_AICc(X, Y, penalty = "enet", alpha = 0.5)
* enetf_msgps_GCV(X, Y, penalty = "enet", alpha = 0.5)
* enetf_msgps_BIC(X, Y, penalty = "enet", alpha = 0.5)
* lasso_cascade(M, Y, K, eps = 10^-5, cv.fun)

User defined functions can alse be specified in the `func` argument. See the vignette for an example of use with *adaptative* lasso.

Default steps for $c_0$

```r
quantile(abs(cor(DATA_exemple$X))[abs(cor(DATA_exemple$X))!=1],(0:10)/10)
#>           0%          10%          20%          30%          40%          50% 
#> 4.141445e-05 1.077611e-02 3.821167e-02 5.018753e-02 7.621041e-02 1.289050e-01 
#>          60%          70%          80%          90%         100% 
#> 2.981748e-01 4.616924e-01 7.858471e-01 8.150396e-01 8.363768e-01
```


```r
result.boost.raw = fastboost(DATA_exemple$X, DATA_exemple$Y)
result.boost.raw
#>               1    2    3    4    5    6    7    8    9   10
#> c0 = 1     1.00 1.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> c0 = 0.836 1.00 1.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> c0 = 0.811 1.00 1.00 1.00 0.46 0.99 0.89 0.43 1.00 0.43 1.00
#> c0 = 0.786 0.38 1.00 0.38 0.03 0.90 0.46 0.33 1.00 0.34 1.00
#> c0 = 0.449 0.47 1.00 0.46 0.36 0.37 0.91 0.46 1.00 0.38 1.00
#> c0 = 0.298 0.46 0.87 0.48 0.85 0.43 0.94 0.46 0.99 0.50 0.95
#> c0 = 0.129 0.41 0.95 0.41 0.94 0.38 0.91 0.49 0.94 0.35 0.91
#> c0 = 0.076 0.38 0.88 0.49 0.91 0.37 0.93 0.37 0.94 0.39 0.96
#> c0 = 0.051 0.45 0.94 0.37 0.89 0.34 0.95 0.38 0.94 0.51 0.91
#> c0 = 0.038 0.40 0.92 0.48 0.89 0.46 0.91 0.40 0.90 0.41 0.94
#> c0 = 0.013 0.42 0.71 0.85 0.95 0.34 0.98 0.32 0.97 0.31 0.95
#> c0 = 0     0.39 0.69 0.83 0.95 0.36 0.98 0.40 0.95 0.33 0.82
#> c0 = 0     0.33 0.38 0.40 0.35 0.35 0.26 0.44 0.37 0.31 0.33
#> attr(,"c0.seq")
#>              100%      90%      80%      70%      60%      50%      40%      30% 
#> 1.000000 0.836377 0.811485 0.785847 0.449464 0.298175 0.128905 0.076210 0.050791 
#>      20%      10%       0%          
#> 0.038212 0.012626 0.000041 0.000000 
#> attr(,"c0lim")
#> [1] TRUE
#> attr(,"steps.seq")
#>  [1] 1.0 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0.0
#> attr(,"typeboost")
#> [1] "fastboost"
#> attr(,"limi_alea")
#> [1] NA
#> attr(,"B")
#> [1] 100
#> attr(,"class")
#> [1] "selectboost"
```

Applying a non increasing post-processing step to the results improves the performance of the algorithm. 

```r
result.boost = force.non.inc(result.boost.raw)
result.boost
#>               1    2    3 4 5 6 7 8 9 10
#>            1.00 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.836 1.00 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.811 1.00 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.786 0.38 1.00 0.38 0 0 0 0 0 0  0
#> c0 = 0.449 0.38 1.00 0.38 0 0 0 0 0 0  0
#> c0 = 0.298 0.37 0.87 0.38 0 0 0 0 0 0  0
#> c0 = 0.129 0.32 0.87 0.31 0 0 0 0 0 0  0
#> c0 = 0.076 0.29 0.80 0.31 0 0 0 0 0 0  0
#> c0 = 0.051 0.29 0.80 0.19 0 0 0 0 0 0  0
#> c0 = 0.038 0.24 0.78 0.19 0 0 0 0 0 0  0
#> c0 = 0.013 0.24 0.57 0.19 0 0 0 0 0 0  0
#> c0 = 0     0.21 0.55 0.17 0 0 0 0 0 0  0
#> c0 = 0     0.15 0.24 0.00 0 0 0 0 0 0  0
#> attr(,"c0.seq")
#>              100%      90%      80%      70%      60%      50%      40%      30% 
#> 1.000000 0.836377 0.811485 0.785847 0.449464 0.298175 0.128905 0.076210 0.050791 
#>      20%      10%       0%          
#> 0.038212 0.012626 0.000041 0.000000 
#> attr(,"c0lim")
#> [1] TRUE
#> attr(,"steps.seq")
#>  [1] 1.0 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0.0
#> attr(,"typeboost")
#> [1] "fastboost"
#> attr(,"limi_alea")
#> [1] NA
#> attr(,"B")
#> [1] 100
#> attr(,"class")
#> [1] "fastboost"
```

#### Comparing true and selected predictors

We can compute, for all the $c_0$ values and for a selection threshold varying from $1$ to $0.5$ by $0.05$ steps, the recall (sensitivity), the precision (positive predictive value), as well as several Fscores ($F_1$ harmonic mean of recall and precision, $F_{1/2}$ and $F_2$ two weighted harmonic means of recall and precision).

```r
All_res=NULL
#Here are the cutoff level tested
for(lev in 20:10/20){
F_score=NULL
for(u in 1:nrow(result.boost)){
	F_score<-rbind(F_score,SelectBoost::compsim(DATA_exemple,result.boost[u,],
	                                            level=lev)[1:5])
}
All_res <- abind::abind(All_res,F_score,along=3)
}
```

For a selection threshold equal to $0.90$, all the $c_0$ values and the 5 criteria.

```r
matplot(1:nrow(result.boost),All_res[,,3],type="l",ylab="criterion value",
        xlab="c0 value",xaxt="n",lwd=2)
axis(1, at=1:length(attr(result.boost,"c0.seq")),  
     labels=round(attr(result.boost,"c0.seq"),3))
legend(x="topright",legend=c("recall (sensitivity)",
      "precision (positive predictive value)","non-weighted Fscore",
      "F1/2 weighted Fscore","F2 weighted Fscore"),lty=1:5,col=1:5,lwd=2)
```

<img src="man/figures/README-datasetsimulation6-1.png" title="plot of chunk datasetsimulation6" alt="plot of chunk datasetsimulation6" width="100%" />

Fscores for all selection thresholds and all the $c_0$ values.

```r
matplot(1:nrow(result.boost),All_res[,3,],type="l",ylab="Fscore",
        xlab="c0 value",xaxt="n",lwd=2,col=1:11,lty=1:11)
axis(1, at=1:length(attr(result.boost,"c0.seq")),
     labels=round(attr(result.boost,"c0.seq"),3))
legend(x="topright",legend=(20:11)/20,lty=1:11,col=1:11,lwd=2,
       title="Threshold")
```

<img src="man/figures/README-datasetsimulation7-1.png" title="plot of chunk datasetsimulation7" alt="plot of chunk datasetsimulation7" width="100%" />

#### Complete Selectboost analysis

What is the maximum number of steps ?

```r
all.cors=unique(abs(cor(DATA_exemple$X))[abs(cor(DATA_exemple$X))!=1])
length(all.cors)
#> [1] 45
```

With such datasets, we can perform all the 45 steps for the Selectboost analysis. We switch to  community analysis from the [igraph package](https://igraph.org) as the grouping variable function.

```r
groups.seq.f2=lapply(sort(unique(c(1,all.cors,0)),decreasing=TRUE), function(c0)
  if(c0!=1){lapply(group_func_2(cor(DATA_exemple$X),c0)$communities,sort)}
  else {lapply(group_func_2(cor(DATA_exemple$X),c0),sort)})
names(groups.seq.f2)<-sort(unique(c(1,all.cors,0)),decreasing=TRUE)
groups.seq.f2[[1]]
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
#> [[4]]
#> [1] 4
#> 
#> [[5]]
#> [1] 5
#> 
#> [[6]]
#> [1] 6
#> 
#> [[7]]
#> [1] 7
#> 
#> [[8]]
#> [1] 8
#> 
#> [[9]]
#> [1] 9
#> 
#> [[10]]
#> [1] 10
```


```r
result.boost.45.raw = fastboost(DATA_exemple$X, DATA_exemple$Y, B=100,
                    steps.seq=sort(unique(all.cors),decreasing=TRUE))
result.boost.45.raw
#>               1    2    3    4    5    6    7    8    9   10
#> c0 = 1     1.00 1.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> c0 = 0.836 1.00 1.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> c0 = 0.829 1.00 1.00 1.00 0.76 1.00 0.55 0.17 0.96 1.00 1.00
#> c0 = 0.829 0.95 1.00 1.00 0.46 1.00 0.74 0.77 1.00 0.52 0.99
#> c0 = 0.819 0.99 1.00 1.00 0.49 0.99 0.89 0.59 1.00 0.68 0.99
#> c0 = 0.815 1.00 1.00 1.00 0.34 1.00 0.88 0.59 1.00 0.64 0.99
#> c0 = 0.806 1.00 1.00 1.00 0.33 0.99 0.91 0.41 1.00 0.46 0.99
#> c0 = 0.796 1.00 1.00 1.00 0.38 0.93 0.85 0.47 1.00 0.54 1.00
#> c0 = 0.791 0.97 1.00 0.99 0.22 0.94 0.86 0.51 1.00 0.44 1.00
#> c0 = 0.79  0.93 1.00 1.00 0.24 0.96 0.89 0.46 1.00 0.55 1.00
#> c0 = 0.785 0.38 1.00 0.46 0.01 0.43 0.16 0.48 1.00 0.51 1.00
#> c0 = 0.543 0.49 1.00 0.45 0.01 0.39 0.31 0.43 1.00 0.56 1.00
#> c0 = 0.487 0.46 1.00 0.38 0.00 0.38 0.36 0.47 1.00 0.47 0.35
#> c0 = 0.475 0.45 1.00 0.43 0.47 0.46 0.34 0.48 1.00 0.35 1.00
#> c0 = 0.462 0.40 1.00 0.42 0.40 0.39 0.92 0.42 1.00 0.41 0.99
#> c0 = 0.401 0.45 1.00 0.39 0.41 0.40 0.99 0.38 1.00 0.44 0.98
#> c0 = 0.359 0.51 0.91 0.40 0.99 0.30 0.82 0.38 0.99 0.45 0.99
#> c0 = 0.31  0.42 0.96 0.46 0.95 0.40 0.97 0.39 0.99 0.43 0.96
#> c0 = 0.304 0.48 0.90 0.53 0.84 0.51 0.96 0.46 1.00 0.45 0.95
#> c0 = 0.294 0.45 0.90 0.42 0.90 0.49 0.95 0.44 0.99 0.42 0.95
#> c0 = 0.284 0.50 0.92 0.44 0.95 0.33 0.89 0.34 0.91 0.37 0.89
#> c0 = 0.131 0.39 0.95 0.42 0.94 0.49 0.90 0.38 0.90 0.37 0.89
#> c0 = 0.13  0.48 0.94 0.38 0.92 0.43 0.89 0.40 0.95 0.35 0.94
#> c0 = 0.129 0.45 0.94 0.47 0.93 0.41 0.95 0.51 0.94 0.43 0.90
#> c0 = 0.114 0.38 0.91 0.47 0.92 0.39 0.93 0.43 0.90 0.45 0.92
#> c0 = 0.111 0.40 0.86 0.44 0.91 0.42 0.93 0.39 0.94 0.39 0.92
#> c0 = 0.096 0.46 0.97 0.44 0.91 0.41 0.92 0.37 0.95 0.40 0.90
#> c0 = 0.083 0.48 0.89 0.39 0.92 0.42 0.88 0.38 0.95 0.51 0.94
#> c0 = 0.065 0.49 0.89 0.39 0.96 0.46 0.88 0.44 0.93 0.41 0.92
#> c0 = 0.063 0.41 0.89 0.38 0.93 0.40 0.94 0.46 0.92 0.34 0.95
#> c0 = 0.056 0.44 0.94 0.38 0.93 0.43 0.87 0.47 0.91 0.30 0.92
#> c0 = 0.053 0.45 0.91 0.54 0.92 0.41 0.91 0.32 0.91 0.41 0.93
#> c0 = 0.05  0.38 0.96 0.46 0.94 0.48 0.93 0.34 0.89 0.40 0.95
#> c0 = 0.046 0.40 0.96 0.43 0.95 0.44 0.93 0.40 0.89 0.52 0.93
#> c0 = 0.044 0.35 0.91 0.39 0.95 0.41 0.93 0.45 0.90 0.40 0.91
#> c0 = 0.043 0.38 0.93 0.39 0.94 0.41 0.92 0.41 0.93 0.42 0.97
#> c0 = 0.04  0.39 0.90 0.46 0.94 0.45 0.90 0.42 0.96 0.44 0.90
#> c0 = 0.03  0.35 0.95 0.42 0.89 0.41 0.93 0.42 0.92 0.41 0.94
#> c0 = 0.02  0.32 0.78 0.92 0.96 0.31 0.97 0.30 0.87 0.23 1.00
#> c0 = 0.018 0.31 0.85 0.87 0.98 0.35 0.94 0.31 0.95 0.32 0.97
#> c0 = 0.015 0.39 0.76 0.87 0.92 0.30 0.94 0.29 0.95 0.31 0.98
#> c0 = 0.011 0.24 0.86 0.87 0.93 0.19 0.99 0.41 0.96 0.28 0.85
#> c0 = 0.009 0.48 0.74 0.84 0.97 0.30 0.99 0.47 0.92 0.34 0.81
#> c0 = 0.008 0.43 0.83 0.82 0.99 0.26 0.94 0.34 0.95 0.27 0.85
#> c0 = 0.008 0.36 0.83 0.81 1.00 0.20 0.98 0.37 0.97 0.28 0.86
#> c0 = 0     0.50 0.85 0.89 0.99 0.31 0.98 0.38 0.93 0.28 0.92
#> c0 = 0     0.34 0.36 0.39 0.35 0.31 0.35 0.51 0.25 0.33 0.35
#> attr(,"c0.seq")
#>  [1] 1.000000 0.836377 0.829162 0.828827 0.819400 0.815040 0.806154 0.796102
#>  [9] 0.790728 0.790366 0.784717 0.543037 0.487104 0.474899 0.461692 0.400552
#> [17] 0.358610 0.309558 0.304284 0.294102 0.284316 0.130908 0.129817 0.128905
#> [25] 0.113625 0.110504 0.095538 0.083468 0.065324 0.063235 0.056491 0.053206
#> [33] 0.050188 0.045647 0.044071 0.043374 0.040275 0.029958 0.020424 0.017876
#> [41] 0.015400 0.010776 0.009051 0.007821 0.007701 0.000041 0.000000
#> attr(,"c0lim")
#> [1] TRUE
#> attr(,"steps.seq")
#>  [1] 0.000000e+00 8.363768e-01 8.291616e-01 8.288270e-01 8.194005e-01
#>  [6] 8.150396e-01 8.061535e-01 7.961019e-01 7.907282e-01 7.903658e-01
#> [11] 7.847174e-01 5.430366e-01 4.871037e-01 4.748994e-01 4.616924e-01
#> [16] 4.005519e-01 3.586096e-01 3.095578e-01 3.042840e-01 2.941020e-01
#> [21] 2.843163e-01 1.309077e-01 1.298174e-01 1.289050e-01 1.136253e-01
#> [26] 1.105037e-01 9.553777e-02 8.346778e-02 6.532436e-02 6.323546e-02
#> [31] 5.649128e-02 5.320573e-02 5.018753e-02 4.564702e-02 4.407068e-02
#> [36] 4.337394e-02 4.027507e-02 2.995808e-02 2.042441e-02 1.787624e-02
#> [41] 1.539992e-02 1.077611e-02 9.050604e-03 7.820714e-03 7.700949e-03
#> [46] 4.141445e-05 1.000000e+00
#> attr(,"typeboost")
#> [1] "fastboost"
#> attr(,"limi_alea")
#> [1] NA
#> attr(,"B")
#> [1] 100
#> attr(,"class")
#> [1] "selectboost"
```

Applying a non increasing post-processing step to the results improves the performance of the algorithm. 

```r
result.boost.45 = force.non.inc(result.boost.45.raw)
result.boost.45
#>               1    2    3 4 5 6 7 8 9 10
#>            1.00 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.836 1.00 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.829 1.00 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.829 0.95 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.819 0.95 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.815 0.95 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.806 0.95 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.796 0.95 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.791 0.92 1.00 0.99 0 0 0 0 0 0  0
#> c0 = 0.79  0.88 1.00 0.99 0 0 0 0 0 0  0
#> c0 = 0.785 0.33 1.00 0.45 0 0 0 0 0 0  0
#> c0 = 0.543 0.33 1.00 0.44 0 0 0 0 0 0  0
#> c0 = 0.487 0.30 1.00 0.37 0 0 0 0 0 0  0
#> c0 = 0.475 0.29 1.00 0.37 0 0 0 0 0 0  0
#> c0 = 0.462 0.24 1.00 0.36 0 0 0 0 0 0  0
#> c0 = 0.401 0.24 1.00 0.33 0 0 0 0 0 0  0
#> c0 = 0.359 0.24 0.91 0.33 0 0 0 0 0 0  0
#> c0 = 0.31  0.15 0.91 0.33 0 0 0 0 0 0  0
#> c0 = 0.304 0.15 0.85 0.33 0 0 0 0 0 0  0
#> c0 = 0.294 0.12 0.85 0.22 0 0 0 0 0 0  0
#> c0 = 0.284 0.12 0.85 0.22 0 0 0 0 0 0  0
#> c0 = 0.131 0.01 0.85 0.20 0 0 0 0 0 0  0
#> c0 = 0.13  0.01 0.84 0.16 0 0 0 0 0 0  0
#> c0 = 0.129 0.00 0.84 0.16 0 0 0 0 0 0  0
#> c0 = 0.114 0.00 0.81 0.16 0 0 0 0 0 0  0
#> c0 = 0.111 0.00 0.76 0.13 0 0 0 0 0 0  0
#> c0 = 0.096 0.00 0.76 0.13 0 0 0 0 0 0  0
#> c0 = 0.083 0.00 0.68 0.08 0 0 0 0 0 0  0
#> c0 = 0.065 0.00 0.68 0.08 0 0 0 0 0 0  0
#> c0 = 0.063 0.00 0.68 0.07 0 0 0 0 0 0  0
#> c0 = 0.056 0.00 0.68 0.07 0 0 0 0 0 0  0
#> c0 = 0.053 0.00 0.65 0.07 0 0 0 0 0 0  0
#> c0 = 0.05  0.00 0.65 0.00 0 0 0 0 0 0  0
#> c0 = 0.046 0.00 0.65 0.00 0 0 0 0 0 0  0
#> c0 = 0.044 0.00 0.60 0.00 0 0 0 0 0 0  0
#> c0 = 0.043 0.00 0.60 0.00 0 0 0 0 0 0  0
#> c0 = 0.04  0.00 0.57 0.00 0 0 0 0 0 0  0
#> c0 = 0.03  0.00 0.57 0.00 0 0 0 0 0 0  0
#> c0 = 0.02  0.00 0.40 0.00 0 0 0 0 0 0  0
#> c0 = 0.018 0.00 0.40 0.00 0 0 0 0 0 0  0
#> c0 = 0.015 0.00 0.31 0.00 0 0 0 0 0 0  0
#> c0 = 0.011 0.00 0.31 0.00 0 0 0 0 0 0  0
#> c0 = 0.009 0.00 0.19 0.00 0 0 0 0 0 0  0
#> c0 = 0.008 0.00 0.19 0.00 0 0 0 0 0 0  0
#> c0 = 0.008 0.00 0.19 0.00 0 0 0 0 0 0  0
#> c0 = 0     0.00 0.19 0.00 0 0 0 0 0 0  0
#> c0 = 0     0.00 0.00 0.00 0 0 0 0 0 0  0
#> attr(,"c0.seq")
#>  [1] 1.000000 0.836377 0.829162 0.828827 0.819400 0.815040 0.806154 0.796102
#>  [9] 0.790728 0.790366 0.784717 0.543037 0.487104 0.474899 0.461692 0.400552
#> [17] 0.358610 0.309558 0.304284 0.294102 0.284316 0.130908 0.129817 0.128905
#> [25] 0.113625 0.110504 0.095538 0.083468 0.065324 0.063235 0.056491 0.053206
#> [33] 0.050188 0.045647 0.044071 0.043374 0.040275 0.029958 0.020424 0.017876
#> [41] 0.015400 0.010776 0.009051 0.007821 0.007701 0.000041 0.000000
#> attr(,"c0lim")
#> [1] TRUE
#> attr(,"steps.seq")
#>  [1] 0.000000e+00 8.363768e-01 8.291616e-01 8.288270e-01 8.194005e-01
#>  [6] 8.150396e-01 8.061535e-01 7.961019e-01 7.907282e-01 7.903658e-01
#> [11] 7.847174e-01 5.430366e-01 4.871037e-01 4.748994e-01 4.616924e-01
#> [16] 4.005519e-01 3.586096e-01 3.095578e-01 3.042840e-01 2.941020e-01
#> [21] 2.843163e-01 1.309077e-01 1.298174e-01 1.289050e-01 1.136253e-01
#> [26] 1.105037e-01 9.553777e-02 8.346778e-02 6.532436e-02 6.323546e-02
#> [31] 5.649128e-02 5.320573e-02 5.018753e-02 4.564702e-02 4.407068e-02
#> [36] 4.337394e-02 4.027507e-02 2.995808e-02 2.042441e-02 1.787624e-02
#> [41] 1.539992e-02 1.077611e-02 9.050604e-03 7.820714e-03 7.700949e-03
#> [46] 4.141445e-05 1.000000e+00
#> attr(,"typeboost")
#> [1] "fastboost"
#> attr(,"limi_alea")
#> [1] NA
#> attr(,"B")
#> [1] 100
#> attr(,"class")
#> [1] "fastboost"
```

#### Comparing true and selected predictors
Due to the effect of the correlated resampling, the proportion of selection for a variable may increase, especially if it is a variable that is often discarded. Hence, one should force those proportions of selection to be non-increasing. It is one of the results of the $summary$ function for the $selectboost$ class.


```r
dec.result.boost.45 <- summary(result.boost.45)$selectboost_result.dec
#> Error in summary(result.boost.45)$selectboost_result.dec: $ operator is invalid for atomic vectors
dec.result.boost.45
#> Error in eval(expr, envir, enclos): objet 'dec.result.boost.45' introuvable
```

Let's compute again, for all the $c_0$ values, the recall (sensitivity), precision (positive predictive value), and several Fscores ($F_1$ harmonic mean of recall and precision, $F_{1/2}$ and $F_2$ two weighted harmonic means of recall and precision).

```r
All_res.45=NULL
#Here are the cutoff level tested
for(lev.45 in 20:10/20){
F_score.45=NULL
for(u.45 in 1:nrow(dec.result.boost.45
)){
	F_score.45<-rbind(F_score.45,SelectBoost::compsim(DATA_exemple,
	           dec.result.boost.45[u.45,],level=lev.45)[1:5])
}
All_res.45 <- abind::abind(All_res.45,F_score.45,along=3)
}
#> Error in nrow(dec.result.boost.45): objet 'dec.result.boost.45' introuvable
```

For a selection threshold equal to $0.90$, all the $c_0$ values and the 5 criteria.

```r
matplot(1:nrow(dec.result.boost.45),All_res.45[,,3],type="l",
        ylab="criterion value",xlab="c0 value",xaxt="n",lwd=2)
#> Error in nrow(dec.result.boost.45): objet 'dec.result.boost.45' introuvable
axis(1, at=1:length(attr(result.boost.45,"c0.seq")), 
     labels=round(attr(result.boost.45,"c0.seq"),3))
#> Error in axis(1, at = 1:length(attr(result.boost.45, "c0.seq")), labels = round(attr(result.boost.45, : plot.new has not been called yet
legend(x="topright",legend=c("recall (sensitivity)",
       "precision (positive predictive value)","non-weighted Fscore",
       "F1/2 weighted Fscore","F2 weighted Fscore"),
       lty=1:5,col=1:5,lwd=2)
#> Error in (function (s, units = "user", cex = NULL, font = NULL, vfont = NULL, : plot.new has not been called yet
```

Fscores for all selection thresholds and all the $c_0$ values.

```r
matplot(1:nrow(dec.result.boost.45),All_res.45[,3,],type="l",
        ylab="Fscore",xlab="c0 value",xaxt="n",lwd=2,col=1:11,lty=1:11)
#> Error in nrow(dec.result.boost.45): objet 'dec.result.boost.45' introuvable
axis(1, at=1:length(attr(result.boost.45,"c0.seq")), 
     labels=round(attr(result.boost.45,"c0.seq"),3))
#> Error in axis(1, at = 1:length(attr(result.boost.45, "c0.seq")), labels = round(attr(result.boost.45, : plot.new has not been called yet
legend(x="topright",legend=(20:11)/20,lty=1:11,col=1:11,lwd=2,
       title="Threshold")
#> Error in (function (s, units = "user", cex = NULL, font = NULL, vfont = NULL, : plot.new has not been called yet
```

#### Confidence indices.

First compute the highest $c_0$ value for which the proportion of selection is under the threshold $thr$. In that analysis, we set $thr=1$.

```r
thr=1
index.last.c0=apply(dec.result.boost.45>=thr,2,which.min)-1
#> Error in apply(dec.result.boost.45 >= thr, 2, which.min): objet 'dec.result.boost.45' introuvable
index.last.c0
#> Error in eval(expr, envir, enclos): objet 'index.last.c0' introuvable
```

Define some colorRamp ranging from blue (high confidence) to red (low confidence).

```r
jet.colors <-
  colorRamp(rev(c(
  "blue", "#007FFF", "#FF7F00", "red", "#7F0000")))
```


```r
rownames(dec.result.boost.45)[index.last.c0]
#> Error in rownames(dec.result.boost.45): objet 'dec.result.boost.45' introuvable
attr(result.boost.45,"c0.seq")[index.last.c0]
#> Error in eval(expr, envir, enclos): objet 'index.last.c0' introuvable
confidence.indices = c(0,1-attr(result.boost.45,"c0.seq"))[index.last.c0+1]
#> Error in eval(expr, envir, enclos): objet 'index.last.c0' introuvable
confidence.indices
#> Error in eval(expr, envir, enclos): objet 'confidence.indices' introuvable
barplot(confidence.indices,col=rgb(jet.colors(confidence.indices), maxColorValue = 255), 
        names.arg=colnames(result.boost.45), ylim=c(0,1))
#> Error in barplot(confidence.indices, col = rgb(jet.colors(confidence.indices), : objet 'confidence.indices' introuvable
```

First compute the highest $c_0$ value for which the proportion of selection is under the threshold $thr$. In that analysis, we set $thr=1$.

```r
thr=.9
index.last.c0=apply(dec.result.boost.45>=thr,2,which.min)-1
#> Error in apply(dec.result.boost.45 >= thr, 2, which.min): objet 'dec.result.boost.45' introuvable
index.last.c0
#> Error in eval(expr, envir, enclos): objet 'index.last.c0' introuvable
```


```r
rownames(dec.result.boost.45)[index.last.c0]
#> Error in rownames(dec.result.boost.45): objet 'dec.result.boost.45' introuvable
attr(result.boost.45,"c0.seq")[index.last.c0]
#> Error in eval(expr, envir, enclos): objet 'index.last.c0' introuvable
confidence.indices = c(0,1-attr(result.boost.45,"c0.seq"))[index.last.c0+1]
#> Error in eval(expr, envir, enclos): objet 'index.last.c0' introuvable
confidence.indices
#> Error in eval(expr, envir, enclos): objet 'confidence.indices' introuvable
barplot(confidence.indices,col=rgb(jet.colors(confidence.indices), maxColorValue = 255), 
        names.arg=colnames(result.boost.45), ylim=c(0,1))
#> Error in barplot(confidence.indices, col = rgb(jet.colors(confidence.indices), : objet 'confidence.indices' introuvable
```


### Second example: biological network data
#### Simulating data using real data

The loop should be used to generate at least 100 datasets and then average the results.

```r
require(CascadeData)
data(micro_S)
data(micro_US)
micro_US<-Cascade::as.micro_array(micro_US,c(60,90,240,390),6)
micro_S<-Cascade::as.micro_array(micro_S,c(60,90,240,390),6)
S<-Cascade::geneSelection(list(micro_S,micro_US),list("condition",c(1,2),1),-1)
rm(micro_S);data(micro_S)
Sel<-micro_S[S@name,]

supp<-c(1,1,1,1,1,rep(0,95))
minB<-1
maxB<-2
stn<-5

set.seed(3141)
for(i in 1:1){
X<-t(as.matrix(Sel[sample(1:1300 ,100),]))
Xnorm<-t(t(X)/sqrt(diag(t(X)%*%X)))
assign(paste("DATA_exemple3_nb_",i,sep=""),simulation_DATA(Xnorm,supp,minB,maxB,stn))
}
```


```r
all.cors.micro=unique(abs(cor(DATA_exemple3_nb_1$X))[abs(cor(
  DATA_exemple3_nb_1$X))!=1])
length(unique(all.cors.micro))
#> [1] 4950
quantile(all.cors.micro,.90)
#>       90% 
#> 0.6938712
```


```r
top10p.all.cors.micro=all.cors.micro[all.cors.micro>=quantile(all.cors.micro,.90)]
c0seq.top10p.all.cors.micro=quantile(top10p.all.cors.micro,rev(
  seq(0,length(top10p.all.cors.micro),length.out = 50)/495))
c0seq.top10p.all.cors.micro
#>      100% 97.95918% 95.91837% 93.87755% 91.83673% 89.79592%  87.7551% 85.71429% 
#> 0.9486685 0.9184348 0.8993626 0.8867508 0.8783368 0.8688498 0.8597920 0.8517712 
#> 83.67347% 81.63265% 79.59184% 77.55102%  75.5102% 73.46939% 71.42857% 69.38776% 
#> 0.8441046 0.8370590 0.8315722 0.8248607 0.8193079 0.8124198 0.8084936 0.8038357 
#> 67.34694% 65.30612% 63.26531% 61.22449% 59.18367% 57.14286% 55.10204% 53.06122% 
#> 0.7967669 0.7920303 0.7885399 0.7842243 0.7803654 0.7783504 0.7750129 0.7711674 
#> 51.02041% 48.97959% 46.93878% 44.89796% 42.85714% 40.81633% 38.77551% 36.73469% 
#> 0.7687731 0.7663441 0.7606838 0.7577961 0.7553123 0.7524554 0.7493711 0.7456580 
#> 34.69388% 32.65306% 30.61224% 28.57143% 26.53061%  24.4898% 22.44898% 20.40816% 
#> 0.7431055 0.7403313 0.7377508 0.7345842 0.7313349 0.7296512 0.7264820 0.7246836 
#> 18.36735% 16.32653% 14.28571%  12.2449% 10.20408% 8.163265% 6.122449% 4.081633% 
#> 0.7229066 0.7198827 0.7158667 0.7122053 0.7076771 0.7044341 0.7009353 0.6991250 
#> 2.040816%        0% 
#> 0.6955766 0.6939670
```


```r
result.boost.micro_nb1 = fastboost(DATA_exemple3_nb_1$X, DATA_exemple3_nb_1$Y, B=100, 
                                   steps.seq=c0seq.top10p.all.cors.micro)
result.boost.micro_nb1
#>               1    2    3    4    5    6    7    8    9   10   11   12   13   14
#> c0 = 1     1.00 1.00 1.00 1.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 1.00 0.00 0.00
#> c0 = 0.949 1.00 1.00 1.00 1.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 1.00 0.00 0.00
#> c0 = 0.918 1.00 1.00 1.00 0.91 1.00 0.03 0.03 0.05 0.00 0.00 0.01 0.69 0.43 0.01
#> c0 = 0.899 1.00 1.00 1.00 0.92 1.00 0.01 0.00 0.06 0.07 0.00 0.00 0.57 0.24 0.01
#> c0 = 0.887 1.00 1.00 0.64 0.80 1.00 0.07 0.01 0.16 0.10 0.14 0.09 0.76 0.30 0.03
#> c0 = 0.878 1.00 1.00 0.55 0.80 1.00 0.05 0.02 0.11 0.11 0.11 0.10 0.62 0.33 0.02
#> c0 = 0.869 1.00 1.00 0.51 0.69 1.00 0.09 0.03 0.08 0.12 0.12 0.48 0.54 0.34 0.01
#> c0 = 0.86  1.00 1.00 0.59 0.69 1.00 0.12 0.06 0.12 0.13 0.19 0.54 0.76 0.37 0.30
#> c0 = 0.852 1.00 1.00 0.57 0.75 1.00 0.14 0.05 0.15 0.12 0.20 0.42 0.77 0.36 0.32
#> c0 = 0.844 1.00 1.00 0.61 0.76 1.00 0.08 0.03 0.11 0.18 0.23 0.39 0.72 0.38 0.34
#>              15   16   17   18   19   20   21   22   23   24   25   26   27   28
#> c0 = 1     1.00 0.00 0.00 1.00 1.00 0.00 0.00 0.00 0.00 0.00 1.00 0.00 1.00 0.00
#> c0 = 0.949 1.00 0.00 0.00 1.00 1.00 0.00 0.00 0.00 0.00 0.00 1.00 0.00 1.00 0.00
#> c0 = 0.918 0.86 0.07 0.07 0.84 1.00 0.24 0.00 0.20 0.02 0.02 0.88 0.11 0.05 0.38
#> c0 = 0.899 0.91 0.03 0.06 0.96 1.00 0.39 0.10 0.19 0.01 0.02 0.88 0.07 0.05 0.19
#> c0 = 0.887 0.81 0.04 0.11 0.96 1.00 0.37 0.10 0.27 0.05 0.00 0.87 0.09 0.01 0.39
#> c0 = 0.878 0.82 0.07 0.08 0.96 1.00 0.27 0.17 0.26 0.17 0.03 0.86 0.18 0.06 0.44
#> c0 = 0.869 0.75 0.05 0.09 0.95 0.98 0.29 0.14 0.17 0.17 0.03 0.83 0.11 0.08 0.49
#> c0 = 0.86  0.91 0.09 0.12 0.91 1.00 0.30 0.14 0.34 0.20 0.08 0.78 0.22 0.12 0.39
#> c0 = 0.852 0.97 0.14 0.22 0.87 0.99 0.16 0.18 0.21 0.22 0.05 0.76 0.18 0.10 0.38
#> c0 = 0.844 0.93 0.19 0.12 0.89 1.00 0.18 0.21 0.30 0.19 0.10 0.67 0.23 0.16 0.45
#>              29   30   31   32   33   34   35   36   37   38   39   40   41   42
#> c0 = 1     1.00 0.00 1.00 0.00 0.00 1.00 1.00 1.00 0.00 0.00 1.00 0.00 0.00 0.00
#> c0 = 0.949 1.00 0.00 1.00 0.00 0.00 1.00 1.00 1.00 0.00 0.00 1.00 0.00 0.00 0.00
#> c0 = 0.918 1.00 0.05 0.84 0.00 0.37 1.00 1.00 0.99 0.00 0.00 0.89 0.00 0.00 0.00
#> c0 = 0.899 1.00 0.02 0.93 0.00 0.37 1.00 1.00 0.99 0.00 0.00 0.90 0.00 0.00 0.00
#> c0 = 0.887 1.00 0.01 0.87 0.52 0.63 1.00 1.00 0.97 0.02 0.02 0.80 0.09 0.01 0.00
#> c0 = 0.878 0.99 0.03 0.81 0.52 0.58 1.00 1.00 0.98 0.02 0.00 0.84 0.09 0.00 0.00
#> c0 = 0.869 0.99 0.02 0.86 0.46 0.52 1.00 0.99 1.00 0.02 0.01 0.85 0.12 0.01 0.00
#> c0 = 0.86  0.99 0.05 0.82 0.44 0.59 0.99 0.99 0.97 0.02 0.01 0.70 0.13 0.00 0.05
#> c0 = 0.852 1.00 0.09 0.81 0.47 0.65 1.00 1.00 0.98 0.24 0.02 0.72 0.17 0.00 0.07
#> c0 = 0.844 0.97 0.10 0.82 0.28 0.60 0.98 1.00 0.99 0.30 0.04 0.74 0.15 0.02 0.09
#>              43   44   45   46   47   48   49   50   51   52   53   54   55   56
#> c0 = 1     0.00 0.00 1.00 0.00 1.00 0.00 0.00 0.00 1.00 0.00 0.00 0.00 1.00 0.00
#> c0 = 0.949 0.00 0.00 1.00 0.00 1.00 0.00 0.00 0.00 1.00 0.00 0.00 0.00 1.00 0.00
#> c0 = 0.918 0.11 0.92 0.25 0.00 0.42 0.01 0.17 0.05 0.96 0.00 0.00 0.15 1.00 0.73
#> c0 = 0.899 0.09 0.90 0.29 0.00 0.28 0.04 0.05 0.01 0.84 0.00 0.00 0.03 1.00 0.44
#> c0 = 0.887 0.14 0.78 0.26 0.02 0.41 0.03 0.14 0.04 0.87 0.02 0.00 0.04 1.00 0.24
#> c0 = 0.878 0.20 0.88 0.55 0.00 0.36 0.12 0.16 0.04 0.77 0.02 0.00 0.17 1.00 0.33
#> c0 = 0.869 0.29 0.64 0.39 0.09 0.26 0.04 0.16 0.03 0.88 0.00 0.00 0.23 1.00 0.24
#> c0 = 0.86  0.35 0.39 0.37 0.16 0.28 0.15 0.12 0.14 0.81 0.05 0.01 0.13 1.00 0.28
#> c0 = 0.852 0.29 0.28 0.39 0.27 0.21 0.10 0.21 0.02 0.84 0.01 0.00 0.10 1.00 0.20
#> c0 = 0.844 0.30 0.39 0.40 0.19 0.20 0.12 0.12 0.04 0.75 0.05 0.03 0.04 1.00 0.28
#>              57   58   59   60   61   62   63   64   65   66   67   68   69   70
#> c0 = 1     0.00 0.00 0.00 0.00 0.00 0.00 0.00 1.00 0.00 0.00 1.00 1.00 0.00 1.00
#> c0 = 0.949 0.00 0.00 0.00 0.00 0.00 0.00 0.00 1.00 0.00 0.00 1.00 1.00 0.00 1.00
#> c0 = 0.918 0.08 0.05 0.02 0.17 0.00 0.03 0.04 0.57 0.06 0.27 1.00 1.00 0.04 0.09
#> c0 = 0.899 0.07 0.04 0.01 0.19 0.00 0.00 0.03 0.68 0.06 0.20 1.00 1.00 0.04 0.11
#> c0 = 0.887 0.06 0.06 0.08 0.23 0.00 0.03 0.06 0.55 0.05 0.14 1.00 1.00 0.04 0.15
#> c0 = 0.878 0.23 0.21 0.04 0.22 0.00 0.04 0.06 0.47 0.04 0.23 1.00 1.00 0.09 0.22
#> c0 = 0.869 0.24 0.11 0.11 0.22 0.00 0.08 0.06 0.53 0.08 0.17 1.00 1.00 0.07 0.24
#> c0 = 0.86  0.34 0.16 0.10 0.14 0.00 0.11 0.10 0.48 0.15 0.17 1.00 1.00 0.13 0.24
#> c0 = 0.852 0.32 0.23 0.10 0.25 0.00 0.11 0.13 0.42 0.16 0.33 0.99 1.00 0.10 0.28
#> c0 = 0.844 0.28 0.18 0.10 0.24 0.00 0.10 0.16 0.46 0.13 0.23 0.99 1.00 0.06 0.37
#>              71   72   73   74   75   76   77   78   79   80   81   82   83   84
#> c0 = 1     0.00 1.00 1.00 0.00 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 1.00
#> c0 = 0.949 0.00 1.00 1.00 0.00 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 1.00
#> c0 = 0.918 0.00 0.06 1.00 0.07 0.00 0.04 0.02 0.79 0.00 0.16 0.03 0.00 0.00 1.00
#> c0 = 0.899 0.00 0.05 1.00 0.03 0.00 0.01 0.00 0.59 0.00 0.17 0.05 0.00 0.00 0.99
#> c0 = 0.887 0.05 0.10 1.00 0.04 0.00 0.04 0.00 0.84 0.00 0.15 0.21 0.00 0.00 1.00
#> c0 = 0.878 0.02 0.09 1.00 0.05 0.03 0.03 0.01 0.68 0.00 0.11 0.18 0.00 0.00 0.99
#> c0 = 0.869 0.07 0.08 0.99 0.08 0.04 0.08 0.01 0.57 0.00 0.14 0.17 0.00 0.02 1.00
#> c0 = 0.86  0.08 0.10 0.98 0.11 0.04 0.12 0.01 0.70 0.00 0.17 0.19 0.00 0.02 0.99
#> c0 = 0.852 0.04 0.12 1.00 0.09 0.06 0.03 0.00 0.66 0.00 0.15 0.15 0.00 0.12 0.98
#> c0 = 0.844 0.07 0.15 0.96 0.06 0.02 0.10 0.13 0.70 0.00 0.12 0.17 0.17 0.12 1.00
#>              85   86   87   88   89   90   91   92   93   94   95   96   97   98
#> c0 = 1     0.00 1.00 0.00 0.00 0.00 0.00 1.00 0.00 1.00 1.00 0.00 0.00 0.00 1.00
#> c0 = 0.949 0.00 1.00 0.00 0.00 0.00 0.00 1.00 0.00 1.00 1.00 0.00 0.00 0.00 1.00
#> c0 = 0.918 0.26 0.07 0.03 0.71 0.01 0.63 0.31 0.00 0.71 0.98 0.00 0.22 0.02 0.85
#> c0 = 0.899 0.37 0.05 0.03 0.74 0.03 0.79 0.12 0.00 0.60 0.89 0.02 0.17 0.04 0.84
#> c0 = 0.887 0.45 0.10 0.09 0.61 0.05 0.41 0.31 0.00 0.60 0.89 0.00 0.15 0.03 0.82
#> c0 = 0.878 0.38 0.08 0.08 0.67 0.05 0.19 0.30 0.00 0.58 0.89 0.02 0.13 0.06 0.81
#> c0 = 0.869 0.45 0.07 0.17 0.87 0.08 0.14 0.43 0.02 0.36 0.93 0.02 0.11 0.05 0.62
#> c0 = 0.86  0.34 0.10 0.21 0.35 0.07 0.20 0.38 0.02 0.36 0.83 0.04 0.26 0.06 0.69
#> c0 = 0.852 0.25 0.14 0.18 0.33 0.10 0.17 0.48 0.03 0.45 0.81 0.01 0.22 0.05 0.72
#> c0 = 0.844 0.26 0.10 0.19 0.32 0.13 0.24 0.42 0.03 0.43 0.87 0.06 0.18 0.10 0.78
#>              99  100
#> c0 = 1     1.00 0.00
#> c0 = 0.949 1.00 0.00
#> c0 = 0.918 1.00 0.00
#> c0 = 0.899 1.00 0.11
#> c0 = 0.887 1.00 0.17
#> c0 = 0.878 1.00 0.14
#> c0 = 0.869 1.00 0.20
#> c0 = 0.86  1.00 0.20
#> c0 = 0.852 1.00 0.17
#> c0 = 0.844 1.00 0.24
#>  [ getOption("max.print") est atteint -- 42 lignes omises ]
#> attr(,"c0.seq")
#>  [1] 1.000000 0.948669 0.918435 0.899363 0.886751 0.878337 0.868850 0.859792
#>  [9] 0.851771 0.844105 0.837059 0.831572 0.824861 0.819308 0.812420 0.808494
#> [17] 0.803836 0.796767 0.792030 0.788540 0.784224 0.780365 0.778350 0.775013
#> [25] 0.771167 0.768773 0.766344 0.760684 0.757796 0.755312 0.752455 0.749371
#> [33] 0.745658 0.743105 0.740331 0.737751 0.734584 0.731335 0.729651 0.726482
#> [41] 0.724684 0.722907 0.719883 0.715867 0.712205 0.707677 0.704434 0.700935
#> [49] 0.699125 0.695577 0.693967 0.000000
#> attr(,"c0lim")
#> [1] TRUE
#> attr(,"steps.seq")
#>  [1] 0.0000000 0.9486685 0.9184348 0.8993626 0.8867508 0.8783368 0.8688498
#>  [8] 0.8597920 0.8517712 0.8441046 0.8370590 0.8315722 0.8248607 0.8193079
#> [15] 0.8124198 0.8084936 0.8038357 0.7967669 0.7920303 0.7885399 0.7842243
#> [22] 0.7803654 0.7783504 0.7750129 0.7711674 0.7687731 0.7663441 0.7606838
#> [29] 0.7577961 0.7553123 0.7524554 0.7493711 0.7456580 0.7431055 0.7403313
#> [36] 0.7377508 0.7345842 0.7313349 0.7296512 0.7264820 0.7246836 0.7229066
#> [43] 0.7198827 0.7158667 0.7122053 0.7076771 0.7044341 0.7009353 0.6991250
#> [50] 0.6955766 0.6939670 1.0000000
#> attr(,"typeboost")
#> [1] "fastboost"
#> attr(,"limi_alea")
#> [1] NA
#> attr(,"B")
#> [1] 100
#> attr(,"class")
#> [1] "selectboost"
```

The summary function computes applies a non increasing post-processing step to the results to improve the performance of the algorithm. The results are store int the selectboost_result.dec entry of the summary.

```r
dec.result.boost.micro_nb1 <- summary(result.boost.micro_nb1)$selectboost_result.dec
dec.result.boost.micro_nb1
#>               1    2    3    4    5 6 7 8 9 10 11   12 13 14   15 16 17   18
#>            1.00 1.00 1.00 1.00 1.00 0 0 0 0  0  0 1.00  0  0 1.00  0  0 1.00
#> c0 = 0.949 1.00 1.00 1.00 1.00 1.00 0 0 0 0  0  0 1.00  0  0 1.00  0  0 1.00
#> c0 = 0.918 1.00 1.00 1.00 0.91 1.00 0 0 0 0  0  0 0.69  0  0 0.86  0  0 0.84
#> c0 = 0.899 1.00 1.00 1.00 0.91 1.00 0 0 0 0  0  0 0.57  0  0 0.86  0  0 0.84
#> c0 = 0.887 1.00 1.00 0.64 0.79 1.00 0 0 0 0  0  0 0.57  0  0 0.76  0  0 0.84
#> c0 = 0.878 1.00 1.00 0.55 0.79 1.00 0 0 0 0  0  0 0.43  0  0 0.76  0  0 0.84
#> c0 = 0.869 1.00 1.00 0.51 0.68 1.00 0 0 0 0  0  0 0.35  0  0 0.69  0  0 0.83
#> c0 = 0.86  1.00 1.00 0.51 0.68 1.00 0 0 0 0  0  0 0.35  0  0 0.69  0  0 0.79
#> c0 = 0.852 1.00 1.00 0.49 0.68 1.00 0 0 0 0  0  0 0.35  0  0 0.69  0  0 0.75
#> c0 = 0.844 1.00 1.00 0.49 0.68 1.00 0 0 0 0  0  0 0.30  0  0 0.65  0  0 0.75
#>              19 20 21 22 23 24   25 26   27 28   29 30   31 32 33           34
#>            1.00  0  0  0  0  0 1.00  0 1.00  0 1.00  0 1.00  0  0 1.000000e+00
#> c0 = 0.949 1.00  0  0  0  0  0 1.00  0 1.00  0 1.00  0 1.00  0  0 1.000000e+00
#> c0 = 0.918 1.00  0  0  0  0  0 0.88  0 0.05  0 1.00  0 0.84  0  0 1.000000e+00
#> c0 = 0.899 1.00  0  0  0  0  0 0.88  0 0.05  0 1.00  0 0.84  0  0 1.000000e+00
#> c0 = 0.887 1.00  0  0  0  0  0 0.87  0 0.01  0 1.00  0 0.78  0  0 1.000000e+00
#> c0 = 0.878 1.00  0  0  0  0  0 0.86  0 0.01  0 0.99  0 0.72  0  0 1.000000e+00
#> c0 = 0.869 0.98  0  0  0  0  0 0.83  0 0.01  0 0.99  0 0.72  0  0 1.000000e+00
#> c0 = 0.86  0.98  0  0  0  0  0 0.78  0 0.01  0 0.99  0 0.68  0  0 9.900000e-01
#> c0 = 0.852 0.97  0  0  0  0  0 0.76  0 0.00  0 0.99  0 0.67  0  0 9.900000e-01
#> c0 = 0.844 0.97  0  0  0  0  0 0.67  0 0.00  0 0.96  0 0.67  0  0 9.700000e-01
#>              35   36 37 38   39 40 41 42 43 44   45 46   47 48 49 50   51 52 53
#>            1.00 1.00  0  0 1.00  0  0  0  0  0 1.00  0 1.00  0  0  0 1.00  0  0
#> c0 = 0.949 1.00 1.00  0  0 1.00  0  0  0  0  0 1.00  0 1.00  0  0  0 1.00  0  0
#> c0 = 0.918 1.00 0.99  0  0 0.89  0  0  0  0  0 0.25  0 0.42  0  0  0 0.96  0  0
#> c0 = 0.899 1.00 0.99  0  0 0.89  0  0  0  0  0 0.25  0 0.28  0  0  0 0.84  0  0
#> c0 = 0.887 1.00 0.97  0  0 0.79  0  0  0  0  0 0.22  0 0.28  0  0  0 0.84  0  0
#> c0 = 0.878 1.00 0.97  0  0 0.79  0  0  0  0  0 0.22  0 0.23  0  0  0 0.74  0  0
#> c0 = 0.869 0.99 0.97  0  0 0.79  0  0  0  0  0 0.06  0 0.13  0  0  0 0.74  0  0
#> c0 = 0.86  0.99 0.94  0  0 0.64  0  0  0  0  0 0.04  0 0.13  0  0  0 0.67  0  0
#> c0 = 0.852 0.99 0.94  0  0 0.64  0  0  0  0  0 0.04  0 0.06  0  0  0 0.67  0  0
#> c0 = 0.844 0.99 0.94  0  0 0.64  0  0  0  0  0 0.04  0 0.05  0  0  0 0.58  0  0
#>            54   55 56 57 58 59 60 61 62 63   64 65 66   67   68 69   70 71   72
#>             0 1.00  0  0  0  0  0  0  0  0 1.00  0  0 1.00 1.00  0 1.00  0 1.00
#> c0 = 0.949  0 1.00  0  0  0  0  0  0  0  0 1.00  0  0 1.00 1.00  0 1.00  0 1.00
#> c0 = 0.918  0 1.00  0  0  0  0  0  0  0  0 0.57  0  0 1.00 1.00  0 0.09  0 0.06
#> c0 = 0.899  0 1.00  0  0  0  0  0  0  0  0 0.57  0  0 1.00 1.00  0 0.09  0 0.05
#> c0 = 0.887  0 1.00  0  0  0  0  0  0  0  0 0.44  0  0 1.00 1.00  0 0.09  0 0.05
#> c0 = 0.878  0 1.00  0  0  0  0  0  0  0  0 0.36  0  0 1.00 1.00  0 0.09  0 0.04
#> c0 = 0.869  0 1.00  0  0  0  0  0  0  0  0 0.36  0  0 1.00 1.00  0 0.09  0 0.03
#> c0 = 0.86   0 1.00  0  0  0  0  0  0  0  0 0.31  0  0 1.00 1.00  0 0.09  0 0.03
#> c0 = 0.852  0 1.00  0  0  0  0  0  0  0  0 0.25  0  0 0.99 1.00  0 0.09  0 0.03
#> c0 = 0.844  0 1.00  0  0  0  0  0  0  0  0 0.25  0  0 0.99 1.00  0 0.09  0 0.03
#>              73 74 75 76 77   78 79 80 81 82 83   84 85   86 87 88 89 90   91 92
#>            1.00  0  0  0  0 1.00  0  0  0  0  0 1.00  0 1.00  0  0  0  0 1.00  0
#> c0 = 0.949 1.00  0  0  0  0 1.00  0  0  0  0  0 1.00  0 1.00  0  0  0  0 1.00  0
#> c0 = 0.918 1.00  0  0  0  0 0.79  0  0  0  0  0 1.00  0 0.07  0  0  0  0 0.31  0
#> c0 = 0.899 1.00  0  0  0  0 0.59  0  0  0  0  0 0.99  0 0.05  0  0  0  0 0.12  0
#> c0 = 0.887 1.00  0  0  0  0 0.59  0  0  0  0  0 0.99  0 0.05  0  0  0  0 0.12  0
#> c0 = 0.878 1.00  0  0  0  0 0.43  0  0  0  0  0 0.98  0 0.03  0  0  0  0 0.11  0
#> c0 = 0.869 0.99  0  0  0  0 0.32  0  0  0  0  0 0.98  0 0.02  0  0  0  0 0.11  0
#> c0 = 0.86  0.98  0  0  0  0 0.32  0  0  0  0  0 0.97  0 0.02  0  0  0  0 0.06  0
#> c0 = 0.852 0.98  0  0  0  0 0.28  0  0  0  0  0 0.96  0 0.02  0  0  0  0 0.06  0
#> c0 = 0.844 0.94  0  0  0  0 0.28  0  0  0  0  0 0.96  0 0.00  0  0  0  0 0.00  0
#>              93   94 95 96 97   98   99 100
#>            1.00 1.00  0  0  0 1.00 1.00   0
#> c0 = 0.949 1.00 1.00  0  0  0 1.00 1.00   0
#> c0 = 0.918 0.71 0.98  0  0  0 0.85 1.00   0
#> c0 = 0.899 0.60 0.89  0  0  0 0.84 1.00   0
#> c0 = 0.887 0.60 0.89  0  0  0 0.82 1.00   0
#> c0 = 0.878 0.58 0.89  0  0  0 0.81 1.00   0
#> c0 = 0.869 0.36 0.89  0  0  0 0.62 1.00   0
#> c0 = 0.86  0.36 0.79  0  0  0 0.62 1.00   0
#> c0 = 0.852 0.36 0.77  0  0  0 0.62 1.00   0
#> c0 = 0.844 0.34 0.77  0  0  0 0.62 1.00   0
#>  [ getOption("max.print") est atteint -- 42 lignes omises ]
```

#### Confidence indices.

First compute the highest $c_0$ value for which the proportion of selection is under the threshold $thr$. In that analysis, we set $thr=1$.

```r
thr=1
index.last.c0.micro_nb1=apply(dec.result.boost.micro_nb1>=thr,2,which.min)-1
index.last.c0.micro_nb1
#>   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
#>  13  15   4   2  16   0   0   0   0   0   0   2   0   0   2   0   0   2   6   0 
#>  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40 
#>   0   0   0   0   2   0   2   0   5   0   2   0   0   7   6   2   0   0   2   0 
#>  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60 
#>   0   0   0   0   2   0   2   0   0   0   2   0   0   0  16   0   0   0   0   0 
#>  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80 
#>   0   0   0   2   0   0   8  17   0   2   0   2   6   0   0   0   0   2   0   0 
#>  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 
#>   0   0   0   3   0   2   0   0   0   0   2   0   2   2   0   0   0   2  16   0
```

We have to cap the confidence index value to the $1-\{\textrm{smallest } c_0\}$ that we specified in the $c_0$ sequence and that was actually used for resampling. As a consequence, we have to exclude the $c_0=0$ case since we do not know what happen between $c0=\mathrm{quantile}(cors,.9)$ and $c_0=0$.


```r
index.last.c0.micro_nb1 <- pmin(index.last.c0.micro_nb1,
                                nrow(dec.result.boost.micro_nb1)-1)
```

Define some colorRamp ranging from blue (high confidence) to red (low confidence).

```r
jet.colors <-colorRamp(rev(c("blue", "#007FFF", "#FF7F00", "red", "#7F0000")))
```


```r
rownames(dec.result.boost.micro_nb1)[index.last.c0.micro_nb1]
#>  [1] "c0 = 0.825" "c0 = 0.812" "c0 = 0.899" "c0 = 0.949" "c0 = 0.808"
#>  [6] "c0 = 0.949" "c0 = 0.949" "c0 = 0.949" "c0 = 0.878" "c0 = 0.949"
#> [11] "c0 = 0.949" "c0 = 0.887" "c0 = 0.949" "c0 = 0.869" "c0 = 0.878"
#> [16] "c0 = 0.949" "c0 = 0.949" "c0 = 0.949" "c0 = 0.949" "c0 = 0.949"
#> [21] "c0 = 0.808" "c0 = 0.949" "c0 = 0.86"  "c0 = 0.804" "c0 = 0.949"
#> [26] "c0 = 0.949" "c0 = 0.878" "c0 = 0.949" "c0 = 0.918" "c0 = 0.949"
#> [31] "c0 = 0.949" "c0 = 0.949" "c0 = 0.949" "c0 = 0.949" "c0 = 0.808"
attr(result.boost.micro_nb1,"c0.seq")[index.last.c0.micro_nb1]
#>  [1] 0.824861 0.812420 0.899363 0.948669 0.808494 0.948669 0.948669 0.948669
#>  [9] 0.878337 0.948669 0.948669 0.886751 0.948669 0.868850 0.878337 0.948669
#> [17] 0.948669 0.948669 0.948669 0.948669 0.808494 0.948669 0.859792 0.803836
#> [25] 0.948669 0.948669 0.878337 0.948669 0.918435 0.948669 0.948669 0.948669
#> [33] 0.948669 0.948669 0.808494
confidence.indices.micro_nb1 = c(0,1-attr(result.boost.micro_nb1,"c0.seq"))[
  index.last.c0.micro_nb1+1]
confidence.indices.micro_nb1
#>   [1] 0.175139 0.187580 0.100637 0.051331 0.191506 0.000000 0.000000 0.000000
#>   [9] 0.000000 0.000000 0.000000 0.051331 0.000000 0.000000 0.051331 0.000000
#>  [17] 0.000000 0.051331 0.121663 0.000000 0.000000 0.000000 0.000000 0.000000
#>  [25] 0.051331 0.000000 0.051331 0.000000 0.113249 0.000000 0.051331 0.000000
#>  [33] 0.000000 0.131150 0.121663 0.051331 0.000000 0.000000 0.051331 0.000000
#>  [41] 0.000000 0.000000 0.000000 0.000000 0.051331 0.000000 0.051331 0.000000
#>  [49] 0.000000 0.000000 0.051331 0.000000 0.000000 0.000000 0.191506 0.000000
#>  [57] 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.051331
#>  [65] 0.000000 0.000000 0.140208 0.196164 0.000000 0.051331 0.000000 0.051331
#>  [73] 0.121663 0.000000 0.000000 0.000000 0.000000 0.051331 0.000000 0.000000
#>  [81] 0.000000 0.000000 0.000000 0.081565 0.000000 0.051331 0.000000 0.000000
#>  [89] 0.000000 0.000000 0.051331 0.000000 0.051331 0.051331 0.000000 0.000000
#>  [97] 0.000000 0.051331 0.191506 0.000000
barplot(confidence.indices.micro_nb1,col=rgb(jet.colors(confidence.indices.micro_nb1),
maxColorValue = 255), names.arg=colnames(result.boost.micro_nb1), ylim=c(0,1))
abline(h=)
```

<img src="man/figures/README-CascadeDatabarplot-1.png" title="plot of chunk CascadeDatabarplot" alt="plot of chunk CascadeDatabarplot" width="100%" />


Let's compute again, for all the $c_0$ values, the recall (sensitivity), precision (positive predictive value), and several Fscores ($F_1$ harmonic mean of recall and precision, $F_{1/2}$ and $F_2$ two weighted harmonic means of recall and precision).

```r
All_micro_nb1=NULL
#Here are the cutoff level tested
for(lev.micro_nb1 in 20:10/20){
F_score.micro_nb1=NULL
for(u.micro_nb1 in 1:nrow(dec.result.boost.micro_nb1
)){
	F_score.micro_nb1<-rbind(F_score.micro_nb1,SelectBoost::compsim(DATA_exemple,
	                                                                dec.result.boost.micro_nb1[u.micro_nb1,],level=lev.micro_nb1)[1:5])
}
All_micro_nb1 <- abind::abind(All_micro_nb1,F_score.micro_nb1,along=3)
}
```

For a selection threshold equal to $0.90$, all the c0 values and the 5 criteria.

```r
matplot(1:nrow(dec.result.boost.micro_nb1),All_micro_nb1[,,3],type="l",
        ylab="criterion value",xlab="c0 value",xaxt="n",lwd=2)
axis(1, at=1:length(attr(result.boost.micro_nb1,"c0.seq")),
     labels=round(attr(result.boost.micro_nb1,"c0.seq"),3))
legend(x="topright",legend=c("recall (sensitivity)",
       "precision (positive predictive value)","non-weighted Fscore",
       "F1/2 weighted Fscore","F2 weighted Fscore"),
       lty=1:5,col=1:5,lwd=2)
```

<img src="man/figures/README-datasetsimulation6micro-1.png" title="plot of chunk datasetsimulation6micro" alt="plot of chunk datasetsimulation6micro" width="100%" />

Fscores for all selection thresholds and all the $c_0$ values.

```r
matplot(1:nrow(dec.result.boost.micro_nb1),All_micro_nb1[,3,],type="l",
        ylab="Fscore",xlab="c0 value",xaxt="n",lwd=2,col=1:11,lty=1:11)
```

<img src="man/figures/README-datasetsimulation7micro-1.png" title="plot of chunk datasetsimulation7micro" alt="plot of chunk datasetsimulation7micro" width="100%" />

```r
axis(1, at=1:length(attr(result.boost.micro_nb1,"c0.seq")), 
     labels=round(attr(result.boost.micro_nb1,"c0.seq"),3))
```

<img src="man/figures/README-datasetsimulation7micro-2.png" title="plot of chunk datasetsimulation7micro" alt="plot of chunk datasetsimulation7micro" width="100%" />

```r
legend(x="bottomright",legend=(20:11)/20,lty=1:11,col=1:11,lwd=2,
       title="Threshold")
```

<img src="man/figures/README-datasetsimulation7micro-3.png" title="plot of chunk datasetsimulation7micro" alt="plot of chunk datasetsimulation7micro" width="100%" />

