<!-- README.md is generated from README.Rmd. Please edit that file -->



# A General Algorithm to Enhance the Performance of Variable Selection Methods in Correlated Datasets


### *Frédéric Bertrand and Myriam Maumy-Bertrand*


[![CRAN status](https://www.r-pkg.org/badges/version/SelectBoost)](https://cran.r-project.org/package=SelectBoost)
[![DOI](https://zenodo.org/badge/136206211.svg)](https://zenodo.org/badge/latestdoi/136206211)


----------------


# SelectBoost



With the growth of big data, variable selection has become one of the major challenges in statistics. Although many methods have been proposed in the literature their performance in terms of recall and precision are limited in a context where the number of variables by far exceeds the number of observations or in a high correlated setting. Results: This package implements a new general algorithm which improves the precision of any existing variable selection method. This algorithm is based on highly intensive simulations and takes into account the correlation structure of the data. Our algorithm can either produce a confidence index for variable selection or it can be used in an experimental design planning perspective.


This website and these examples were created by F. Bertrand and M. Maumy-Bertrand.

## Installation

You can install the released version of SelectBoost from [CRAN](https://CRAN.R-project.org) with:


```r
install.packages("SelectBoost")
```

You can install the development version of SelectBoost from [github](https://github.com) with:


```r
devtools::install_github("fbertran/SelectBoost")
```

If you are a Linux/Unix or a Macos user, you can install a version of SelectBoost with support for `doMC` from [github](https://github.com) with:


```r
devtools::install_github("fbertran/SelectBoost", ref = "doMC")
```


## Examples

### First example: Simulated dataset 

#### Simulating data
Create a correlation matrix for two groups of variable with an intragroup correlation value of $cor\_group$.

```r
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
#>             [,1]        [,2]       [,3]         [,4]       [,5]        [,6]
#> [1,] -1.72442776 -0.10046047 -2.2595088  0.001124577 -1.3419203 -1.13267066
#> [2,] -0.93379720  0.01954047 -0.6891294 -1.321628670 -0.7263601 -0.18728905
#> [3,]  0.70060982  1.33778478  0.7307758  0.638194535  1.3237905  1.81314310
#> [4,] -1.56064223  0.58054614 -2.1643902  2.108551452 -2.3780180 -0.60262482
#> [5,] -0.02507007 -0.78832052 -0.0536616 -0.246697324 -0.3540182  0.04246783
#> [6,] -1.67197858  0.18552748 -0.7988037  0.407939162 -1.5655295 -0.29098080
#>             [,7]       [,8]        [,9]      [,10]
#> [1,] -0.53784338 -0.9655456 -0.75837644 -1.3010891
#> [2,] -0.42947965 -1.3754401 -0.05911396 -1.2358071
#> [3,]  0.79599202  2.1892071  0.61677553  1.5136174
#> [4,] -1.57412045  0.4685144 -1.52966945  0.9789675
#> [5,] -0.03143043 -0.5312222  1.03171455 -0.3228274
#> [6,] -1.32085372 -0.7218018 -0.72804471 -1.2519241
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
#>  $ X      : num [1:100, 1:10] -1.7244 -0.9338 0.7006 -1.5606 -0.0251 ...
#>  $ Y      : num [1:100] 5.78 2.26 -4.47 4.16 1.56 ...
#>  $ support: num [1:10] 1 1 1 0 0 0 0 0 0 0
#>  $ beta   : num [1:10] -1.44 -1.84 -1.38 0 0 ...
#>  $ stn    : num 500
#>  $ sigma  : num [1, 1] 0.162
#>  - attr(*, "class")= chr "simuls"
```

#### Selectboost analysis

By default `fastboost` performs $B=100$ resamplings of the model. As a result, we get a matrix with the proportions of selection of each variable at a given resampling level $c_0$. The resampling are designed to take into account the correlation structure of the predictors. The correlation used by default is the Pearson Correlation but any can be passed through the `corrfunc` argument. The $c_0$ value sets the minimum level for which correlations between two predictors are kept in the resampling process. The correlation structure is used to group the variables. Two groups functions `group_func_1`, grouping by thresholding the correlation matrix, and `group_func_2`, grouping using community selection, are available but any can be provided using the `group` argument of the function. The `func` argument is the variable selection function that should be used to assess variable memberships. It  defaults to `lasso_msgps_AICc` but many others, for instance for lasso, elastinet, logistic glmnet and network inference with the [Cascade package](http://fbertran.github.io/Cascade/), are provided:

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
#>          0%         10%         20%         30%         40%         50%         60% 
#> 0.004507965 0.022993559 0.051339626 0.082474761 0.136768300 0.148901096 0.227120116 
#>         70%         80%         90%        100% 
#> 0.401467864 0.799502462 0.821611002 0.852633226
```


```r
result.boost.raw = fastboost(DATA_exemple$X, DATA_exemple$Y)
result.boost.raw
#>               1    2    3    4    5    6    7    8    9   10
#> c0 = 1     1.00 1.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> c0 = 0.853 1.00 1.00 1.00 0.43 0.46 0.88 1.00 0.61 0.99 0.64
#> c0 = 0.821 0.99 1.00 1.00 0.36 0.92 0.67 0.95 0.52 1.00 0.87
#> c0 = 0.8   1.00 1.00 1.00 0.29 1.00 0.94 0.91 0.77 1.00 0.98
#> c0 = 0.391 0.97 0.85 0.95 1.00 0.97 0.95 0.97 0.37 0.97 1.00
#> c0 = 0.227 0.89 0.90 0.88 0.99 0.93 0.98 0.82 0.94 0.89 0.97
#> c0 = 0.149 0.82 0.90 0.85 0.94 0.96 0.99 0.99 0.96 0.96 0.96
#> c0 = 0.137 0.82 0.95 0.81 0.95 0.96 0.99 1.00 0.95 0.99 0.93
#> c0 = 0.083 0.89 0.91 0.89 0.90 1.00 0.97 0.96 0.91 0.91 0.91
#> c0 = 0.051 0.98 0.83 0.99 0.82 0.93 0.93 0.98 0.88 0.89 0.92
#> c0 = 0.024 0.99 0.97 0.97 0.87 0.95 0.91 1.00 0.92 0.93 0.96
#> c0 = 0.005 1.00 0.97 0.98 0.82 0.96 0.93 0.98 0.97 0.96 0.94
#> c0 = 0     0.95 0.92 0.94 0.94 0.95 0.94 0.84 0.93 0.94 0.98
#> attr(,"c0.seq")
#>              100%      90%      80%      70%      60%      50%      40%      30% 
#> 1.000000 0.852633 0.821407 0.799502 0.391184 0.227120 0.148901 0.136768 0.082534 
#>      20%      10%       0%          
#> 0.051340 0.023623 0.004508 0.000000 
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
#> c0 = 0.853 1.00 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.821 0.99 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.8   0.99 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.391 0.96 0.85 0.95 0 0 0 0 0 0  0
#> c0 = 0.227 0.88 0.85 0.88 0 0 0 0 0 0  0
#> c0 = 0.149 0.81 0.85 0.85 0 0 0 0 0 0  0
#> c0 = 0.137 0.81 0.85 0.81 0 0 0 0 0 0  0
#> c0 = 0.083 0.81 0.81 0.81 0 0 0 0 0 0  0
#> c0 = 0.051 0.81 0.73 0.81 0 0 0 0 0 0  0
#> c0 = 0.024 0.81 0.73 0.79 0 0 0 0 0 0  0
#> c0 = 0.005 0.81 0.73 0.79 0 0 0 0 0 0  0
#> c0 = 0     0.76 0.68 0.75 0 0 0 0 0 0  0
#> attr(,"c0.seq")
#>              100%      90%      80%      70%      60%      50%      40%      30% 
#> 1.000000 0.852633 0.821407 0.799502 0.391184 0.227120 0.148901 0.136768 0.082534 
#>      20%      10%       0%          
#> 0.051340 0.023623 0.004508 0.000000 
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

With such datasets, we can perform all the 45 steps for the Selectboost analysis. We switch to  community analysis from the [igraph package](http://igraph.org) as the grouping variable function.

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
#> c0 = 0.853 1.00 1.00 1.00 0.44 0.49 0.90 1.00 0.60 1.00 0.64
#> c0 = 0.844 1.00 1.00 1.00 0.42 0.44 0.88 1.00 0.59 0.99 0.60
#> c0 = 0.836 0.99 1.00 1.00 0.17 0.35 0.92 1.00 0.86 1.00 0.82
#> c0 = 0.826 1.00 1.00 1.00 0.31 0.41 0.94 1.00 0.84 1.00 0.82
#> c0 = 0.822 1.00 1.00 1.00 0.36 0.95 0.70 0.94 0.54 1.00 0.90
#> c0 = 0.821 1.00 1.00 1.00 0.43 0.98 0.71 0.93 0.46 1.00 0.89
#> c0 = 0.811 0.99 1.00 1.00 0.39 0.97 0.72 0.88 0.45 1.00 0.87
#> c0 = 0.81  1.00 1.00 1.00 0.33 1.00 0.94 0.79 0.74 1.00 0.99
#> c0 = 0.807 0.98 1.00 1.00 0.41 1.00 0.92 0.84 0.69 1.00 0.94
#> c0 = 0.798 1.00 1.00 1.00 0.26 1.00 0.93 0.91 0.70 0.99 0.98
#> c0 = 0.489 0.99 1.00 1.00 0.27 0.99 0.97 1.00 0.74 1.00 1.00
#> c0 = 0.445 0.94 1.00 0.92 1.00 0.94 1.00 0.96 0.73 0.96 1.00
#> c0 = 0.413 0.98 0.80 0.95 1.00 0.94 1.00 0.98 0.78 0.98 1.00
#> c0 = 0.401 0.94 0.80 0.98 1.00 0.96 1.00 0.98 0.78 0.95 1.00
#> c0 = 0.35  0.94 0.93 0.94 1.00 0.90 0.88 0.97 0.34 0.96 1.00
#> c0 = 0.282 0.93 0.96 0.96 1.00 0.92 0.97 0.94 0.43 0.96 0.92
#> c0 = 0.271 0.95 0.94 0.93 1.00 0.95 0.92 0.93 0.54 0.96 0.89
#> c0 = 0.266 0.95 0.77 0.93 1.00 0.95 0.95 0.94 0.94 0.93 0.93
#> c0 = 0.201 0.87 0.85 0.92 0.99 0.93 0.99 0.93 0.91 0.91 0.96
#> c0 = 0.196 0.91 0.93 0.90 0.99 0.92 0.97 0.88 0.93 0.92 0.94
#> c0 = 0.192 0.92 0.88 0.92 0.97 0.89 0.97 0.90 0.92 0.88 0.96
#> c0 = 0.156 0.96 0.93 0.72 0.96 0.90 0.96 0.90 0.95 0.94 0.90
#> c0 = 0.149 0.89 0.93 0.79 0.97 0.99 0.95 0.99 0.93 0.94 0.89
#> c0 = 0.144 0.82 0.95 0.79 0.95 0.96 0.95 0.96 0.94 0.98 0.94
#> c0 = 0.142 0.81 0.92 0.81 0.94 0.97 0.93 0.95 0.96 0.99 0.96
#> c0 = 0.141 0.81 0.98 0.90 0.94 0.96 0.94 0.98 0.98 0.95 0.98
#> c0 = 0.139 0.85 0.96 0.91 0.92 0.98 0.99 0.98 0.94 0.97 0.94
#> c0 = 0.133 0.82 0.93 0.89 0.96 0.97 0.94 0.98 0.95 0.96 0.89
#> c0 = 0.098 0.81 0.97 0.85 0.96 1.00 0.94 0.92 0.98 0.99 0.92
#> c0 = 0.093 0.91 0.94 0.86 0.98 0.98 0.92 0.90 0.96 0.99 0.96
#> c0 = 0.083 0.84 0.93 0.90 0.94 1.00 0.96 0.96 0.92 0.95 0.91
#> c0 = 0.082 0.89 0.91 0.85 0.94 1.00 0.93 0.96 0.92 0.96 0.94
#> c0 = 0.077 0.86 0.95 0.87 0.92 1.00 0.93 0.95 0.94 0.82 0.90
#> c0 = 0.071 0.91 0.98 0.94 0.91 1.00 0.87 0.90 0.90 0.88 0.91
#> c0 = 0.055 0.94 0.86 0.98 0.93 0.99 0.91 1.00 0.91 0.98 0.93
#> c0 = 0.052 0.98 0.88 0.99 0.89 0.97 0.96 0.98 0.89 0.91 0.97
#> c0 = 0.048 0.98 0.90 1.00 0.88 0.94 0.95 0.99 0.86 0.93 0.96
#> c0 = 0.032 1.00 0.92 0.98 0.92 0.94 0.95 0.99 0.91 0.91 0.96
#> c0 = 0.031 0.99 0.94 0.99 0.90 0.93 0.97 0.98 0.96 0.96 0.93
#> c0 = 0.025 0.99 0.99 0.97 0.79 0.97 0.95 1.00 0.93 0.91 0.95
#> c0 = 0.023 1.00 0.95 0.96 0.92 0.95 0.95 0.99 0.91 0.91 0.98
#> c0 = 0.021 1.00 0.94 0.99 0.86 0.92 0.94 0.98 0.93 0.93 0.96
#> c0 = 0.02  1.00 0.96 0.98 0.89 1.00 0.97 0.94 0.96 0.93 0.95
#> c0 = 0.011 0.99 0.98 0.97 0.77 0.94 0.96 0.96 0.97 0.95 0.96
#> c0 = 0.005 1.00 0.97 0.98 0.78 0.99 0.93 0.97 0.96 0.97 0.97
#> c0 = 0     0.95 0.98 0.96 0.92 0.95 0.93 0.92 0.94 0.96 0.91
#> attr(,"c0.seq")
#>  [1] 1.000000 0.852633 0.844427 0.835515 0.826228 0.821611 0.821100 0.811418 0.810240
#> [10] 0.806897 0.797654 0.488712 0.444650 0.412741 0.401468 0.350050 0.281902 0.271109
#> [19] 0.266162 0.201092 0.195846 0.192118 0.155810 0.148901 0.144035 0.141782 0.140643
#> [28] 0.139279 0.133002 0.097694 0.093412 0.082770 0.082475 0.077067 0.071189 0.055336
#> [37] 0.052168 0.048026 0.032246 0.030982 0.024566 0.022994 0.021494 0.020089 0.011099
#> [46] 0.004508 0.000000
#> attr(,"c0lim")
#> [1] TRUE
#> attr(,"steps.seq")
#>  [1] 0.000000000 0.852633226 0.844426695 0.835515267 0.826227837 0.821611002
#>  [7] 0.821100330 0.811417699 0.810240150 0.806896907 0.797653851 0.488711802
#> [13] 0.444649742 0.412741476 0.401467864 0.350050092 0.281902455 0.271108711
#> [19] 0.266161872 0.201092279 0.195845507 0.192117678 0.155809745 0.148901096
#> [25] 0.144035411 0.141782024 0.140643234 0.139279240 0.133001890 0.097694414
#> [31] 0.093411832 0.082770297 0.082474761 0.077066975 0.071188689 0.055335565
#> [37] 0.052168111 0.048025686 0.032245933 0.030982117 0.024566078 0.022993559
#> [43] 0.021493806 0.020089237 0.011099144 0.004507965 1.000000000
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
#> c0 = 0.853 1.00 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.844 1.00 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.836 0.99 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.826 0.99 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.822 0.99 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.821 0.99 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.811 0.98 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.81  0.98 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.807 0.96 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.798 0.96 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.489 0.95 1.00 1.00 0 0 0 0 0 0  0
#> c0 = 0.445 0.90 1.00 0.92 0 0 0 0 0 0  0
#> c0 = 0.413 0.90 0.80 0.92 0 0 0 0 0 0  0
#> c0 = 0.401 0.86 0.80 0.92 0 0 0 0 0 0  0
#> c0 = 0.35  0.86 0.80 0.88 0 0 0 0 0 0  0
#> c0 = 0.282 0.85 0.80 0.88 0 0 0 0 0 0  0
#> c0 = 0.271 0.85 0.78 0.85 0 0 0 0 0 0  0
#> c0 = 0.266 0.85 0.61 0.85 0 0 0 0 0 0  0
#> c0 = 0.201 0.77 0.61 0.84 0 0 0 0 0 0  0
#> c0 = 0.196 0.77 0.61 0.82 0 0 0 0 0 0  0
#> c0 = 0.192 0.77 0.56 0.82 0 0 0 0 0 0  0
#> c0 = 0.156 0.77 0.56 0.62 0 0 0 0 0 0  0
#> c0 = 0.149 0.70 0.56 0.62 0 0 0 0 0 0  0
#> c0 = 0.144 0.63 0.56 0.62 0 0 0 0 0 0  0
#> c0 = 0.142 0.62 0.53 0.62 0 0 0 0 0 0  0
#> c0 = 0.141 0.62 0.53 0.62 0 0 0 0 0 0  0
#> c0 = 0.139 0.62 0.51 0.62 0 0 0 0 0 0  0
#> c0 = 0.133 0.59 0.48 0.60 0 0 0 0 0 0  0
#> c0 = 0.098 0.58 0.48 0.56 0 0 0 0 0 0  0
#> c0 = 0.093 0.58 0.45 0.56 0 0 0 0 0 0  0
#> c0 = 0.083 0.51 0.44 0.56 0 0 0 0 0 0  0
#> c0 = 0.082 0.51 0.42 0.51 0 0 0 0 0 0  0
#> c0 = 0.077 0.48 0.42 0.51 0 0 0 0 0 0  0
#> c0 = 0.071 0.48 0.42 0.51 0 0 0 0 0 0  0
#> c0 = 0.055 0.48 0.30 0.51 0 0 0 0 0 0  0
#> c0 = 0.052 0.48 0.30 0.51 0 0 0 0 0 0  0
#> c0 = 0.048 0.48 0.30 0.51 0 0 0 0 0 0  0
#> c0 = 0.032 0.48 0.30 0.49 0 0 0 0 0 0  0
#> c0 = 0.031 0.47 0.30 0.49 0 0 0 0 0 0  0
#> c0 = 0.025 0.47 0.30 0.47 0 0 0 0 0 0  0
#> c0 = 0.023 0.47 0.26 0.46 0 0 0 0 0 0  0
#> c0 = 0.021 0.47 0.25 0.46 0 0 0 0 0 0  0
#> c0 = 0.02  0.47 0.25 0.45 0 0 0 0 0 0  0
#> c0 = 0.011 0.46 0.25 0.44 0 0 0 0 0 0  0
#> c0 = 0.005 0.46 0.24 0.44 0 0 0 0 0 0  0
#> c0 = 0     0.41 0.24 0.42 0 0 0 0 0 0  0
#> attr(,"c0.seq")
#>  [1] 1.000000 0.852633 0.844427 0.835515 0.826228 0.821611 0.821100 0.811418 0.810240
#> [10] 0.806897 0.797654 0.488712 0.444650 0.412741 0.401468 0.350050 0.281902 0.271109
#> [19] 0.266162 0.201092 0.195846 0.192118 0.155810 0.148901 0.144035 0.141782 0.140643
#> [28] 0.139279 0.133002 0.097694 0.093412 0.082770 0.082475 0.077067 0.071189 0.055336
#> [37] 0.052168 0.048026 0.032246 0.030982 0.024566 0.022994 0.021494 0.020089 0.011099
#> [46] 0.004508 0.000000
#> attr(,"c0lim")
#> [1] TRUE
#> attr(,"steps.seq")
#>  [1] 0.000000000 0.852633226 0.844426695 0.835515267 0.826227837 0.821611002
#>  [7] 0.821100330 0.811417699 0.810240150 0.806896907 0.797653851 0.488711802
#> [13] 0.444649742 0.412741476 0.401467864 0.350050092 0.281902455 0.271108711
#> [19] 0.266161872 0.201092279 0.195845507 0.192117678 0.155809745 0.148901096
#> [25] 0.144035411 0.141782024 0.140643234 0.139279240 0.133001890 0.097694414
#> [31] 0.093411832 0.082770297 0.082474761 0.077066975 0.071188689 0.055335565
#> [37] 0.052168111 0.048025686 0.032245933 0.030982117 0.024566078 0.022993559
#> [43] 0.021493806 0.020089237 0.011099144 0.004507965 1.000000000
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
#> Error in strwidth(legend, units = "user", cex = cex, font = text.font): plot.new has not been called yet
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
#> Error in strwidth(legend, units = "user", cex = cex, font = text.font): plot.new has not been called yet
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
#> 0.5619114
```


```r
top10p.all.cors.micro=all.cors.micro[all.cors.micro>=quantile(all.cors.micro,.90)]
c0seq.top10p.all.cors.micro=quantile(top10p.all.cors.micro,rev(
  seq(0,length(top10p.all.cors.micro),length.out = 50)/495))
c0seq.top10p.all.cors.micro
#>      100% 97.95918% 95.91837% 93.87755% 91.83673% 89.79592%  87.7551% 85.71429% 
#> 0.9485474 0.8399869 0.8107720 0.7938613 0.7856982 0.7667610 0.7574738 0.7451456 
#> 83.67347% 81.63265% 79.59184% 77.55102%  75.5102% 73.46939% 71.42857% 69.38776% 
#> 0.7391484 0.7302308 0.7249281 0.7212886 0.7167389 0.7121513 0.7074834 0.7014723 
#> 67.34694% 65.30612% 63.26531% 61.22449% 59.18367% 57.14286% 55.10204% 53.06122% 
#> 0.6952162 0.6905869 0.6874555 0.6823518 0.6771191 0.6743822 0.6679470 0.6633555 
#> 51.02041% 48.97959% 46.93878% 44.89796% 42.85714% 40.81633% 38.77551% 36.73469% 
#> 0.6596246 0.6548128 0.6494970 0.6396384 0.6324979 0.6260551 0.6225704 0.6199723 
#> 34.69388% 32.65306% 30.61224% 28.57143% 26.53061%  24.4898% 22.44898% 20.40816% 
#> 0.6151346 0.6130546 0.6092452 0.6068366 0.6020911 0.5997397 0.5956504 0.5908566 
#> 18.36735% 16.32653% 14.28571%  12.2449% 10.20408% 8.163265% 6.122449% 4.081633% 
#> 0.5873301 0.5845246 0.5807592 0.5776753 0.5756477 0.5728338 0.5705570 0.5682815 
#> 2.040816%        0% 
#> 0.5655307 0.5619894
```


```r
result.boost.micro_nb1 = fastboost(DATA_exemple3_nb_1$X, DATA_exemple3_nb_1$Y, B=100, 
                                   steps.seq=c0seq.top10p.all.cors.micro)
result.boost.micro_nb1
#>               1    2    3    4    5    6    7    8    9   10   11   12   13   14
#> c0 = 1     1.00 0.00 1.00 1.00 1.00 0.00 1.00 1.00 0.00 0.00 0.00 0.00 1.00 1.00
#> c0 = 0.949 1.00 0.00 1.00 1.00 1.00 0.00 1.00 1.00 0.00 0.00 0.00 0.00 1.00 1.00
#> c0 = 0.84  1.00 0.69 0.37 1.00 1.00 0.23 0.51 0.96 0.07 0.09 0.01 0.00 1.00 0.61
#> c0 = 0.811 1.00 0.73 0.38 0.73 1.00 0.35 0.48 0.93 0.04 0.23 0.00 0.01 1.00 0.25
#> c0 = 0.794 1.00 0.65 0.39 0.78 1.00 0.34 0.53 0.87 0.02 0.19 0.02 0.00 1.00 0.25
#> c0 = 0.786 1.00 0.67 0.33 0.61 1.00 0.41 0.47 0.80 0.03 0.19 0.01 0.18 0.98 0.34
#> c0 = 0.767 1.00 0.68 0.41 0.71 1.00 0.45 0.44 0.82 0.09 0.20 0.03 0.17 0.98 0.29
#> c0 = 0.757 0.99 0.59 0.38 0.75 1.00 0.50 0.43 0.71 0.09 0.30 0.00 0.17 0.96 0.31
#> c0 = 0.745 1.00 0.62 0.41 0.84 1.00 0.48 0.42 0.72 0.09 0.22 0.01 0.13 0.96 0.33
#> c0 = 0.739 1.00 0.56 0.42 0.47 1.00 0.40 0.50 0.77 0.18 0.21 0.23 0.30 0.93 0.29
#>              15   16   17   18   19   20   21   22   23   24   25   26   27   28
#> c0 = 1     0.00 1.00 0.00 0.00 0.00 1.00 0.00 0.00 1.00 0.00 1.00 0.00 0.00 0.00
#> c0 = 0.949 0.00 1.00 0.00 0.00 0.00 0.07 0.00 0.00 1.00 0.00 1.00 0.00 0.00 0.00
#> c0 = 0.84  0.08 0.63 0.07 0.21 0.44 0.47 0.02 0.14 0.05 0.25 0.99 0.11 0.09 0.25
#> c0 = 0.811 0.06 0.54 0.08 0.25 0.31 0.52 0.13 0.24 0.07 0.17 1.00 0.16 0.25 0.22
#> c0 = 0.794 0.09 0.47 0.10 0.36 0.36 0.38 0.15 0.18 0.06 0.27 0.97 0.21 0.19 0.27
#> c0 = 0.786 0.14 0.57 0.14 0.47 0.36 0.40 0.08 0.28 0.21 0.34 0.94 0.20 0.24 0.18
#> c0 = 0.767 0.11 0.37 0.27 0.42 0.30 0.18 0.16 0.22 0.27 0.31 0.88 0.32 0.32 0.27
#> c0 = 0.757 0.10 0.54 0.22 0.46 0.36 0.16 0.12 0.20 0.17 0.28 0.90 0.19 0.24 0.32
#> c0 = 0.745 0.13 0.56 0.22 0.46 0.28 0.16 0.28 0.19 0.22 0.26 0.83 0.21 0.30 0.25
#> c0 = 0.739 0.12 0.62 0.19 0.54 0.36 0.19 0.26 0.25 0.30 0.21 0.72 0.21 0.20 0.31
#>              29   30   31   32   33   34   35   36   37   38   39   40   41   42
#> c0 = 1     1.00 0.00 0.00 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> c0 = 0.949 1.00 0.00 0.00 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> c0 = 0.84  0.88 0.15 0.54 0.10 0.00 0.05 0.23 0.09 0.00 0.57 0.23 0.00 0.45 0.39
#> c0 = 0.811 0.95 0.18 0.07 0.22 0.24 0.07 0.08 0.07 0.05 0.53 0.18 0.00 0.55 0.20
#> c0 = 0.794 0.93 0.37 0.33 0.16 0.13 0.03 0.10 0.16 0.09 0.61 0.34 0.00 0.65 0.23
#> c0 = 0.786 0.90 0.24 0.26 0.16 0.15 0.02 0.15 0.19 0.16 0.54 0.26 0.00 0.55 0.23
#> c0 = 0.767 0.98 0.48 0.23 0.25 0.16 0.23 0.13 0.14 0.15 0.64 0.18 0.01 0.72 0.41
#> c0 = 0.757 0.92 0.27 0.24 0.24 0.27 0.12 0.11 0.10 0.30 0.57 0.26 0.01 0.61 0.30
#> c0 = 0.745 0.91 0.31 0.27 0.31 0.20 0.15 0.12 0.21 0.19 0.57 0.25 0.00 0.63 0.35
#> c0 = 0.739 0.91 0.32 0.20 0.27 0.19 0.22 0.24 0.31 0.22 0.60 0.24 0.02 0.73 0.32
#>              43   44   45   46   47   48   49   50   51   52   53   54   55   56
#> c0 = 1     1.00 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00
#> c0 = 0.949 1.00 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00
#> c0 = 0.84  0.70 0.62 0.71 0.01 0.88 0.00 0.32 0.00 0.85 0.32 0.88 0.31 0.18 0.04
#> c0 = 0.811 0.28 0.41 0.35 0.10 0.60 0.01 0.45 0.00 0.74 0.13 0.97 0.15 0.17 0.05
#> c0 = 0.794 0.20 0.37 0.34 0.10 0.63 0.00 0.51 0.03 0.70 0.13 0.95 0.24 0.21 0.08
#> c0 = 0.786 0.36 0.30 0.28 0.06 0.55 0.27 0.63 0.02 0.67 0.07 0.99 0.32 0.28 0.23
#> c0 = 0.767 0.32 0.27 0.28 0.14 0.55 0.17 0.55 0.07 0.89 0.13 0.29 0.27 0.29 0.28
#> c0 = 0.757 0.43 0.32 0.26 0.16 0.54 0.18 0.16 0.15 0.90 0.10 0.31 0.31 0.30 0.24
#> c0 = 0.745 0.37 0.26 0.31 0.19 0.47 0.35 0.17 0.18 0.77 0.15 0.34 0.40 0.28 0.38
#> c0 = 0.739 0.25 0.24 0.28 0.22 0.54 0.31 0.26 0.20 0.75 0.18 0.33 0.34 0.32 0.46
#>              57   58   59   60   61   62   63   64   65   66   67   68   69   70
#> c0 = 1     0.00 0.00 1.00 0.00 0.00 0.00 1.00 0.00 0.00 0.00 1.00 0.00 0.00 0.00
#> c0 = 0.949 0.00 0.00 1.00 0.00 0.00 0.00 1.00 0.00 0.00 0.00 1.00 0.02 0.00 0.00
#> c0 = 0.84  0.06 0.10 0.96 0.04 0.15 0.17 0.88 0.00 0.31 0.04 0.97 0.09 0.00 0.27
#> c0 = 0.811 0.11 0.67 0.93 0.02 0.02 0.31 0.89 0.71 0.66 0.00 0.98 0.04 0.01 0.13
#> c0 = 0.794 0.08 0.66 0.90 0.02 0.09 0.31 0.71 0.68 0.54 0.01 1.00 0.10 0.00 0.20
#> c0 = 0.786 0.24 0.61 0.82 0.36 0.09 0.23 0.64 0.78 0.62 0.01 0.98 0.12 0.00 0.14
#> c0 = 0.767 0.29 0.66 0.85 0.60 0.08 0.34 0.67 0.70 0.39 0.02 0.98 0.27 0.24 0.11
#> c0 = 0.757 0.24 0.60 0.84 0.60 0.09 0.21 0.71 0.67 0.49 0.37 0.99 0.20 0.18 0.13
#> c0 = 0.745 0.23 0.66 0.86 0.66 0.27 0.36 0.68 0.50 0.51 0.15 0.99 0.25 0.31 0.16
#> c0 = 0.739 0.23 0.52 0.80 0.52 0.17 0.24 0.76 0.67 0.50 0.19 1.00 0.31 0.33 0.06
#>              71   72   73   74   75   76   77   78   79   80   81   82   83   84
#> c0 = 1     1.00 1.00 0.00 1.00 1.00 0.00 1.00 0.00 1.00 0.00 0.00 1.00 0.00 0.00
#> c0 = 0.949 1.00 1.00 0.00 1.00 1.00 0.00 1.00 0.00 1.00 0.00 0.00 1.00 0.00 0.00
#> c0 = 0.84  0.31 0.37 0.10 0.62 0.97 0.01 0.12 0.37 0.55 0.49 0.11 0.67 0.17 0.03
#> c0 = 0.811 0.15 0.34 0.17 0.56 0.98 0.04 0.42 0.39 0.35 0.68 0.26 0.79 0.28 0.50
#> c0 = 0.794 0.17 0.37 0.22 0.52 0.97 0.05 0.46 0.39 0.23 0.64 0.28 0.79 0.29 0.41
#> c0 = 0.786 0.30 0.40 0.17 0.54 0.98 0.15 0.56 0.32 0.24 0.63 0.29 0.67 0.34 0.31
#> c0 = 0.767 0.22 0.47 0.22 0.38 1.00 0.24 0.28 0.36 0.34 0.61 0.35 0.66 0.34 0.38
#> c0 = 0.757 0.22 0.45 0.26 0.50 0.98 0.15 0.32 0.26 0.23 0.56 0.32 0.54 0.31 0.28
#> c0 = 0.745 0.26 0.32 0.27 0.52 0.91 0.25 0.36 0.38 0.15 0.69 0.31 0.55 0.39 0.37
#> c0 = 0.739 0.23 0.35 0.23 0.44 0.96 0.24 0.34 0.30 0.18 0.68 0.33 0.60 0.41 0.37
#>              85   86   87   88   89   90   91   92   93   94   95   96   97   98
#> c0 = 1     1.00 1.00 0.00 0.00 1.00 0.00 1.00 1.00 1.00 0.00 0.00 1.00 1.00 0.00
#> c0 = 0.949 1.00 1.00 0.00 0.00 1.00 0.00 1.00 1.00 1.00 0.00 0.00 1.00 1.00 0.00
#> c0 = 0.84  1.00 0.87 0.11 0.36 0.18 0.00 0.46 0.13 1.00 0.11 0.10 0.92 0.84 0.17
#> c0 = 0.811 1.00 0.70 0.01 0.53 0.01 0.01 0.71 0.21 1.00 0.13 0.26 0.97 0.68 0.15
#> c0 = 0.794 1.00 0.10 0.25 0.66 0.02 0.01 0.54 0.24 1.00 0.16 0.29 0.97 0.71 0.29
#> c0 = 0.786 0.40 0.22 0.39 0.47 0.02 0.00 0.48 0.12 1.00 0.13 0.19 0.90 0.42 0.31
#> c0 = 0.767 0.49 0.29 0.32 0.53 0.00 0.05 0.48 0.20 1.00 0.24 0.20 0.89 0.48 0.27
#> c0 = 0.757 0.49 0.27 0.30 0.50 0.02 0.03 0.47 0.16 1.00 0.24 0.29 0.91 0.47 0.19
#> c0 = 0.745 0.40 0.30 0.34 0.60 0.01 0.05 0.47 0.22 1.00 0.26 0.27 0.89 0.40 0.28
#> c0 = 0.739 0.53 0.17 0.32 0.67 0.58 0.07 0.30 0.16 1.00 0.29 0.22 0.84 0.40 0.33
#>              99  100
#> c0 = 1     0.00 0.00
#> c0 = 0.949 0.00 0.00
#> c0 = 0.84  0.04 0.05
#> c0 = 0.811 0.12 0.02
#> c0 = 0.794 0.09 0.24
#> c0 = 0.786 0.12 0.25
#> c0 = 0.767 0.17 0.30
#> c0 = 0.757 0.20 0.20
#> c0 = 0.745 0.25 0.22
#> c0 = 0.739 0.25 0.33
#>  [ getOption("max.print") est atteint -- 42 lignes omises ]
#> attr(,"c0.seq")
#>  [1] 1.000000 0.948547 0.839987 0.810772 0.793861 0.785698 0.766761 0.757474 0.745146
#> [10] 0.739148 0.730231 0.724928 0.721289 0.716739 0.712151 0.707483 0.701472 0.695216
#> [19] 0.690587 0.687455 0.682352 0.677119 0.674382 0.667947 0.663356 0.659625 0.654813
#> [28] 0.649497 0.639638 0.632498 0.626055 0.622570 0.619972 0.615135 0.613055 0.609245
#> [37] 0.606837 0.602091 0.599740 0.595650 0.590857 0.587330 0.584525 0.580759 0.577675
#> [46] 0.575648 0.572834 0.570557 0.568282 0.565531 0.561989 0.000000
#> attr(,"c0lim")
#> [1] TRUE
#> attr(,"steps.seq")
#>  [1] 0.0000000 0.9485474 0.8399869 0.8107720 0.7938613 0.7856982 0.7667610 0.7574738
#>  [9] 0.7451456 0.7391484 0.7302308 0.7249281 0.7212886 0.7167389 0.7121513 0.7074834
#> [17] 0.7014723 0.6952162 0.6905869 0.6874555 0.6823518 0.6771191 0.6743822 0.6679470
#> [25] 0.6633555 0.6596246 0.6548128 0.6494970 0.6396384 0.6324979 0.6260551 0.6225704
#> [33] 0.6199723 0.6151346 0.6130546 0.6092452 0.6068366 0.6020911 0.5997397 0.5956504
#> [41] 0.5908566 0.5873301 0.5845246 0.5807592 0.5776753 0.5756477 0.5728338 0.5705570
#> [49] 0.5682815 0.5655307 0.5619894 1.0000000
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
#>               1 2    3    4    5 6    7    8 9 10 11 12   13   14 15   16 17 18 19
#>            1.00 0 1.00 1.00 1.00 0 1.00 1.00 0  0  0  0 1.00 1.00  0 1.00  0  0  0
#> c0 = 0.949 1.00 0 1.00 1.00 1.00 0 1.00 1.00 0  0  0  0 1.00 1.00  0 1.00  0  0  0
#> c0 = 0.84  1.00 0 0.37 1.00 1.00 0 0.51 0.96 0  0  0  0 1.00 0.61  0 0.63  0  0  0
#> c0 = 0.811 1.00 0 0.37 0.73 1.00 0 0.48 0.93 0  0  0  0 1.00 0.25  0 0.54  0  0  0
#> c0 = 0.794 1.00 0 0.37 0.73 1.00 0 0.48 0.87 0  0  0  0 1.00 0.25  0 0.47  0  0  0
#> c0 = 0.786 1.00 0 0.31 0.56 1.00 0 0.42 0.80 0  0  0  0 0.98 0.25  0 0.47  0  0  0
#> c0 = 0.767 1.00 0 0.31 0.56 1.00 0 0.39 0.80 0  0  0  0 0.98 0.20  0 0.27  0  0  0
#> c0 = 0.757 0.99 0 0.28 0.56 1.00 0 0.38 0.69 0  0  0  0 0.96 0.20  0 0.27  0  0  0
#> c0 = 0.745 0.99 0 0.28 0.56 1.00 0 0.37 0.69 0  0  0  0 0.96 0.20  0 0.27  0  0  0
#> c0 = 0.739 0.99 0 0.28 0.19 1.00 0 0.37 0.69 0  0  0  0 0.93 0.16  0 0.27  0  0  0
#>              20 21 22   23 24   25 26 27 28           29 30 31 32 33 34   35 36 37
#>            1.00  0  0 1.00  0 1.00  0  0  0 1.000000e+00  0  0  0  0  0 1.00  0  0
#> c0 = 0.949 0.07  0  0 1.00  0 1.00  0  0  0 1.000000e+00  0  0  0  0  0 1.00  0  0
#> c0 = 0.84  0.07  0  0 0.05  0 0.99  0  0  0 8.800000e-01  0  0  0  0  0 0.23  0  0
#> c0 = 0.811 0.07  0  0 0.05  0 0.99  0  0  0 8.800000e-01  0  0  0  0  0 0.08  0  0
#> c0 = 0.794 0.00  0  0 0.04  0 0.96  0  0  0 8.600000e-01  0  0  0  0  0 0.08  0  0
#> c0 = 0.786 0.00  0  0 0.04  0 0.93  0  0  0 8.300000e-01  0  0  0  0  0 0.08  0  0
#> c0 = 0.767 0.00  0  0 0.04  0 0.87  0  0  0 8.300000e-01  0  0  0  0  0 0.06  0  0
#> c0 = 0.757 0.00  0  0 0.00  0 0.87  0  0  0 7.700000e-01  0  0  0  0  0 0.04  0  0
#> c0 = 0.745 0.00  0  0 0.00  0 0.80  0  0  0 7.600000e-01  0  0  0  0  0 0.04  0  0
#> c0 = 0.739 0.00  0  0 0.00  0 0.69  0  0  0 7.600000e-01  0  0  0  0  0 0.04  0  0
#>            38 39 40 41 42   43 44 45 46   47 48 49 50 51   52 53 54 55 56 57 58   59
#>             0  0  0  0  0 1.00  0  0  0 1.00  0  0  0  0 1.00  0  0  0  0  0  0 1.00
#> c0 = 0.949  0  0  0  0  0 1.00  0  0  0 1.00  0  0  0  0 1.00  0  0  0  0  0  0 1.00
#> c0 = 0.84   0  0  0  0  0 0.70  0  0  0 0.88  0  0  0  0 0.32  0  0  0  0  0  0 0.96
#> c0 = 0.811  0  0  0  0  0 0.28  0  0  0 0.60  0  0  0  0 0.13  0  0  0  0  0  0 0.93
#> c0 = 0.794  0  0  0  0  0 0.20  0  0  0 0.60  0  0  0  0 0.13  0  0  0  0  0  0 0.90
#> c0 = 0.786  0  0  0  0  0 0.20  0  0  0 0.52  0  0  0  0 0.07  0  0  0  0  0  0 0.82
#> c0 = 0.767  0  0  0  0  0 0.16  0  0  0 0.52  0  0  0  0 0.07  0  0  0  0  0  0 0.82
#> c0 = 0.757  0  0  0  0  0 0.16  0  0  0 0.51  0  0  0  0 0.04  0  0  0  0  0  0 0.81
#> c0 = 0.745  0  0  0  0  0 0.10  0  0  0 0.44  0  0  0  0 0.04  0  0  0  0  0  0 0.81
#> c0 = 0.739  0  0  0  0  0 0.00  0  0  0 0.44  0  0  0  0 0.04  0  0  0  0  0  0 0.75
#>            60 61 62   63 64 65 66   67 68 69 70   71   72 73   74   75 76   77 78
#>             0  0  0 1.00  0  0  0 1.00  0  0  0 1.00 1.00  0 1.00 1.00  0 1.00  0
#> c0 = 0.949  0  0  0 1.00  0  0  0 1.00  0  0  0 1.00 1.00  0 1.00 1.00  0 1.00  0
#> c0 = 0.84   0  0  0 0.88  0  0  0 0.97  0  0  0 0.31 0.37  0 0.62 0.97  0 0.12  0
#> c0 = 0.811  0  0  0 0.88  0  0  0 0.97  0  0  0 0.15 0.34  0 0.56 0.97  0 0.12  0
#> c0 = 0.794  0  0  0 0.70  0  0  0 0.97  0  0  0 0.15 0.34  0 0.52 0.96  0 0.12  0
#> c0 = 0.786  0  0  0 0.63  0  0  0 0.95  0  0  0 0.15 0.34  0 0.52 0.96  0 0.12  0
#> c0 = 0.767  0  0  0 0.63  0  0  0 0.95  0  0  0 0.07 0.34  0 0.36 0.96  0 0.00  0
#> c0 = 0.757  0  0  0 0.63  0  0  0 0.95  0  0  0 0.07 0.32  0 0.36 0.94  0 0.00  0
#> c0 = 0.745  0  0  0 0.60  0  0  0 0.95  0  0  0 0.07 0.19  0 0.36 0.87  0 0.00  0
#> c0 = 0.739  0  0  0 0.60  0  0  0 0.95  0  0  0 0.04 0.19  0 0.28 0.87  0 0.00  0
#>              79 80 81           82 83 84   85   86 87 88   89 90   91   92   93 94
#>            1.00  0  0 1.000000e+00  0  0 1.00 1.00  0  0 1.00  0 1.00 1.00 1.00  0
#> c0 = 0.949 1.00  0  0 1.000000e+00  0  0 1.00 1.00  0  0 1.00  0 1.00 1.00 1.00  0
#> c0 = 0.84  0.55  0  0 6.700000e-01  0  0 1.00 0.87  0  0 0.18  0 0.46 0.13 1.00  0
#> c0 = 0.811 0.35  0  0 6.700000e-01  0  0 1.00 0.70  0  0 0.01  0 0.46 0.13 1.00  0
#> c0 = 0.794 0.23  0  0 6.700000e-01  0  0 1.00 0.10  0  0 0.01  0 0.29 0.13 1.00  0
#> c0 = 0.786 0.23  0  0 5.500000e-01  0  0 0.40 0.10  0  0 0.01  0 0.23 0.01 1.00  0
#> c0 = 0.767 0.23  0  0 5.400000e-01  0  0 0.40 0.10  0  0 0.00  0 0.23 0.01 1.00  0
#> c0 = 0.757 0.12  0  0 4.200000e-01  0  0 0.40 0.08  0  0 0.00  0 0.22 0.00 1.00  0
#> c0 = 0.745 0.04  0  0 4.200000e-01  0  0 0.31 0.08  0  0 0.00  0 0.22 0.00 1.00  0
#> c0 = 0.739 0.04  0  0 4.200000e-01  0  0 0.31 0.00  0  0 0.00  0 0.05 0.00 1.00  0
#>            95   96   97 98 99 100
#>             0 1.00 1.00  0  0   0
#> c0 = 0.949  0 1.00 1.00  0  0   0
#> c0 = 0.84   0 0.92 0.84  0  0   0
#> c0 = 0.811  0 0.92 0.68  0  0   0
#> c0 = 0.794  0 0.92 0.68  0  0   0
#> c0 = 0.786  0 0.85 0.39  0  0   0
#> c0 = 0.767  0 0.84 0.39  0  0   0
#> c0 = 0.757  0 0.84 0.38  0  0   0
#> c0 = 0.745  0 0.82 0.31  0  0   0
#> c0 = 0.739  0 0.77 0.31  0  0   0
#>  [ getOption("max.print") est atteint -- 42 lignes omises ]
```

#### Confidence indices.

First compute the highest $c_0$ value for which the proportion of selection is under the threshold $thr$. In that analysis, we set $thr=1$.

```r
thr=1
index.last.c0.micro_nb1=apply(dec.result.boost.micro_nb1>=thr,2,which.min)-1
index.last.c0.micro_nb1
#>   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21 
#>   7   0   2   3  51   0   2   2   0   0   0   0   5   2   0   2   0   0   0   1   0 
#>  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42 
#>   0   2   0   2   0   0   0   2   0   0   0   0   0   2   0   0   0   0   0   0   0 
#>  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63 
#>   2   0   0   0   2   0   0   0   0   2   0   0   0   0   0   0   2   0   0   0   2 
#>  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84 
#>   0   0   0   2   0   0   0   2   2   0   2   2   0   2   0   2   0   0   2   0   0 
#>  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 
#>   5   2   0   0   2   0   2   2  11   0   0   2   2   0   0   0
```

We have to cap the confidence index value to the $1-\{\textrm{smallest } c_0\}$ that we specified in the $c_0$ sequence and that was actually used for resampling. As a consequence, we have to exclude the $c_0=0$ case since we do not know what happen between $c0=\verb+quantile+(cors,.9)$ and $c_0=0$.


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
#>  [1] "c0 = 0.767" "c0 = 0.949" "c0 = 0.84"  "c0 = 0.562" "c0 = 0.949" "c0 = 0.949"
#>  [7] "c0 = 0.794" "c0 = 0.949" "c0 = 0.949" ""           "c0 = 0.949" "c0 = 0.949"
#> [13] "c0 = 0.949" "c0 = 0.949" "c0 = 0.949" "c0 = 0.949" "c0 = 0.949" "c0 = 0.949"
#> [19] "c0 = 0.949" "c0 = 0.949" "c0 = 0.949" "c0 = 0.949" "c0 = 0.949" "c0 = 0.949"
#> [25] "c0 = 0.949" "c0 = 0.949" "c0 = 0.949" "c0 = 0.794" "c0 = 0.949" "c0 = 0.949"
#> [31] "c0 = 0.949" "c0 = 0.949" "c0 = 0.73"  "c0 = 0.949" "c0 = 0.949"
attr(result.boost.micro_nb1,"c0.seq")[index.last.c0.micro_nb1]
#>  [1] 0.766761 0.948547 0.839987 0.561989 0.948547 0.948547 0.793861 0.948547 0.948547
#> [10] 1.000000 0.948547 0.948547 0.948547 0.948547 0.948547 0.948547 0.948547 0.948547
#> [19] 0.948547 0.948547 0.948547 0.948547 0.948547 0.948547 0.948547 0.948547 0.948547
#> [28] 0.793861 0.948547 0.948547 0.948547 0.948547 0.730231 0.948547 0.948547
confidence.indices.micro_nb1 = c(0,1-attr(result.boost.micro_nb1,"c0.seq"))[
  index.last.c0.micro_nb1+1]
confidence.indices.micro_nb1
#>   [1] 0.233239 0.000000 0.051453 0.160013 0.438011 0.000000 0.051453 0.051453
#>   [9] 0.000000 0.000000 0.000000 0.000000 0.206139 0.051453 0.000000 0.051453
#>  [17] 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.051453 0.000000
#>  [25] 0.051453 0.000000 0.000000 0.000000 0.051453 0.000000 0.000000 0.000000
#>  [33] 0.000000 0.000000 0.051453 0.000000 0.000000 0.000000 0.000000 0.000000
#>  [41] 0.000000 0.000000 0.051453 0.000000 0.000000 0.000000 0.051453 0.000000
#>  [49] 0.000000 0.000000 0.000000 0.051453 0.000000 0.000000 0.000000 0.000000
#>  [57] 0.000000 0.000000 0.051453 0.000000 0.000000 0.000000 0.051453 0.000000
#>  [65] 0.000000 0.000000 0.051453 0.000000 0.000000 0.000000 0.051453 0.051453
#>  [73] 0.000000 0.051453 0.051453 0.000000 0.051453 0.000000 0.051453 0.000000
#>  [81] 0.000000 0.051453 0.000000 0.000000 0.206139 0.051453 0.000000 0.000000
#>  [89] 0.051453 0.000000 0.051453 0.051453 0.269769 0.000000 0.000000 0.051453
#>  [97] 0.051453 0.000000 0.000000 0.000000
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

