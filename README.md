---
title: "A General Algorithm to Enhance the Performance of Variable Selection Methods in Correlated Datasets"
author: "Frédéric Bertrand and Myriam Maumy-Bertrand"
output: github_document
---

[![CRAN status](https://www.r-pkg.org/badges/version/SelectBoost)](https://cran.r-project.org/package=SelectBoost)

<!-- README.md is generated from README.Rmd. Please edit that file -->


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
#>            [,1]       [,2]       [,3]       [,4]        [,5]       [,6]
#> [1,] -0.4404975  0.2636575 -0.1432384 -0.3237501 -0.86453870 -1.1129293
#> [2,]  1.3100210  0.3129508  0.4259410 -1.4395966  0.08573403  1.3616465
#> [3,]  1.4544963  0.4409880  1.6711281  1.4614908  1.18517732  0.1683203
#> [4,]  0.6065767 -0.9252151  1.7261698 -0.5665939  1.25573736 -0.0109570
#> [5,] -1.6831288 -0.6629509 -0.4589231  0.3756388 -0.92236981  1.0977587
#> [6,]  0.5748739 -0.6442761  1.1428925 -0.3851747  1.35538380  0.5397800
#>            [,7]       [,8]       [,9]      [,10]
#> [1,] -0.9361019  0.1575435 -0.3902457 -0.7618517
#> [2,]  0.7508037 -1.6770836  0.9187821  0.7029273
#> [3,]  2.0724515  1.7497669  1.7702351  0.7191604
#> [4,]  1.3540557 -0.7647179  1.3293046 -1.8133875
#> [5,] -0.5238724 -0.6315983 -1.0669595  0.3247423
#> [6,]  1.4423592 -0.9422312  1.0906825 -0.9576496
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
#>  $ X      : num [1:100, 1:10] -0.44 1.31 1.454 0.607 -1.683 ...
#>  $ Y      : num [1:100] -0.266 2.473 1.682 -2.036 -3.667 ...
#>  $ support: num [1:10] 1 1 1 0 0 0 0 0 0 0
#>  $ beta   : num [1:10] 1.85 1.53 -1.01 0 0 ...
#>  $ stn    : num 500
#>  $ sigma  : num [1, 1] 0.0861
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
#>          0%         10%         20%         30%         40%         50% 
#> 0.008392426 0.047271362 0.071403879 0.093203097 0.141174590 0.176774811 
#>         60%         70%         80%         90%        100% 
#> 0.318255536 0.475232618 0.719014877 0.763721310 0.786646568
```


```r
result.boost = fastboost(DATA_exemple$X, DATA_exemple$Y)
result.boost
#>               1    2    3    4    5    6    7    8    9   10
#> c0 = 1     1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0.00 1.00
#> c0 = 0.787 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0.00 1.00
#> c0 = 0.759 1.00 1.00 0.98 1.00 0.52 0.97 0.71 0.88 0.47 0.90
#> c0 = 0.719 0.86 1.00 0.64 1.00 0.37 0.84 0.68 0.48 0.68 0.45
#> c0 = 0.471 0.60 1.00 0.60 0.36 0.68 0.22 0.69 0.23 0.69 0.38
#> c0 = 0.318 0.48 1.00 0.51 0.34 0.56 0.45 0.77 0.79 0.72 0.90
#> c0 = 0.177 0.66 0.81 0.60 0.70 0.64 0.71 0.78 0.76 0.68 0.87
#> c0 = 0.141 0.56 0.80 0.67 0.75 0.58 0.63 0.80 0.66 0.71 0.94
#> c0 = 0.098 0.54 0.66 0.71 0.70 0.55 0.63 0.83 0.70 0.69 0.95
#> c0 = 0.071 0.34 0.57 0.72 0.65 0.76 0.62 0.96 0.64 0.67 0.91
#> c0 = 0.054 0.82 0.70 0.69 0.63 0.81 0.58 0.85 0.67 0.70 0.81
#> c0 = 0.008 0.76 0.68 0.68 0.61 0.79 0.65 0.79 0.70 0.71 0.80
#> c0 = 0     0.77 0.68 0.70 0.62 0.79 0.65 0.79 0.68 0.71 0.77
#> attr(,"c0.seq")
#>              100%      90%      80%      70%      60%      50%      40% 
#> 1.000000 0.786647 0.759414 0.719015 0.471485 0.318256 0.176775 0.141175 
#>      30%      20%      10%       0%          
#> 0.097882 0.071404 0.054014 0.008392 0.000000 
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

#### Comparing true and selected predictors

We can compute, for all the $c_0$ values and for a selection threshold varying from $1$ to $0.5$ by $0.05$ steps, the sensitivity (precision), the specificity (recall), as well as several Fscores ($F_1$ harmonic mean of precision and recall, $F_{1/2}$ and $F_2$ two weighted harmonic means of precision and recall).

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
legend(x="topright",legend=c("sensitivity (precision)",
      "specificity (recall)","non-weighted Fscore",
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
result.boost.45 = fastboost(DATA_exemple$X, DATA_exemple$Y, B=100,
                    steps.seq=sort(unique(all.cors),decreasing=TRUE))
result.boost.45
#>               1    2    3    4    5    6    7    8    9   10
#> c0 = 1     1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0.00 1.00
#> c0 = 0.787 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0.00 1.00
#> c0 = 0.78  1.00 1.00 1.00 1.00 0.25 1.00 0.25 1.00 0.00 1.00
#> c0 = 0.779 1.00 1.00 1.00 1.00 0.36 0.92 0.96 0.98 0.54 0.98
#> c0 = 0.768 1.00 1.00 1.00 1.00 0.36 0.94 0.96 1.00 0.50 0.99
#> c0 = 0.764 1.00 1.00 0.99 1.00 0.55 0.98 0.72 0.88 0.51 0.91
#> c0 = 0.753 0.92 1.00 0.76 1.00 0.45 0.74 0.66 0.54 0.41 0.18
#> c0 = 0.741 0.92 1.00 0.75 1.00 0.44 0.77 0.65 0.54 0.41 0.23
#> c0 = 0.729 0.96 1.00 0.76 1.00 0.34 0.84 0.74 0.62 0.37 0.39
#> c0 = 0.727 0.87 1.00 0.66 1.00 0.36 0.84 0.64 0.47 0.68 0.43
#> c0 = 0.717 0.86 1.00 0.66 1.00 0.35 0.84 0.63 0.48 0.64 0.44
#> c0 = 0.49  0.65 1.00 0.63 0.48 0.65 0.40 0.72 0.56 0.55 0.32
#> c0 = 0.486 0.64 1.00 0.62 0.48 0.65 0.35 0.72 0.56 0.54 0.32
#> c0 = 0.479 0.60 1.00 0.56 0.41 0.68 0.19 0.69 0.26 0.64 0.66
#> c0 = 0.475 0.57 1.00 0.63 0.36 0.68 0.20 0.69 0.24 0.66 0.66
#> c0 = 0.456 0.60 1.00 0.59 0.28 0.69 0.50 0.67 0.26 0.68 0.41
#> c0 = 0.425 0.59 1.00 0.62 0.26 0.70 0.46 0.67 0.26 0.67 0.40
#> c0 = 0.423 0.62 1.00 0.64 0.31 0.51 0.28 0.67 0.74 0.69 0.44
#> c0 = 0.347 0.46 1.00 0.52 0.28 0.54 0.47 0.80 0.83 0.69 0.90
#> c0 = 0.299 0.50 1.00 0.53 0.30 0.56 0.46 0.78 0.84 0.68 0.88
#> c0 = 0.245 0.49 0.81 0.55 0.75 0.60 0.74 0.65 0.75 0.78 0.86
#> c0 = 0.199 0.51 0.81 0.53 0.75 0.59 0.74 0.63 0.73 0.75 0.87
#> c0 = 0.195 0.52 0.81 0.55 0.69 0.57 0.74 0.67 0.74 0.78 0.89
#> c0 = 0.177 0.65 0.76 0.64 0.69 0.64 0.69 0.77 0.78 0.67 0.88
#> c0 = 0.175 0.63 0.81 0.60 0.69 0.66 0.68 0.72 0.70 0.69 0.88
#> c0 = 0.16  0.51 0.82 0.68 0.77 0.64 0.71 0.78 0.67 0.74 0.89
#> c0 = 0.148 0.54 0.82 0.69 0.76 0.61 0.66 0.79 0.64 0.74 0.94
#> c0 = 0.147 0.53 0.80 0.69 0.76 0.61 0.66 0.77 0.62 0.75 0.94
#> c0 = 0.132 0.59 0.82 0.60 0.70 0.63 0.63 0.86 0.65 0.69 0.94
#> c0 = 0.131 0.58 0.82 0.62 0.63 0.64 0.69 0.89 0.67 0.70 0.92
#> c0 = 0.13  0.55 0.78 0.61 0.72 0.63 0.66 0.85 0.64 0.69 0.94
#> c0 = 0.117 0.57 0.75 0.57 0.70 0.64 0.59 0.91 0.66 0.68 0.96
#> c0 = 0.093 0.52 0.63 0.74 0.68 0.82 0.60 0.78 0.68 0.69 0.96
#> c0 = 0.092 0.50 0.63 0.71 0.73 0.80 0.65 0.79 0.68 0.73 0.90
#> c0 = 0.085 0.43 0.59 0.69 0.65 0.74 0.57 0.97 0.71 0.68 0.90
#> c0 = 0.078 0.30 0.56 0.68 0.61 0.72 0.62 0.96 0.70 0.68 0.92
#> c0 = 0.072 0.29 0.54 0.70 0.62 0.76 0.63 0.95 0.67 0.68 0.89
#> c0 = 0.07  0.39 0.62 0.73 0.66 0.76 0.63 0.89 0.63 0.71 0.91
#> c0 = 0.068 0.80 0.67 0.68 0.64 0.78 0.61 0.87 0.61 0.70 0.90
#> c0 = 0.067 0.81 0.69 0.72 0.63 0.79 0.63 0.88 0.64 0.70 0.81
#> c0 = 0.064 0.81 0.64 0.70 0.65 0.79 0.59 0.87 0.65 0.70 0.82
#> c0 = 0.047 0.84 0.69 0.71 0.59 0.80 0.64 0.83 0.71 0.71 0.83
#> c0 = 0.029 0.79 0.68 0.72 0.62 0.78 0.67 0.82 0.69 0.71 0.85
#> c0 = 0.026 0.86 0.66 0.70 0.61 0.81 0.62 0.84 0.69 0.68 0.79
#> c0 = 0.017 0.84 0.67 0.75 0.59 0.77 0.63 0.77 0.71 0.70 0.80
#> c0 = 0.008 0.74 0.69 0.68 0.63 0.78 0.64 0.77 0.66 0.73 0.79
#> c0 = 0     0.75 0.71 0.68 0.62 0.80 0.65 0.81 0.69 0.72 0.79
#> attr(,"c0.seq")
#>  [1] 1.000000 0.786647 0.779872 0.779447 0.768247 0.763721 0.752952
#>  [8] 0.741361 0.729431 0.727298 0.716944 0.489844 0.486041 0.478898
#> [15] 0.475233 0.456496 0.425445 0.423183 0.346901 0.299159 0.244631
#> [22] 0.198694 0.195465 0.176775 0.175339 0.159530 0.148088 0.147035
#> [29] 0.132384 0.130839 0.129596 0.116597 0.093203 0.092268 0.085306
#> [36] 0.077545 0.071655 0.070400 0.067684 0.066694 0.064129 0.047271
#> [43] 0.029217 0.026028 0.017489 0.008392 0.000000
#> attr(,"steps.seq")
#>  [1] 0.000000000 0.786646568 0.779871626 0.779447120 0.768246931
#>  [6] 0.763721310 0.752952200 0.741360567 0.729431346 0.727298486
#> [11] 0.716943975 0.489844080 0.486040541 0.478898494 0.475232618
#> [16] 0.456496188 0.425444925 0.423182770 0.346901014 0.299158551
#> [21] 0.244631272 0.198693739 0.195464804 0.176774811 0.175338744
#> [26] 0.159529717 0.148088347 0.147034822 0.132384242 0.130838956
#> [31] 0.129595802 0.116596508 0.093203097 0.092268257 0.085306098
#> [36] 0.077545193 0.071654738 0.070400441 0.067684309 0.066694117
#> [41] 0.064128550 0.047271362 0.029217084 0.026027766 0.017488965
#> [46] 0.008392426 1.000000000
#> attr(,"typeboost")
#> [1] "fastboost"
#> attr(,"limi_alea")
#> [1] NA
#> attr(,"B")
#> [1] 100
#> attr(,"class")
#> [1] "selectboost"
```

#### Comparing true and selected predictors
Due to the effect of the correlated resampling, the proportion of selection for a variable may increase, especially if it is a variable that is often discarded. Hence, one should force those proportions of selection to be non-increasing. It is one of the results of the $summary$ function for the $selectboost$ class.


```r
dec.result.boost.45 <- summary(result.boost.45)$selectboost_result.dec
dec.result.boost.45
#>               1    2    3    4    5    6    7    8 9   10
#>            1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0 1.00
#> c0 = 0.787 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0 1.00
#> c0 = 0.78  1.00 1.00 1.00 1.00 0.26 1.00 0.24 1.00 0 1.00
#> c0 = 0.779 1.00 1.00 1.00 1.00 0.26 0.93 0.24 1.00 0 0.97
#> c0 = 0.768 1.00 1.00 1.00 1.00 0.26 0.93 0.23 0.98 0 0.97
#> c0 = 0.764 1.00 1.00 0.99 1.00 0.26 0.93 0.01 0.88 0 0.90
#> c0 = 0.753 0.92 1.00 0.75 1.00 0.18 0.74 0.00 0.51 0 0.16
#> c0 = 0.741 0.90 1.00 0.75 1.00 0.18 0.74 0.00 0.51 0 0.16
#> c0 = 0.729 0.90 1.00 0.75 1.00 0.11 0.74 0.00 0.51 0 0.16
#> c0 = 0.727 0.80 1.00 0.66 1.00 0.11 0.74 0.00 0.38 0 0.16
#> c0 = 0.717 0.80 1.00 0.64 1.00 0.09 0.73 0.00 0.35 0 0.16
#> c0 = 0.49  0.60 1.00 0.63 0.49 0.09 0.28 0.00 0.35 0 0.02
#> c0 = 0.486 0.60 1.00 0.61 0.49 0.09 0.26 0.00 0.35 0 0.02
#> c0 = 0.479 0.51 1.00 0.59 0.41 0.09 0.10 0.00 0.00 0 0.02
#> c0 = 0.475 0.51 1.00 0.59 0.39 0.09 0.10 0.00 0.00 0 0.02
#> c0 = 0.456 0.51 1.00 0.57 0.27 0.08 0.10 0.00 0.00 0 0.00
#> c0 = 0.425 0.51 1.00 0.56 0.27 0.08 0.08 0.00 0.00 0 0.00
#> c0 = 0.423 0.51 1.00 0.56 0.27 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.347 0.36 1.00 0.43 0.25 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.299 0.35 1.00 0.43 0.25 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.245 0.35 0.78 0.42 0.25 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.199 0.34 0.78 0.42 0.25 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.195 0.34 0.74 0.42 0.19 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.177 0.34 0.74 0.42 0.19 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.175 0.32 0.74 0.37 0.17 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.16  0.22 0.74 0.37 0.17 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.148 0.22 0.74 0.37 0.17 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.147 0.22 0.74 0.35 0.16 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.132 0.22 0.72 0.29 0.10 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.131 0.22 0.72 0.29 0.07 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.13  0.21 0.68 0.27 0.07 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.117 0.19 0.64 0.26 0.07 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.093 0.14 0.55 0.26 0.02 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.092 0.11 0.55 0.26 0.02 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.085 0.06 0.48 0.24 0.00 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.078 0.00 0.46 0.21 0.00 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.072 0.00 0.44 0.21 0.00 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.07  0.00 0.44 0.21 0.00 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.068 0.00 0.44 0.18 0.00 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.067 0.00 0.44 0.18 0.00 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.064 0.00 0.42 0.18 0.00 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.047 0.00 0.37 0.17 0.00 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.029 0.00 0.37 0.17 0.00 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.026 0.00 0.37 0.16 0.00 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.017 0.00 0.36 0.13 0.00 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0.008 0.00 0.36 0.13 0.00 0.00 0.00 0.00 0.00 0 0.00
#> c0 = 0     0.00 0.36 0.13 0.00 0.00 0.00 0.00 0.00 0 0.00
```

Let's compute again, for all the $c_0$ values, the sensitivity (precision), specificity (recall), and several Fscores ($F_1$ harmonic mean of precision and recall, $F_{1/2}$ and $F_2$ two weighted harmonic means of precision and recall).

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
```

For a selection threshold equal to $0.90$, all the $c_0$ values and the 5 criteria.

```r
matplot(1:nrow(dec.result.boost.45),All_res.45[,,3],type="l",
        ylab="criterion value",xlab="c0 value",xaxt="n",lwd=2)
axis(1, at=1:length(attr(result.boost.45,"c0.seq")), 
     labels=round(attr(result.boost.45,"c0.seq"),3))
legend(x="topright",legend=c("sensitivity (precision)",
       "specificity (recall)","non-weighted Fscore",
       "F1/2 weighted Fscore","F2 weighted Fscore"),
       lty=1:5,col=1:5,lwd=2)
```

<img src="man/figures/README-datasetsimulation6res45-1.png" title="plot of chunk datasetsimulation6res45" alt="plot of chunk datasetsimulation6res45" width="100%" />

Fscores for all selection thresholds and all the $c_0$ values.

```r
matplot(1:nrow(dec.result.boost.45),All_res.45[,3,],type="l",
        ylab="Fscore",xlab="c0 value",xaxt="n",lwd=2,col=1:11,lty=1:11)
axis(1, at=1:length(attr(result.boost.45,"c0.seq")), 
     labels=round(attr(result.boost.45,"c0.seq"),3))
legend(x="topright",legend=(20:11)/20,lty=1:11,col=1:11,lwd=2,
       title="Threshold")
```

<img src="man/figures/README-datasetsimulation7res45-1.png" title="plot of chunk datasetsimulation7res45" alt="plot of chunk datasetsimulation7res45" width="100%" />

#### Confidence indices.

First compute the highest $c_0$ value for which the proportion of selection is under the threshold $thr$. In that analysis, we set $thr=1$.

```r
thr=1
index.last.c0=apply(dec.result.boost.45>=thr,2,which.min)-1
index.last.c0
#>  1  2  3  4  5  6  7  8  9 10 
#>  6 20  5 11  2  3  2  4  0  3
```

Define some colorRamp ranging from blue (high confidence) to red (low confidence).

```r
jet.colors <-
  colorRamp(rev(c(
  "blue", "#007FFF", "#FF7F00", "red", "#7F0000")))
```


```r
rownames(dec.result.boost.45)[index.last.c0]
#> [1] "c0 = 0.764" "c0 = 0.299" "c0 = 0.768" "c0 = 0.717" "c0 = 0.787"
#> [6] "c0 = 0.78"  "c0 = 0.787" "c0 = 0.779" "c0 = 0.78"
attr(result.boost.45,"c0.seq")[index.last.c0]
#> [1] 0.763721 0.299159 0.768247 0.716944 0.786647 0.779872 0.786647 0.779447
#> [9] 0.779872
confidence.indices = c(0,1-attr(result.boost.45,"c0.seq"))[index.last.c0+1]
confidence.indices
#>  [1] 0.236279 0.700841 0.231753 0.283056 0.213353 0.220128 0.213353
#>  [8] 0.220553 0.000000 0.220128
barplot(confidence.indices,col=rgb(jet.colors(confidence.indices), maxColorValue = 255), 
        names.arg=colnames(result.boost.45), ylim=c(0,1))
```

<img src="man/figures/README-datasetsimulation9res45-1.png" title="plot of chunk datasetsimulation9res45" alt="plot of chunk datasetsimulation9res45" width="100%" />

First compute the highest $c_0$ value for which the proportion of selection is under the threshold $thr$. In that analysis, we set $thr=1$.

```r
thr=.9
index.last.c0=apply(dec.result.boost.45>=thr,2,which.min)-1
index.last.c0
#>  1  2  3  4  5  6  7  8  9 10 
#>  9 20  6 11  2  6  2  5  0  6
```


```r
rownames(dec.result.boost.45)[index.last.c0]
#> [1] "c0 = 0.729" "c0 = 0.299" "c0 = 0.764" "c0 = 0.717" "c0 = 0.787"
#> [6] "c0 = 0.764" "c0 = 0.787" "c0 = 0.768" "c0 = 0.764"
attr(result.boost.45,"c0.seq")[index.last.c0]
#> [1] 0.729431 0.299159 0.763721 0.716944 0.786647 0.763721 0.786647 0.768247
#> [9] 0.763721
confidence.indices = c(0,1-attr(result.boost.45,"c0.seq"))[index.last.c0+1]
confidence.indices
#>  [1] 0.270569 0.700841 0.236279 0.283056 0.213353 0.236279 0.213353
#>  [8] 0.231753 0.000000 0.236279
barplot(confidence.indices,col=rgb(jet.colors(confidence.indices), maxColorValue = 255), 
        names.arg=colnames(result.boost.45), ylim=c(0,1))
```

<img src="man/figures/README-datasetsimulation9res45bisbis-1.png" title="plot of chunk datasetsimulation9res45bisbis" alt="plot of chunk datasetsimulation9res45bisbis" width="100%" />


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
data(micro_S)
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
#> 0.5946165
```


```r
top10p.all.cors.micro=all.cors.micro[all.cors.micro>=quantile(all.cors.micro,.90)]
c0seq.top10p.all.cors.micro=quantile(top10p.all.cors.micro,rev(
  seq(0,length(top10p.all.cors.micro),length.out = 50)/495))
c0seq.top10p.all.cors.micro
#>      100% 97.95918% 95.91837% 93.87755% 91.83673% 89.79592%  87.7551% 
#> 0.9324777 0.8817000 0.8516759 0.8322181 0.8176826 0.8037348 0.7974261 
#> 85.71429% 83.67347% 81.63265% 79.59184% 77.55102%  75.5102% 73.46939% 
#> 0.7845523 0.7773261 0.7637356 0.7560691 0.7492756 0.7429626 0.7354952 
#> 71.42857% 69.38776% 67.34694% 65.30612% 63.26531% 61.22449% 59.18367% 
#> 0.7291972 0.7261506 0.7216514 0.7163867 0.7110924 0.7063313 0.6996139 
#> 57.14286% 55.10204% 53.06122% 51.02041% 48.97959% 46.93878% 44.89796% 
#> 0.6938668 0.6895966 0.6849656 0.6811116 0.6792702 0.6753463 0.6714343 
#> 42.85714% 40.81633% 38.77551% 36.73469% 34.69388% 32.65306% 30.61224% 
#> 0.6680970 0.6640128 0.6590942 0.6549434 0.6512180 0.6476016 0.6458247 
#> 28.57143% 26.53061%  24.4898% 22.44898% 20.40816% 18.36735% 16.32653% 
#> 0.6415746 0.6388103 0.6360603 0.6309453 0.6290600 0.6249131 0.6219063 
#> 14.28571%  12.2449% 10.20408% 8.163265% 6.122449% 4.081633% 2.040816% 
#> 0.6184595 0.6155353 0.6134627 0.6098312 0.6052737 0.6019864 0.5998630 
#>        0% 
#> 0.5947247
```


```r
result.boost.micro_nb1 = fastboost(DATA_exemple3_nb_1$X, DATA_exemple3_nb_1$Y, B=100, 
                                   steps.seq=c0seq.top10p.all.cors.micro)
result.boost.micro_nb1
#>               1    2    3    4    5    6    7    8    9   10   11   12
#> c0 = 1     1.00 1.00 1.00 1.00 1.00 1.00 0.00 0.00 0.00 0.00 0.00 1.00
#> c0 = 0.932 1.00 1.00 1.00 1.00 1.00 1.00 0.00 0.00 0.00 0.00 0.00 1.00
#> c0 = 0.882 0.45 1.00 1.00 0.92 1.00 0.88 0.28 0.07 0.38 0.14 0.01 0.67
#> c0 = 0.852 0.78 1.00 0.74 0.54 1.00 0.95 0.29 0.03 0.10 0.14 0.00 0.24
#> c0 = 0.832 0.58 1.00 0.72 0.84 1.00 0.81 0.25 0.00 0.16 0.05 0.00 0.48
#> c0 = 0.818 0.61 1.00 0.83 0.49 1.00 0.70 0.24 0.00 0.33 0.08 0.00 0.48
#> c0 = 0.804 0.46 1.00 0.80 0.52 1.00 0.64 0.18 0.00 0.18 0.15 0.13 0.56
#> c0 = 0.797 0.53 1.00 0.62 0.54 1.00 0.69 0.16 0.01 0.23 0.10 0.06 0.52
#> c0 = 0.785 0.56 1.00 0.63 0.60 1.00 0.44 0.15 0.00 0.21 0.20 0.04 0.30
#> c0 = 0.777 0.51 1.00 0.44 0.63 0.99 0.48 0.23 0.03 0.19 0.20 0.13 0.38
#>              13   14   15   16   17   18   19   20   21   22   23   24
#> c0 = 1     0.00 0.00 1.00 1.00 0.00 0.00 1.00 0.00 0.00 1.00 0.00 1.00
#> c0 = 0.932 0.00 0.00 1.00 1.00 0.00 0.00 1.00 0.00 0.00 1.00 0.00 1.00
#> c0 = 0.882 0.02 0.23 0.76 0.65 0.11 0.01 0.81 0.35 0.17 0.52 0.30 0.07
#> c0 = 0.852 0.00 0.09 0.64 0.40 0.10 0.01 0.85 0.21 0.10 0.39 0.25 0.14
#> c0 = 0.832 0.01 0.36 0.62 0.35 0.07 0.14 1.00 0.51 0.21 0.36 0.46 0.16
#> c0 = 0.818 0.00 0.30 0.63 0.43 0.09 0.06 0.97 0.53 0.18 0.15 0.15 0.16
#> c0 = 0.804 0.14 0.36 0.64 0.48 0.07 0.09 0.94 0.43 0.23 0.41 0.31 0.12
#> c0 = 0.797 0.18 0.34 0.60 0.43 0.10 0.18 0.86 0.30 0.24 0.17 0.27 0.26
#> c0 = 0.785 0.20 0.23 0.64 0.32 0.07 0.15 0.88 0.23 0.34 0.54 0.11 0.20
#> c0 = 0.777 0.34 0.24 0.58 0.45 0.07 0.12 0.82 0.28 0.20 0.62 0.27 0.18
#>              25   26   27   28   29   30   31   32   33   34   35   36
#> c0 = 1     0.00 0.00 1.00 0.00 0.00 0.00 1.00 1.00 0.00 0.00 1.00 0.00
#> c0 = 0.932 0.00 0.00 1.00 0.00 0.00 0.00 1.00 1.00 0.00 0.00 1.00 0.00
#> c0 = 0.882 0.11 0.62 0.40 0.15 0.27 0.35 0.40 0.54 0.15 0.17 0.25 0.16
#> c0 = 0.852 0.05 0.43 0.28 0.19 0.57 0.44 0.23 0.56 0.05 0.21 0.15 0.06
#> c0 = 0.832 0.08 0.29 0.66 0.32 0.48 0.31 0.15 0.45 0.01 0.20 0.34 0.16
#> c0 = 0.818 0.24 0.36 0.79 0.45 0.70 0.22 0.15 0.27 0.02 0.09 0.30 0.16
#> c0 = 0.804 0.14 0.46 0.80 0.31 0.64 0.08 0.30 0.49 0.01 0.14 0.28 0.11
#> c0 = 0.797 0.28 0.45 0.83 0.35 0.67 0.17 0.11 0.37 0.08 0.13 0.11 0.17
#> c0 = 0.785 0.15 0.44 0.89 0.35 0.63 0.21 0.13 0.46 0.01 0.18 0.21 0.18
#> c0 = 0.777 0.15 0.45 0.72 0.43 0.67 0.21 0.20 0.37 0.03 0.07 0.23 0.09
#>              37   38   39   40   41   42   43   44   45   46   47   48
#> c0 = 1     0.00 0.00 1.00 0.00 0.00 1.00 0.00 1.00 0.00 0.00 1.00 1.00
#> c0 = 0.932 0.00 0.00 1.00 0.00 0.00 1.00 0.00 1.00 0.00 0.00 1.00 1.00
#> c0 = 0.882 0.27 0.20 0.30 0.08 0.04 0.44 0.04 0.58 0.01 0.39 0.26 0.60
#> c0 = 0.852 0.29 0.10 0.14 0.08 0.02 0.59 0.13 0.53 0.00 0.20 0.65 0.35
#> c0 = 0.832 0.16 0.28 0.10 0.12 0.02 0.50 0.00 0.74 0.09 0.16 0.76 0.61
#> c0 = 0.818 0.25 0.20 0.16 0.15 0.01 0.20 0.12 0.75 0.20 0.12 0.62 0.70
#> c0 = 0.804 0.26 0.32 0.13 0.19 0.03 0.10 0.14 0.84 0.17 0.09 0.50 0.64
#> c0 = 0.797 0.17 0.08 0.08 0.10 0.02 0.10 0.10 0.73 0.18 0.15 0.68 0.43
#> c0 = 0.785 0.33 0.19 0.24 0.14 0.09 0.21 0.10 0.65 0.10 0.11 0.48 0.60
#> c0 = 0.777 0.42 0.13 0.17 0.24 0.06 0.24 0.10 0.81 0.12 0.23 0.47 0.48
#>              49   50   51   52   53   54   55   56   57   58   59   60
#> c0 = 1     0.00 0.00 0.00 1.00 1.00 0.00 1.00 1.00 0.00 0.00 0.00 0.00
#> c0 = 0.932 0.00 0.00 0.00 1.00 1.00 0.00 1.00 1.00 0.00 0.00 0.00 0.00
#> c0 = 0.882 0.04 0.58 0.52 0.14 0.42 0.16 0.50 0.34 0.15 0.13 0.04 0.36
#> c0 = 0.852 0.15 0.53 0.64 0.17 0.25 0.08 0.40 0.42 0.18 0.08 0.01 0.64
#> c0 = 0.832 0.16 0.62 0.74 0.16 0.44 0.08 0.27 0.26 0.24 0.09 0.07 0.44
#> c0 = 0.818 0.15 0.55 0.66 0.10 0.41 0.21 0.41 0.30 0.31 0.16 0.01 0.56
#> c0 = 0.804 0.31 0.64 0.74 0.12 0.19 0.09 0.36 0.19 0.09 0.21 0.02 0.46
#> c0 = 0.797 0.46 0.54 0.61 0.17 0.30 0.15 0.56 0.35 0.31 0.12 0.02 0.52
#> c0 = 0.785 0.45 0.58 0.70 0.13 0.46 0.20 0.52 0.28 0.20 0.03 0.22 0.74
#> c0 = 0.777 0.41 0.56 0.60 0.12 0.48 0.23 0.44 0.18 0.30 0.28 0.18 0.59
#>              61   62   63   64   65   66   67   68   69   70   71   72
#> c0 = 1     0.00 0.00 1.00 0.00 0.00 1.00 1.00 1.00 0.00 0.00 0.00 1.00
#> c0 = 0.932 0.00 0.00 1.00 0.00 0.00 1.00 1.00 1.00 0.00 0.00 0.00 1.00
#> c0 = 0.882 0.16 0.35 0.93 0.29 0.64 0.16 0.31 0.10 0.11 0.04 0.11 1.00
#> c0 = 0.852 0.16 0.28 0.94 0.27 0.74 0.31 0.53 0.22 0.10 0.31 0.45 0.92
#> c0 = 0.832 0.18 0.45 0.95 0.44 0.49 0.34 0.63 0.14 0.14 0.43 0.21 0.96
#> c0 = 0.818 0.22 0.35 0.95 0.35 0.50 0.24 0.61 0.26 0.31 0.35 0.11 0.96
#> c0 = 0.804 0.23 0.47 1.00 0.42 0.37 0.22 0.77 0.21 0.25 0.27 0.42 0.96
#> c0 = 0.797 0.24 0.44 0.96 0.45 0.52 0.35 0.49 0.14 0.15 0.36 0.26 0.96
#> c0 = 0.785 0.16 0.38 0.98 0.35 0.46 0.45 0.67 0.15 0.20 0.35 0.12 0.95
#> c0 = 0.777 0.19 0.38 0.97 0.28 0.36 0.50 0.69 0.23 0.32 0.40 0.16 0.92
#>              73   74   75   76   77   78   79   80   81   82   83   84
#> c0 = 1     1.00 1.00 0.00 0.00 1.00 0.00 0.00 0.00 1.00 1.00 0.00 0.00
#> c0 = 0.932 1.00 1.00 0.00 0.00 1.00 0.00 0.00 0.00 1.00 1.00 0.00 0.00
#> c0 = 0.882 0.93 0.52 0.04 0.47 0.56 0.23 0.02 0.25 0.56 0.61 0.21 0.40
#> c0 = 0.852 0.97 0.58 0.07 0.27 0.26 0.19 0.05 0.18 0.34 0.60 0.16 0.24
#> c0 = 0.832 0.98 0.62 0.08 0.49 0.27 0.19 0.03 0.12 0.51 0.82 0.11 0.41
#> c0 = 0.818 1.00 0.53 0.21 0.34 0.27 0.14 0.06 0.29 0.36 0.68 0.11 0.49
#> c0 = 0.804 1.00 0.37 0.16 0.26 0.40 0.12 0.14 0.09 0.55 0.54 0.08 0.49
#> c0 = 0.797 1.00 0.38 0.24 0.31 0.30 0.26 0.18 0.13 0.44 0.65 0.13 0.37
#> c0 = 0.785 0.97 0.43 0.14 0.24 0.35 0.22 0.22 0.14 0.60 0.69 0.02 0.46
#> c0 = 0.777 1.00 0.39 0.22 0.19 0.28 0.17 0.30 0.16 0.55 0.52 0.20 0.39
#>              85   86   87   88   89   90   91   92   93   94   95   96
#> c0 = 1     0.00 0.00 0.00 1.00 0.00 0.00 0.00 1.00 0.00 1.00 1.00 0.00
#> c0 = 0.932 0.00 0.00 0.00 1.00 0.00 0.00 0.00 1.00 0.00 1.00 1.00 0.00
#> c0 = 0.882 0.05 0.34 0.01 0.49 0.14 0.00 0.05 0.18 0.76 0.44 0.57 0.63
#> c0 = 0.852 0.13 0.16 0.03 0.78 0.20 0.00 0.00 0.17 0.86 0.47 0.48 0.77
#> c0 = 0.832 0.07 0.39 0.02 0.46 0.09 0.20 0.01 0.24 0.91 0.53 0.55 0.45
#> c0 = 0.818 0.09 0.30 0.02 0.69 0.08 0.21 0.02 0.22 0.84 0.56 0.58 0.43
#> c0 = 0.804 0.10 0.26 0.02 0.68 0.16 0.17 0.00 0.19 1.00 0.42 0.39 0.41
#> c0 = 0.797 0.19 0.26 0.00 0.64 0.13 0.19 0.02 0.31 0.90 0.35 0.35 0.46
#> c0 = 0.785 0.02 0.19 0.00 0.57 0.23 0.28 0.03 0.20 0.92 0.66 0.43 0.44
#> c0 = 0.777 0.08 0.29 0.00 0.56 0.43 0.22 0.05 0.30 0.89 0.34 0.51 0.42
#>              97   98   99  100
#> c0 = 1     0.00 0.00 1.00 0.00
#> c0 = 0.932 0.00 0.00 1.00 0.00
#> c0 = 0.882 0.00 0.36 0.89 0.19
#> c0 = 0.852 0.08 0.34 0.74 0.14
#> c0 = 0.832 0.27 0.46 0.82 0.02
#> c0 = 0.818 0.14 0.49 0.73 0.03
#> c0 = 0.804 0.15 0.64 0.86 0.10
#> c0 = 0.797 0.07 0.59 0.73 0.08
#> c0 = 0.785 0.31 0.84 0.82 0.12
#> c0 = 0.777 0.23 0.76 0.80 0.12
#>  [ getOption("max.print") est atteint -- 42 lignes omises ]
#> attr(,"c0.seq")
#>  [1] 1.000000 0.932478 0.881700 0.851676 0.832218 0.817683 0.803735
#>  [8] 0.797426 0.784552 0.777326 0.763736 0.756069 0.749276 0.742963
#> [15] 0.735495 0.729197 0.726151 0.721651 0.716387 0.711092 0.706331
#> [22] 0.699614 0.693867 0.689597 0.684966 0.681112 0.679270 0.675346
#> [29] 0.671434 0.668097 0.664013 0.659094 0.654943 0.651218 0.647602
#> [36] 0.645825 0.641575 0.638810 0.636060 0.630945 0.629060 0.624913
#> [43] 0.621906 0.618460 0.615535 0.613463 0.609831 0.605274 0.601986
#> [50] 0.599863 0.594725 0.000000
#> attr(,"steps.seq")
#>  [1] 0.0000000 0.9324777 0.8817000 0.8516759 0.8322181 0.8176826 0.8037348
#>  [8] 0.7974261 0.7845523 0.7773261 0.7637356 0.7560691 0.7492756 0.7429626
#> [15] 0.7354952 0.7291972 0.7261506 0.7216514 0.7163867 0.7110924 0.7063313
#> [22] 0.6996139 0.6938668 0.6895966 0.6849656 0.6811116 0.6792702 0.6753463
#> [29] 0.6714343 0.6680970 0.6640128 0.6590942 0.6549434 0.6512180 0.6476016
#> [36] 0.6458247 0.6415746 0.6388103 0.6360603 0.6309453 0.6290600 0.6249131
#> [43] 0.6219063 0.6184595 0.6155353 0.6134627 0.6098312 0.6052737 0.6019864
#> [50] 0.5998630 0.5947247 1.0000000
#> attr(,"typeboost")
#> [1] "fastboost"
#> attr(,"limi_alea")
#> [1] NA
#> attr(,"B")
#> [1] 100
#> attr(,"class")
#> [1] "selectboost"
```


```r
dec.result.boost.micro_nb1 <- summary(result.boost.micro_nb1)$selectboost_result.dec
dec.result.boost.micro_nb1
#>                       1    2    3    4    5    6    7 8 9 10 11 12 13 14
#>            1.000000e+00 1.00 1.00 1.00 1.00 1.00 1.00 0 0  0  0  0  0  0
#> c0 = 0.932 1.000000e+00 1.00 1.00 1.00 1.00 1.00 1.00 0 0  0  0  0  0  0
#> c0 = 0.882 3.300000e-01 1.00 0.95 0.88 0.72 0.75 0.55 0 0  0  0  0  0  0
#> c0 = 0.852 3.300000e-01 1.00 0.47 0.53 0.72 0.73 0.55 0 0  0  0  0  0  0
#> c0 = 0.832 6.000000e-02 1.00 0.42 0.53 0.71 0.62 0.51 0 0  0  0  0  0  0
#> c0 = 0.818 6.000000e-02 1.00 0.42 0.16 0.65 0.56 0.42 0 0  0  0  0  0  0
#> c0 = 0.804 6.000000e-02 1.00 0.42 0.16 0.58 0.56 0.40 0 0  0  0  0  0  0
#> c0 = 0.797 1.110223e-16 1.00 0.16 0.11 0.58 0.49 0.39 0 0  0  0  0  0  0
#> c0 = 0.785 1.110223e-16 1.00 0.14 0.09 0.49 0.30 0.37 0 0  0  0  0  0  0
#> c0 = 0.777 0.000000e+00 1.00 0.00 0.06 0.49 0.30 0.32 0 0  0  0  0  0  0
#>            15   16 17 18   19   20 21 22 23 24 25   26   27 28 29 30   31
#>             0 1.00  0  0 1.00 1.00  0  0  0  0  0 1.00 1.00  0  0  0 1.00
#> c0 = 0.932  0 1.00  0  0 1.00 1.00  0  0  0  0  0 1.00 1.00  0  0  0 1.00
#> c0 = 0.882  0 1.00  0  0 1.00 0.96  0  0  0  0  0 1.00 0.65  0  0  0 0.92
#> c0 = 0.852  0 0.94  0  0 1.00 0.77  0  0  0  0  0 1.00 0.65  0  0  0 0.92
#> c0 = 0.832  0 0.86  0  0 1.00 0.77  0  0  0  0  0 1.00 0.12  0  0  0 0.22
#> c0 = 0.818  0 0.86  0  0 0.98 0.70  0  0  0  0  0 1.00 0.12  0  0  0 0.22
#> c0 = 0.804  0 0.80  0  0 0.98 0.65  0  0  0  0  0 0.98 0.12  0  0  0 0.21
#> c0 = 0.797  0 0.80  0  0 0.98 0.55  0  0  0  0  0 0.98 0.02  0  0  0 0.19
#> c0 = 0.785  0 0.76  0  0 0.98 0.35  0  0  0  0  0 0.98 0.02  0  0  0 0.19
#> c0 = 0.777  0 0.75  0  0 0.93 0.29  0  0  0  0  0 0.98 0.02  0  0  0 0.19
#>            32 33   34 35 36 37 38 39 40 41 42 43   44 45 46 47   48 49 50
#>             0  0 1.00  0  0  0  0  0  0  0  0  0 1.00  0  0  0 1.00  0  0
#> c0 = 0.932  0  0 1.00  0  0  0  0  0  0  0  0  0 1.00  0  0  0 1.00  0  0
#> c0 = 0.882  0  0 0.96  0  0  0  0  0  0  0  0  0 0.53  0  0  0 0.56  0  0
#> c0 = 0.852  0  0 0.71  0  0  0  0  0  0  0  0  0 0.53  0  0  0 0.33  0  0
#> c0 = 0.832  0  0 0.69  0  0  0  0  0  0  0  0  0 0.53  0  0  0 0.33  0  0
#> c0 = 0.818  0  0 0.58  0  0  0  0  0  0  0  0  0 0.46  0  0  0 0.17  0  0
#> c0 = 0.804  0  0 0.58  0  0  0  0  0  0  0  0  0 0.46  0  0  0 0.17  0  0
#> c0 = 0.797  0  0 0.50  0  0  0  0  0  0  0  0  0 0.41  0  0  0 0.00  0  0
#> c0 = 0.785  0  0 0.50  0  0  0  0  0  0  0  0  0 0.41  0  0  0 0.00  0  0
#> c0 = 0.777  0  0 0.44  0  0  0  0  0  0  0  0  0 0.32  0  0  0 0.00  0  0
#>            51 52   53   54   55 56 57 58 59   60 61 62 63 64 65 66 67 68
#>             0  0 1.00 1.00 1.00  0  0  0  0 1.00  0  0  0  0  0  1  0  0
#> c0 = 0.932  0  0 1.00 1.00 1.00  0  0  0  0 1.00  0  0  0  0  0  1  0  0
#> c0 = 0.882  0  0 0.99 0.47 0.88  0  0  0  0 1.00  0  0  0  0  0  0  0  0
#> c0 = 0.852  0  0 0.99 0.08 0.74  0  0  0  0 1.00  0  0  0  0  0  0  0  0
#> c0 = 0.832  0  0 0.95 0.08 0.66  0  0  0  0 1.00  0  0  0  0  0  0  0  0
#> c0 = 0.818  0  0 0.92 0.08 0.66  0  0  0  0 1.00  0  0  0  0  0  0  0  0
#> c0 = 0.804  0  0 0.92 0.05 0.41  0  0  0  0 1.00  0  0  0  0  0  0  0  0
#> c0 = 0.797  0  0 0.92 0.05 0.41  0  0  0  0 1.00  0  0  0  0  0  0  0  0
#> c0 = 0.785  0  0 0.89 0.05 0.36  0  0  0  0 1.00  0  0  0  0  0  0  0  0
#> c0 = 0.777  0  0 0.89 0.05 0.24  0  0  0  0 1.00  0  0  0  0  0  0  0  0
#>            69 70 71   72   73 74 75 76   77 78 79 80   81   82 83 84 85
#>             0  0  0 1.00 1.00  0  0  0 1.00  0  0  0 1.00 1.00  0  0  0
#> c0 = 0.932  0  0  0 1.00 1.00  0  0  0 1.00  0  0  0 1.00 1.00  0  0  0
#> c0 = 0.882  0  0  0 0.90 1.00  0  0  0 0.68  0  0  0 1.00 0.98  0  0  0
#> c0 = 0.852  0  0  0 0.53 1.00  0  0  0 0.66  0  0  0 0.99 0.83  0  0  0
#> c0 = 0.832  0  0  0 0.53 0.96  0  0  0 0.66  0  0  0 0.96 0.83  0  0  0
#> c0 = 0.818  0  0  0 0.49 0.96  0  0  0 0.41  0  0  0 0.96 0.74  0  0  0
#> c0 = 0.804  0  0  0 0.48 0.96  0  0  0 0.34  0  0  0 0.96 0.74  0  0  0
#> c0 = 0.797  0  0  0 0.48 0.96  0  0  0 0.34  0  0  0 0.96 0.64  0  0  0
#> c0 = 0.785  0  0  0 0.48 0.96  0  0  0 0.14  0  0  0 0.95 0.62  0  0  0
#> c0 = 0.777  0  0  0 0.48 0.96  0  0  0 0.06  0  0  0 0.94 0.56  0  0  0
#>              86 87 88 89 90 91 92   93 94 95 96 97 98 99 100
#>            1.00  0  0  0  0  0  0 1.00  0  0  0  0  0  0   0
#> c0 = 0.932 1.00  0  0  0  0  0  0 1.00  0  0  0  0  0  0   0
#> c0 = 0.882 0.89  0  0  0  0  0  0 1.00  0  0  0  0  0  0   0
#> c0 = 0.852 0.87  0  0  0  0  0  0 0.98  0  0  0  0  0  0   0
#> c0 = 0.832 0.87  0  0  0  0  0  0 0.98  0  0  0  0  0  0   0
#> c0 = 0.818 0.78  0  0  0  0  0  0 0.97  0  0  0  0  0  0   0
#> c0 = 0.804 0.69  0  0  0  0  0  0 0.97  0  0  0  0  0  0   0
#> c0 = 0.797 0.69  0  0  0  0  0  0 0.96  0  0  0  0  0  0   0
#> c0 = 0.785 0.69  0  0  0  0  0  0 0.96  0  0  0  0  0  0   0
#> c0 = 0.777 0.59  0  0  0  0  0  0 0.96  0  0  0  0  0  0   0
#>  [ reached getOption("max.print") -- omitted 42 rows ]
```

#### Confidence indices.

First compute the highest $c_0$ value for which the proportion of selection is under the threshold $thr$. In that analysis, we set $thr=1$.

```r
thr=1
index.last.c0.micro_nb1=apply(dec.result.boost.micro_nb1>=thr,2,which.min)-1
index.last.c0.micro_nb1
#>   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18 
#>   2  51   2   2   2   2   2   0   0   0   0   0   0   0   0   3   0   0 
#>  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36 
#>   5   2   0   0   0   0   0   6   2   0   0   0   2   0   0   2   0   0 
#>  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54 
#>   0   0   0   0   0   0   0   2   0   0   0   2   0   0   0   0   2   2 
#>  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72 
#>   2   0   0   0   0  37   0   0   0   0   0   2   0   0   0   0   0   2 
#>  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90 
#>   4   0   0   0   2   0   0   0   3   2   0   0   0   2   0   0   0   0 
#>  91  92  93  94  95  96  97  98  99 100 
#>   0   0   3   0   0   0   0   0   0   0
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
#>  [1] "c0 = 0.932" "c0 = 0.595" "c0 = 0.932" "c0 = 0.932" "c0 = 0.932"
#>  [6] "c0 = 0.932" "c0 = 0.932" "c0 = 0.882" "c0 = 0.832" "c0 = 0.932"
#> [11] "c0 = 0.818" "c0 = 0.932" "c0 = 0.932" "c0 = 0.932" "c0 = 0.932"
#> [16] "c0 = 0.932" "c0 = 0.932" "c0 = 0.932" "c0 = 0.932" "c0 = 0.642"
#> [21] "c0 = 0.932" "c0 = 0.932" "c0 = 0.852" "c0 = 0.932" "c0 = 0.882"
#> [26] "c0 = 0.932" "c0 = 0.932" "c0 = 0.882"
attr(result.boost.micro_nb1,"c0.seq")[index.last.c0.micro_nb1]
#>  [1] 0.932478 0.594725 0.932478 0.932478 0.932478 0.932478 0.932478
#>  [8] 0.881700 0.832218 0.932478 0.817683 0.932478 0.932478 0.932478
#> [15] 0.932478 0.932478 0.932478 0.932478 0.932478 0.641575 0.932478
#> [22] 0.932478 0.851676 0.932478 0.881700 0.932478 0.932478 0.881700
confidence.indices.micro_nb1 = c(0,1-attr(result.boost.micro_nb1,"c0.seq"))[
  index.last.c0.micro_nb1+1]
confidence.indices.micro_nb1
#>   [1] 0.067522 0.405275 0.067522 0.067522 0.067522 0.067522 0.067522
#>   [8] 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000
#>  [15] 0.000000 0.118300 0.000000 0.000000 0.167782 0.067522 0.000000
#>  [22] 0.000000 0.000000 0.000000 0.000000 0.182317 0.067522 0.000000
#>  [29] 0.000000 0.000000 0.067522 0.000000 0.000000 0.067522 0.000000
#>  [36] 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000
#>  [43] 0.000000 0.067522 0.000000 0.000000 0.000000 0.067522 0.000000
#>  [50] 0.000000 0.000000 0.000000 0.067522 0.067522 0.067522 0.000000
#>  [57] 0.000000 0.000000 0.000000 0.358425 0.000000 0.000000 0.000000
#>  [64] 0.000000 0.000000 0.067522 0.000000 0.000000 0.000000 0.000000
#>  [71] 0.000000 0.067522 0.148324 0.000000 0.000000 0.000000 0.067522
#>  [78] 0.000000 0.000000 0.000000 0.118300 0.067522 0.000000 0.000000
#>  [85] 0.000000 0.067522 0.000000 0.000000 0.000000 0.000000 0.000000
#>  [92] 0.000000 0.118300 0.000000 0.000000 0.000000 0.000000 0.000000
#>  [99] 0.000000 0.000000
barplot(confidence.indices.micro_nb1,col=rgb(jet.colors(confidence.indices.micro_nb1),
maxColorValue = 255), names.arg=colnames(result.boost.micro_nb1), ylim=c(0,1))
abline(h=)
```

<img src="man/figures/README-CascadeDatabarplot-1.png" title="plot of chunk CascadeDatabarplot" alt="plot of chunk CascadeDatabarplot" width="100%" />


Let's compute again, for all the $c_0$ values, the sensitivity (precision), specificity (recall), and several Fscores ($F_1$ harmonic mean of precision and recall, $F_{1/2}$ and $F_2$ two weighted harmonic means of precision and recall).

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
legend(x="topright",legend=c("sensitivity (precision)",
       "specificity (recall)","non-weighted Fscore",
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

