#' SelectBoost
#'
#' Motivation: With the growth of big data, variable selection has become one of the major
#' challenges in statistics. Although many methods have been proposed in the literature their
#' performance in terms of recall and precision are limited in a context where the number of
#' variables by far exceeds the number of observations or in a high correlated setting.
#' Results: This package implements a new general algorithm which improves the precision of any
#' existing variable selection method. This algorithm is based on highly intensive simulations and
#' takes into account the correlation structure of the data. Our algorithm can either produce a
#' confidence index for variable selection or it can be used in an experimental design planning
#' perspective.
#'
#' @docType package
#' @name SelectBoost
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Frédéric Bertrand, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Bioinformatics, 2020. \url{https://doi:10.1093/bioinformatics/btaa855}
# @import lars glmnet igraph parallel msgps Rfast doMC foreach
#' @import lars glmnet msgps spls varbvs
#' @importFrom abind abind
#' @importFrom methods new
#' @importFrom utils head tail
#' @importFrom stats coef cor heatmap predict var
#' @importFrom grDevices rainbow
#' @importFrom graphics abline axis barplot matplot mtext par plot points segments
#' @importFrom stats binom.test median plogis pt qbinom quantile rbinom rnorm runif sd t.test wilcox.test
#' @importFrom Rfast vmf.mle rvmf
# #' @importFrom parallel detectCores makeCluster clusterEvalQ clusterExport parLapply stopCluster
# #' @importFrom doMC registerDoMC
# #' @importFrom foreach foreach "%dopar%"
#' @importFrom igraph graph.adjacency infomap.community communities
#' @importFrom grDevices rgb gray
#' @importFrom graphics image layout
#' @examples
#' set.seed(314)
#' xran=matrix(rnorm(75),15,5)
#' ybin=sample(0:1,15,replace=TRUE)
#' yran=rnorm(15)
#'
#' #For quick test purpose, not meaningful, should be run with greater value of B
#' #(disabling parallel computing as well)
#' res.fastboost <- fastboost(xran,yran,B=3,use.parallel=FALSE)
#'
#' \donttest{
#' fastboost(xran,yran)
#' #Customize resampling levels
#' fastboost(xran,yran,steps.seq=c(.99,.95,.9),c0lim=FALSE)
#'
#' #Binary logistic regression
#' fastboost(xran,ybin,func=lasso_cv_glmnet_bin_min)
#'}

NULL
