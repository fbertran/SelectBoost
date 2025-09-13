#' @keywords internal
#' @aliases SelectBoost-package SelectBoost NULL
#'
#' @references F. Bertrand, I. Aouadi, N. Jung, R. Carapito, L. Vallat, S. Bahram, M. Maumy-Bertrand (2020). SelectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets, \emph{Bioinformatics}. \doi{10.1093/bioinformatics/btaa855}
#'
#' SelectBoost was used to decypher networks in
#' C. Schleiss, [...], M. Maumy-Bertrand, S. Bahram, F. Bertrand, and L. Vallat. (2021). Temporal multiomic modelling reveals a B-cell receptor proliferative program in chronic lymphocytic leukemia. \emph{Leukemia}.
#'
#'#' @author This package has been written by Frédéric Bertrand, Myriam
#' Maumy-Bertrand, Ismail Aouadi and Nicolas Jung.
#' Maintainer: Frédéric Bertrand <frederic.bertrand@@lecnam.net>
#'
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
#'
"_PACKAGE"

# @import parallel doMC foreach
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
#' @importFrom parallel detectCores
#'
NULL
