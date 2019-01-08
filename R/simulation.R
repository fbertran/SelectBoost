#' @title Miscellaneous simulation functions
#'
#' @description Define several simulation functions to be used in the demos of the package.
#'
#' @name simulation
#' @param P A numeric value. The number of variables.
#' @param v A numeric value. The diagonal value of the generated matrix.
#' @param group A numeric vector. Group membership of each of the variables.
#' @param cor_group A numeric vector. Intrag-roup Pearson correlation.
#' @param N A numeric value. The number of observations.
#' @param Cor A numeric matrix. A correlation matrix to be used for random sampling.
#' @param X A numeric matrix. Observations*variables.
#' @param supp A numeric vector. The true predictors.
#' @param minB A numeric value. Minimum absolute value for a beta coefficient.
#' @param maxB A numeric value. Maximum absolute value for a beta coefficient.
#' @param stn A numeric value. A scaling factor for the noise in the response.
#'
#' @family Simulation functions
#' @author Frederic Bertrand, \email{frederic.bertrand@@math.unistra.fr}
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Frédéric Bertrand, \url{https://arxiv.org/abs/1810.01670}
#' @seealso \code{\link[glmnet]{glmnet}}, \code{\link[glmnet]{cv.glmnet}}, \code{\link{AICc_BIC_glmnetB}}, \code{\link[lars]{lars}}, \code{\link[lars]{cv.lars}}, \code{\link[msgps]{msgps}}
#' @examples
#' set.seed(314)
#' P<-10
#' N<-10
#' group<-c(rep(1:2,5))
#' cor_group<-c(.8,.4)
#' supp<-c(1,1,1,0,0,0,0,0,0,0)
#' minB<-1
#' maxB<-2
#' stn<-5
NULL
#> NULL

#' @rdname simulation
#'
#' @details \code{simulation_cor} returns a numeric symetric matrix c whose order
#' is the number of variables. An entry \eqn{c_{i,j}} is equal to
#' \itemize{
#'   \item \eqn{i=j}, entries on the diagonal are equal to the v value
#'   \item \eqn{i<>j}, 0 if the variable i and j do not belong to the same group
#'   \item \eqn{i<>j}, \code{cor_group[k]} if the variable i and j belong to the group k
#' }
#' @return \code{simulation_cor} returns a numeric matrix.
#'
#' @examples
#' set.seed(314)
#' C<-simulation_cor(P,1,group,cor_group)
#'
#' @export
simulation_cor<-function(P,v=1,group,cor_group){

	Cor<-matrix(0,P,P)

	u<-1
	for(i in unique(group)){
	Cor[which( group %in% i ),which( group %in% i )]<-cor_group[u]
	u<-u+1
	}

	diag(Cor)<-v
	return(Cor)
}

#' @rdname simulation
#'
#' @details \code{simulation_X} returns a numeric matrix of replicates (by row) of
#' random samples generated according to the Cor matrix.
#'
#' @return \code{simulation_X} returns a numeric matrix.
#'
#' @examples
#' set.seed(314)
#' X<-simulation_X(10,C)
#' G<-abs(cor(X))
#' hist(G[lower.tri(G)])
#'
#' @export
simulation_X<-function(N,Cor){

	L = chol(Cor)
	nvars = dim(L)[1]
	r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
	return(t(r))
}

#' @rdname simulation
#'
#' @details \code{simulation_DATA} returns a list with the X matrix, the response vector Y,
#' the true predictors, the beta coefficients, the scaling factor and the standard deviation.
#'
#' @return \code{simulation_DATA} returns a list.
#'
#' @examples
#' set.seed(314)
#' DATA_exemple<-simulation_DATA(X,supp,1,2,stn)
#'
#' @export
simulation_DATA<-function(X,supp,minB,maxB,stn){

	beta<-rep(0,length(supp))
	co<-runif(sum(supp),minB,maxB)*(rbinom(sum(supp),1,.5)*2-1)
	beta[which(supp==1)]<-co

	sigma2<-(t(beta)%*%var(X)%*%beta)/stn

	Y<-apply(t(t(X)*beta),1,sum)+ rnorm(dim(X)[1],0,1)*c(sqrt(sigma2))

	DATA<-list(X=X,Y=Y,support=supp,beta=beta,stn=stn,sigma=sqrt(sigma2))
	return(DATA)
}

