#' @title AICc and BIC for glmnet logistic models
#'
#' @description Compute AICc and BIC for glmnet logistic models.
#'
#' @details Calculate AICc and BIC for glmnet logistic models from the glmnetB function
#' of the package rLogistic \url{https://github.com/echi/rLogistic} and adapted
#' to deal with non finite exponential values in AICc and BIC computations
#'
#' @name AICc_BIC_glmnetB
#'
#' @return A list relevant to model selection.
#' @author Frederic Bertrand, \email{frederic.bertrand@@lecnam.net}
#' @references \emph{Robust Parametric Classification and Variable Selection by a Minimum Distance Criterion}, Chi and Scott, Journal of Computational and Graphical Statistics, \bold{23}(1), 2014, p111--128, \doi{10.1080/10618600.2012.737296}.
#' @seealso \code{\link{var_select}}
#' @examples
#' set.seed(314)
#' xran=matrix(rnorm(150),30,5)
#' ybin=sample(0:1,30,replace=TRUE)
#' glmnet.fit <- glmnet.fit <- glmnet::glmnet(xran,ybin,family="binomial",standardize=FALSE)
NULL
#> NULL


#' @rdname AICc_BIC_glmnetB
#'
#' @param v1 A numeric vector.
#' @param v2 A numeric vector.
#'
#' @examples
#' set.seed(314)
#' rerr(1:10,10:1)
#'
#' @export
rerr = function(v1, v2) {

	return (max(abs(v1 - v2) / (1 + abs(v1))))

}

#' @rdname AICc_BIC_glmnetB
#'
#' @param X A numeric matrix
#' @param Y A numeric 0/1 vector.
#' @param lambda A numeric value.
#' @param beta0 A numeric value Initial intercept value.
#' @param beta A numeric vector. Initial coefficient values.
#' @param maxiter A numeric value. Maximum number of iterations.
#' @param tol A numeric value. Tolerance value.
#'
#' @examples
#' set.seed(314)
#' ridge_logistic(xran,ybin,lambda=.5,beta0=rnorm(5),beta=rnorm(5,1))
#'
#' @export
ridge_logistic = function(X, Y, lambda, beta0, beta, maxiter=1000, tol=1e-10) {

  if(is.factor(Y)){Ynum=unclass(Y)-1} else {Ynum=Y}
	n = dim(X)[1]; p = dim(X)[2]
	X = scale(X, center=T, scale=F)
	svdX = svd(X)
	U = svdX$u
	V = svdX$v
	d = svdX$d
	d = d / (d^2 + 4*lambda)

	for (iter in 1:maxiter) {
		P = plogis(beta0 + X %*% beta)
		beta0_last = beta0; beta_last = beta
		# Update beta0
		beta0 = beta0 + 4*mean(Ynum-P)

		# Update beta
		z = X %*% beta + 4 * (Ynum - P)
		beta = V %*% (d * (t(U) %*% z))

		if (rerr(c(beta0_last, beta_last), c(beta0, beta)) < tol)
			break
	}
	return(list(b0 = beta0, beta=beta, iters=iter))

}

#' @rdname AICc_BIC_glmnetB
#'
#' @param Z A numeric matrix
#' @param glmnet.model A fitted glmnet model.
#' @param alpha A numeric value.
#' @param modelSet Modelset to consider.
#' @param reducer A character value. Reducer function. Either 'median' or 'mean'.
#'
#' @examples
#' set.seed(314)
#' if(is.factor(ybin)){ynum=unclass(ybin)-1} else {ynum=ybin}
#' subSample <- 1:min(ncol(xran),100)
#' BIC_glmnetB(xran,ynum,glmnet.fit,alpha=1,subSample, reducer='median')
#'
#' @export
BIC_glmnetB = function(Z, Y, glmnet.model, alpha, modelSet, reducer='median') {

# First calculate the degrees of freedom.
	activeSet.gn = list()
	lrm.fit.gn = list()
	dof = c()
	BIC = c()
	L = c()
	bic.gn = list()
	n = length(Y)

	if (reducer == 'median') {

		for (i in modelSet) {

			activeSet.gn[[i]] = which(abs(glmnet.model$beta[,i]) > 0)
			sas = length(activeSet.gn[[i]])
			lambda = glmnet.model$lambda[i]*(1-alpha)
			if (sas > 0) {
				beta0 = glmnet.model$a0[i]
				beta = matrix(glmnet.model$beta[activeSet.gn[[i]],i], ncol=1)
				Za = Z[,activeSet.gn[[i]],drop=F]
				ridge = ridge_logistic(Za, Y, n*lambda, beta0, beta)
				beta0 = ridge[[1]]
				beta = ridge[[2]]
				Zb = Za %*% beta

				corzz = function(zz) {
				  if(is.infinite(exp(zz))){
				    return(zz)
				    #cat(L[i],"\n")
				  } else {
				    return(log(1 + exp(zz)))
				  }
				}

				#if(any(is.infinite(exp(Zb)))){cat(exp(Zb),"\n");cat(corzz(Zb),"\n")}

				L[i] = median(Y * Zb - sapply(Zb,corzz))
				d = svd(Za)$d
				dof[i] = sum(d^2 / (d^2 + n*lambda))

				mod = list()
				mod$a0 = beta0
				mod$beta = beta
				mod$df = dof[i]
				mod$lambda = lambda
				lrm.fit.gn[[i]] = mod
			} else {
				dof[i] = 0
				linearPredictor = glmnet.model$a0[i]
				corzz = function(zz) {
				  if(is.infinite(exp(zz))){
				    return(zz)
				    #cat(L[i],"\n")
				  } else {
				    return(log(1 + exp(zz)))
				  }
				}

				L[i] = median(Y * linearPredictor - sapply(linearPredictor,corzz))

				nullModel = list()
				nullModel$a0 = linearPredictor
				nullModel$beta = NULL
				nullModel$df = 0
				nullModel$lambda = lambda
				lrm.fit.gn[[i]] = nullModel
			}
			BIC[i] = -2*L[i] + log(n)*dof[i]/n
		}
	} else if (reducer == 'mean') {
		for (i in modelSet) {

			activeSet.gn[[i]] = which(abs(glmnet.model$beta[,i]) > 0)
			sas = length(activeSet.gn[[i]])
			lambda = glmnet.model$lambda[i]*(1-alpha)
			if (sas > 0) {
				beta0 = glmnet.model$a0[i]
				beta = matrix(glmnet.model$beta[activeSet.gn[[i]],i], ncol=1)
				Za = Z[,activeSet.gn[[i]],drop=F]
				ridge = ridge_logistic(Za, Y, lambda, beta0, beta)
				beta0 = ridge[[1]]
				beta = ridge[[2]]
				Zb = Za %*% beta

				corzz = function(zz) {
				  if(is.infinite(exp(zz))){
				    return(zz)
				    #cat(L[i],"\n")
				  } else {
				    return(log(1 + exp(zz)))
				  }
				}

				#if(any(is.infinite(exp(Zb)))){cat(exp(Zb),"\n");cat(corzz(Zb),"\n")}

				L[i] = sum(Y * Zb - sapply(Zb,corzz))
				d = svd(Za)$d
				dof[i] = sum(d^2 / (d^2 + n*lambda))

				mod = list()
				mod$a0 = beta0
				mod$beta = beta
				mod$df = dof[i]
				mod$lambda = lambda
				lrm.fit.gn[[i]] = mod
			} else {
				dof[i] = 0
				linearPredictor = glmnet.model$a0[i]
				corzz = function(zz) {
				  if(is.infinite(exp(zz))){
				    return(zz)
				    #cat(L[i],"\n")
				  } else {
				    return(log(1 + exp(zz)))
				  }
				}


				L[i] = sum(Y * linearPredictor - sapply(linearPredictor,corzz))
				nullModel = list()
				nullModel$a0 = linearPredictor
				nullModel$beta = NULL
				nullModel$df = 0
				nullModel$lambda = lambda
				lrm.fit.gn[[i]] = nullModel
			}
			BIC[i] = (-2*L[i] + log(n)*dof[i])/n
		}
	} else {
		# Add error to generate a message that this reducer is not supported.

	}

	bic.gn$BIC = BIC
	bic.gn$minBIC = min(BIC)
	bic.gn$dof = dof
	bic.gn$activeSets = activeSet.gn
	bic.model.index = min(which(BIC == min(BIC)))
	bic.gn$bestSet = activeSet.gn[[bic.model.index]]
	bic.gn$model = lrm.fit.gn[[bic.model.index]]

	return(bic.gn)
}

#' @rdname AICc_BIC_glmnetB
#'
#' @examples
#' set.seed(314)
#' if(is.factor(ybin)){ynum=unclass(ybin)-1} else {ynum=ybin}
#' subSample <- 1:min(ncol(xran),100)
#' AICc_glmnetB(xran,ynum,glmnet.fit,alpha=1,subSample, reducer='median')
#'
#' @export
AICc_glmnetB = function(Z, Y, glmnet.model, alpha, modelSet, reducer='median') {

# First calculate the degrees of freedom.
	activeSet.gn = list()
	lrm.fit.gn = list()
	dof = c()
	AICc = c()
	L = c()
	aicc.gn = list()
	n = length(Y)

	if (reducer == 'median') {

		for (i in modelSet) {

			activeSet.gn[[i]] = which(abs(glmnet.model$beta[,i]) > 0)
			sas = length(activeSet.gn[[i]])
			lambda = glmnet.model$lambda[i]*(1-alpha)
			if (sas > 0) {
				beta0 = glmnet.model$a0[i]
				beta = matrix(glmnet.model$beta[activeSet.gn[[i]],i], ncol=1)
				Za = Z[,activeSet.gn[[i]],drop=F]
				ridge = ridge_logistic(Za, Y, n*lambda, beta0, beta)
				beta0 = ridge[[1]]
				beta = ridge[[2]]
				Zb = Za %*% beta
				corzz = function(zz) {
				  if(is.infinite(exp(zz))){
				    return(zz)
				    #cat(L[i],"\n")
				  } else {
				    return(log(1 + exp(zz)))
				  }
				}

				#if(any(is.infinite(exp(Zb)))){cat(exp(Zb),"\n");cat(corzz(Zb),"\n")}

				L[i] = median(Y * Zb - sapply(Zb,corzz))
				d = svd(Za)$d
				dof[i] = sum(d^2 / (d^2 + n*lambda))

				mod = list()
				mod$a0 = beta0
				mod$beta = beta
				mod$df = dof[i]
				mod$lambda = lambda
				lrm.fit.gn[[i]] = mod
			} else {
				dof[i] = 0
				linearPredictor = glmnet.model$a0[i]
				corzz = function(zz) {
				  if(is.infinite(exp(zz))){
				    return(zz)
				    #cat(L[i],"\n")
				  } else {
				    return(log(1 + exp(zz)))
				  }
				}

				L[i] = median(Y * linearPredictor - sapply(linearPredictor,corzz))
				nullModel = list()
				nullModel$a0 = linearPredictor
				nullModel$beta = NULL
				nullModel$df = 0
				nullModel$lambda = lambda
				lrm.fit.gn[[i]] = nullModel
			}
			###Verifier ici
			tempk=dof[i]/n
			AICc[i] = -2*L[i] + 2*tempk + 2*tempk*(tempk+1)/(n-tempk-1)
		}
	} else if (reducer == 'mean') {
		for (i in modelSet) {

			activeSet.gn[[i]] = which(abs(glmnet.model$beta[,i]) > 0)
			sas = length(activeSet.gn[[i]])
			lambda = glmnet.model$lambda[i]*(1-alpha)
			if (sas > 0) {
				beta0 = glmnet.model$a0[i]
				beta = matrix(glmnet.model$beta[activeSet.gn[[i]],i], ncol=1)
				Za = Z[,activeSet.gn[[i]],drop=F]
				ridge = ridge_logistic(Za, Y, lambda, beta0, beta)
				beta0 = ridge[[1]]
				beta = ridge[[2]]
				Zb = Za %*% beta
				corzz = function(zz) {
				  if(is.infinite(exp(zz))){
				    return(zz)
				    #cat(L[i],"\n")
				  } else {
				    return(log(1 + exp(zz)))
				  }
				}

				#if(any(is.infinite(exp(Zb)))){cat(exp(Zb),"\n");cat(corzz(Zb),"\n")}

				L[i] = sum(Y * Zb - sapply(Zb,corzz))
				d = svd(Za)$d
				dof[i] = sum(d^2 / (d^2 + n*lambda))

				mod = list()
				mod$a0 = beta0
				mod$beta = beta
				mod$df = dof[i]
				mod$lambda = lambda
				lrm.fit.gn[[i]] = mod
			} else {
				dof[i] = 0
				linearPredictor = glmnet.model$a0[i]
				corzz = function(zz) {
				  if(is.infinite(exp(zz))){
				    return(zz)
				    #cat(L[i],"\n")
				  } else {
				    return(log(1 + exp(zz)))
				  }
				}


				L[i] = sum(Y * linearPredictor - sapply(linearPredictor,corzz))
				nullModel = list()
				nullModel$a0 = linearPredictor
				nullModel$beta = NULL
				nullModel$df = 0
				nullModel$lambda = lambda
				lrm.fit.gn[[i]] = nullModel
			}
			###Verifier ici
			tempk=dof[i]/n
			AICc[i] = -2*L[i]/n + 2*tempk + 2*tempk*(tempk+1)/(n-tempk-1)
		}
	} else {
		# Add error to generate a message that this reducer is not supported.

	}

	aicc.gn$AICc = AICc
	aicc.gn$minAICc = min(AICc)
	aicc.gn$dof = dof
	aicc.gn$activeSets = activeSet.gn
	aicc.model.index = min(which(AICc == min(AICc)))
	aicc.gn$bestSet = activeSet.gn[[aicc.model.index]]
	aicc.gn$model = lrm.fit.gn[[aicc.model.index]]

	return(aicc.gn)
}
