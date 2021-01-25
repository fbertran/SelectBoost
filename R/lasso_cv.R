#' @title Variable selection functions
#'
#' @description Compute coefficient vector after variable selection.
#'
#' @name var_select
#' @param X A numeric matrix. The predictors matrix.
#' @param Y A binary factor. The 0/1 classification response.
#' @param priors A numeric vector. Weighting vector for the variable selection. When used with the \code{glmnet} estimation function, the weights share the following meanings:
#' \itemize{
#'   \item 0: the variable is always included in the model
#'   \item 1: neutral weight
#'   \item Inf: variable is always excluded from the model
#' }
#' @param penalty A character value to select the penalty term in msgps
#' (Model Selection Criteria via Generalized Path Seeking). Defaults to "enet".
#' "genet" is the generalized elastic net and "alasso" is the adaptive lasso,
#' which is a weighted version of the lasso.
#' @param alpha A numeric value to set the value of \eqn{\alpha} on "enet" and "genet" penalty in msgps
#' (Model Selection Criteria via Generalized Path Seeking).
#' @param M A numeric matrix. The transposed predictors matrix.
#' @param K A numeric value. Number of folds to use.
#' @param eps A numeric value. Threshold to set to 0 the inferred value of a parameter.
#' @param cv.fun A function. Fonction used to create folds. Used to perform corss-validation subkectwise.
#'
#' @return A vector of coefficients.
#' @family Variable selection functions
#' @author Frederic Bertrand, \email{frederic.bertrand@@math.unistra.fr}
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Frédéric Bertrand, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Bioinformatics, 2020. \url{https://doi:10.1093/bioinformatics/btaa855}
#' @seealso \code{\link[glmnet]{glmnet}}, \code{\link[glmnet]{cv.glmnet}}, \code{\link{AICc_BIC_glmnetB}}, \code{\link[lars]{lars}}, \code{\link[lars]{cv.lars}}, \code{\link[msgps]{msgps}}
#' @examples
#' set.seed(314)
#' xran=matrix(rnorm(150),30,5)
#' ybin=sample(0:1,30,replace=TRUE)
#' yran=rnorm(30)
NULL
#> NULL

#' @rdname var_select
#'
#' @details \code{lasso_cv_glmnet_bin_min} returns the vector of coefficients
#' for a binary logistic model estimated by the lasso using the \code{lambda.min} value
#' computed by 10 fold cross validation. It uses the \code{glmnet} function of
#' the \code{glmnet}package.
#'
#' @examples
#' set.seed(314)
#' lasso_cv_glmnet_bin_min(xran,ybin)
#'
#' @export
lasso_cv_glmnet_bin_min<-function(X,Y){

  requireNamespace("glmnet")
  resultat<-glmnet::cv.glmnet(X,Y,family="binomial", type.measure = "class",nfolds=10)
  indice<-resultat$lambda.min
  resultat<-glmnet::glmnet(X,Y,family="binomial")
  as.vector(predict(resultat,type="coefficients",s=indice))

}

#' @rdname var_select
#'
#' @details \code{lasso_cv_glmnet_bin_1se} returns the vector of coefficients
#' for a binary logistic model estimated by the lasso using the \code{lambda.1se}
#' (lambda.min+1se) value computed by 10 fold cross validation. It uses the \code{glmnet}
#' function of the \code{glmnet}package.
#'
#' @examples
#' set.seed(314)
#' lasso_cv_glmnet_bin_1se(xran,ybin)
#'
#' @export
lasso_cv_glmnet_bin_1se<-function(X,Y){

  requireNamespace("glmnet")
  resultat<-glmnet::cv.glmnet(X,Y,family="binomial", type.measure = "class",nfolds=10)
  indice<-resultat$lambda.1se
  resultat<-glmnet(X,Y,family="binomial")
  as.vector(predict(resultat,type="coefficients",s=indice))

}

#' @rdname var_select
#'
#' @details \code{lasso_glmnet_bin_AICc} returns the vector of coefficients
#' for a binary logistic model estimated by the lasso and selected according to the
#' bias-corrected AIC (AICC) criterion. It uses the \code{glmnet}
#'
#' @examples
#' set.seed(314)
#' lasso_glmnet_bin_AICc(xran,ybin)
#'
#' @export
lasso_glmnet_bin_AICc<-function(X,Y){

  requireNamespace("glmnet")
  glmnet.fit <- glmnet::glmnet(X,Y,family="binomial",standardize=F)
  #cutOff = min(which(glmnet.fit$df > 30))
  #subSample = 1:cutOff
  #subSample = 1:max(glmnet.fit$df)
  subSample = 1:min(ncol(X),100)

  if(is.factor(Y)){Ynum=unclass(Y)-1} else {Ynum=Y}
  AICc.gn.median <- AICc_glmnetB(X,Ynum,glmnet.fit,alpha=1,subSample, reducer='median')
  resultat<-vector("numeric",ncol(X))
  resultat[AICc.gn.median$bestSet]<-AICc.gn.median$model$beta
  return(resultat)

}

#' @rdname var_select
#'
#' @details \code{lasso_glmnet_bin_BIC} returns the vector of coefficients
#' for a binary logistic model estimated by the lasso and selected according to the BIC
#'  criterion. It uses the \code{glmnet}
#'
#' @examples
#' set.seed(314)
#' lasso_glmnet_bin_BIC(xran,ybin)
#'
#' @export
lasso_glmnet_bin_BIC<-function(X,Y){

  glmnet.fit <- glmnet::glmnet(X,Y,family="binomial",standardize=F)
  #cutOff = min(which(glmnet.fit$df > 30))
  #subSample = 1:cutOff
  #subSample = 1:max(glmnet.fit$df)
  subSample = 1:min(ncol(X),100)

  if(is.factor(Y)){Ynum=unclass(Y)-1} else {Ynum=Y}
  BIC.gn.median <- BIC_glmnetB(X,Ynum,glmnet.fit,alpha=1,subSample, reducer='median')
  resultat<-vector("numeric",ncol(X))
  resultat[BIC.gn.median$bestSet]<-BIC.gn.median$model$beta
  return(resultat)

}

#' @rdname var_select
#'
#' @details \code{lasso_cv_lars_min} returns the vector of coefficients
#' for a linear model estimated by the lasso using the \code{lambda.min} value
#' computed by 5 fold cross validation. It uses the \code{lars} function of the
#' \code{lars} package.
#'
#' @examples
#' set.seed(314)
#' lasso_cv_lars_min(xran,yran)
#'
#' @export
lasso_cv_lars_min<-function(X,Y){

	resultat<-lars::cv.lars(X,Y,plot.it=FALSE,index=seq(0,1,0.005),K=5)
	indice<-resultat$index[which(resultat$cv==min(resultat$cv))[1]]
	resultat<-lars::lars(X,Y)
	lars::predict.lars(resultat,type="coef",mode="fraction",s=indice)$coefficients

}

#' @rdname var_select
#'
#' @details \code{lasso_cv_lars_1se} returns the vector of coefficients
#' for a linear model estimated by the lasso using the \code{lambda.1se}
#' (lambda.min+1se) value computed by 5 fold cross validation.
#' It uses the \code{lars} function of the \code{lars} package.
#'
#' @examples
#' set.seed(314)
#' lasso_cv_lars_1se(xran,yran)
#'
#' @export
lasso_cv_lars_1se<-function(X,Y){

	resultat<-lars::cv.lars(X,Y,plot.it=FALSE,index=seq(0,1,0.005),K=5)
	indice<-resultat$index[which(resultat$cv+resultat$cv.error==min(resultat$cv+resultat$cv.error))[1]]
	resultat<-lars::lars(X,Y)
	predict(resultat,type="coef",mode="fraction",s=indice)$coefficients

}

#' @rdname var_select
#'
#' @details \code{lasso_cv_glmnet_min} returns the vector of coefficients
#' for a linear model estimated by the lasso using the \code{lambda.min} value
#' computed by 10 fold cross validation. It uses the \code{glmnet} function of the
#' \code{glmnet} package.
#'
#' @examples
#' set.seed(314)
#' lasso_cv_glmnet_min(xran,yran)
#'
#' @export
lasso_cv_glmnet_min<-function(X,Y){

  requireNamespace("glmnet")
  resultat<-glmnet::cv.glmnet(X,Y,nfolds=10)
  coefvec<-try(as.vector(coef(resultat,s="lambda.min")[-1]))
  if(!is.vector(coefvec)){coefvec<-rep(0,ncol(X))}
  return(coefvec)
}

#' @rdname var_select
#'
#' @details \code{lasso_cv_glmnet_min_weighted} returns the vector of coefficients
#' for a linear model estimated by the weighted lasso using the \code{lambda.min} value
#' computed by 10 fold cross validation. It uses the \code{glmnet} function of the
#' \code{glmnet} package.
#'
#' @examples
#' set.seed(314)
#' lasso_cv_glmnet_min_weighted(xran,yran,c(1000,0,0,1,1))
#'
#' @export
lasso_cv_glmnet_min_weighted<-function(X,Y,priors){

  requireNamespace("glmnet")
  if(is.null(priors)) priors<-rep(1,ncol(X))
  resultat<-glmnet::cv.glmnet(X,Y,nfolds=10,penalty.factor=priors)
  coefvec<-try(as.vector(coef(resultat,s="lambda.min")[-1]))
  if(!is.vector(coefvec)){coefvec<-rep(0,ncol(X))}
  return(coefvec)
}


#' @rdname var_select
#'
#' @details \code{lasso_cv_glmnet_1se} returns the vector of coefficients
#' for a linear model estimated by the lasso using the \code{lambda.1se}
#' (lambda.min+1se) value computed by 10 fold cross validation. It uses the \code{glmnet}
#'  function of the
#' \code{glmnet} package.
#'
#' @examples
#' set.seed(314)
#' lasso_cv_glmnet_1se(xran,yran)
#'
#' @export
lasso_cv_glmnet_1se<-function(X,Y){

  requireNamespace("glmnet")
  resultat<-glmnet::cv.glmnet(X,Y,nfolds=10)
  coefvec<-try(as.vector(coef(resultat,s="lambda.1se")[-1]))
  if(!is.vector(coefvec)){coefvec<-rep(0,ncol(X))}
  return(coefvec)
}


#' @rdname var_select
#'
#' @details \code{lasso_cv_glmnet_1se_weighted} returns the vector of coefficients
#' for a linear model estimated by the weighted lasso using the \code{lambda.1se}
#' (lambda.min+1se) value computed by 10 fold cross validation. It uses the \code{glmnet}
#' function of the \code{glmnet} package.
#'
#' @examples
#' set.seed(314)
#' lasso_cv_glmnet_1se_weighted(xran,yran,c(1000,0,0,1,1))
#'
#' @export
lasso_cv_glmnet_1se_weighted<-function(X,Y,priors){

  requireNamespace("glmnet")
  if(is.null(priors)) priors<-rep(1,ncol(X))
  resultat<-glmnet::cv.glmnet(X,Y,nfolds=10,penalty.factor=priors)
  coefvec<-try(as.vector(coef(resultat,s="lambda.1se")[-1]))
  if(!is.vector(coefvec)){coefvec<-rep(0,ncol(X))}
  return(coefvec)
}

#' @rdname var_select
#'
#' @details \code{lasso_msgps_Cp} returns the vector of coefficients
#' for a linear model estimated by the lasso selectd using Mallows' Cp.
#' It uses the \code{msgps} function of the \code{msgps} package.
#'
#' @examples
#' set.seed(314)
#' lasso_msgps_Cp(xran,yran)
#'
#' @export
lasso_msgps_Cp<-function(X,Y,penalty="enet"){

	 fit <- msgps(X,Y,penalty=penalty)
	 round(coef(fit)[-1,1],6)

}


#' @rdname var_select
#'
#' @details \code{lasso_msgps_AICc} returns the vector of coefficients
#' for a linear model estimated by the lasso selected according to the bias-corrected AIC
#' (AICC) criterion. It uses the \code{msgps} function of the \code{msgps} package.
#'
#' @examples
#' set.seed(314)
#' lasso_msgps_AICc(xran,yran)
#'
#' @export
lasso_msgps_AICc<-function(X,Y,penalty="enet"){

	 fit <- msgps(X,Y,penalty=penalty)
	 round(coef(fit)[-1,2],6)

}

#' @rdname var_select
#'
#' @details \code{lasso_msgps_GCV} returns the vector of coefficients
#' for a linear model estimated by the lasso selected according to the generalized
#' cross validation criterion. It uses the \code{msgps} function of the \code{msgps} package.
#'
#' @examples
#' set.seed(314)
#' lasso_msgps_GCV(xran,yran)
#'
#' @export
lasso_msgps_GCV<-function(X,Y,penalty="enet"){
#lasso_GCV

	 fit <- msgps(X,Y,lambda=0,penalty=penalty)
	 round(coef(fit)[-1,3],6)

}

#' @rdname var_select
#'
#' @details \code{lasso_msgps_BIC} returns the vector of coefficients
#' for a linear model estimated by the lasso selected according to the BIC criterion.
#' It uses the \code{msgps} function of the \code{msgps} package.
#'
#' @examples
#' set.seed(314)
#' lasso_msgps_BIC(xran,yran)
#'
#' @export
lasso_msgps_BIC<-function(X,Y,penalty="enet"){
  #lasso_GCV

  fit <- msgps(X,Y,lambda=0,penalty=penalty)
  round(coef(fit)[-1,4],6)

}


#' @rdname var_select
#'
#' @details \code{enetf_msgps_Cp} returns the vector of coefficients
#' for a linear model estimated by the elastic net selectd using Mallows' Cp.
#' It uses the \code{msgps} function of the \code{msgps} package.
#'
#' @examples
#' set.seed(314)
#' enetf_msgps_Cp(xran,yran)
#'
#' @export
enetf_msgps_Cp<-function(X,Y,penalty="enet",alpha=0.5){

  fit <- msgps(X,Y,penalty=penalty,alpha=alpha)
  round(coef(fit)[-1,1],6)

}


#' @rdname var_select
#'
#' @details \code{enetf_msgps_AICc} returns the vector of coefficients
#' for a linear model estimated by the elastic net selected according to the bias-corrected AIC
#' (AICC) criterion. It uses the \code{msgps} function of the \code{msgps} package.
#'
#' @examples
#' set.seed(314)
#' enetf_msgps_AICc(xran,yran)
#'
#' @export
enetf_msgps_AICc<-function(X,Y,penalty="enet",alpha=0.5){

  fit <- msgps(X,Y,penalty=penalty,alpha=alpha)
  round(coef(fit)[-1,2],6)

}

#' @rdname var_select
#'
#' @details \code{enetf_msgps_GCV} returns the vector of coefficients
#' for a linear model estimated by the elastic net selected according to the generalized
#' cross validation criterion. It uses the \code{msgps} function of the \code{msgps} package.
#'
#' @examples
#' set.seed(314)
#' enetf_msgps_GCV(xran,yran)
#'
#' @export
enetf_msgps_GCV<-function(X,Y,penalty="enet",alpha=0.5){
  #lasso_GCV

  fit <- msgps(X,Y,lambda=0,penalty=penalty,alpha=alpha)
  round(coef(fit)[-1,3],6)

}

#' @rdname var_select
#'
#' @details \code{enetf_msgps_BIC} returns the vector of coefficients
#' for a linear model estimated by the elastic net selected according to the BIC criterion.
#' It uses the \code{msgps} function of the \code{msgps} package.
#'
#' @examples
#' set.seed(314)
#' enetf_msgps_BIC(xran,yran)
#'
#' @export
enetf_msgps_BIC<-function(X,Y,penalty="enet",alpha=0.5){
  #lasso_GCV

  fit <- msgps(X,Y,lambda=0,penalty=penalty,alpha=alpha)
  round(coef(fit)[-1,4],6)

}

#' @rdname var_select
#'
#' @details \code{lasso_cascade} returns the vector of coefficients
#' for a linear model estimated by the lasso.
#' It uses the \code{lars} function of the \code{lars} package.
#'
#' @examples
#' set.seed(314)
#' lasso_cascade(t(xran),yran,5,cv.fun=lars::cv.folds)
#'
#' @export
lasso_cascade<-function(M,Y,K,eps=10^-5,cv.fun#=lars::cv.folds
#                    ,cv.fun.name#="lars::cv.folds"
){
  #  require(lars)
#  cat("lasso_reg",cv.fun.name,"\n")
  model<-try(cv.lars1(t(M),(Y),intercept=FALSE,K=K,plot.it=FALSE,eps=eps,cv.fun=cv.fun
#                      , cv.fun.name=cv.fun.name
  ))
  n<-try(model$index[which(model$cv %in% min(model$cv))])
  model<-try(lars::lars(t(M),(Y),intercept=FALSE,eps=eps))
  repu<-try(lars::coef.lars(model,s=n,mode="fraction"))
  if(!is.vector(repu)){repu<-rep(0,dim(M)[1])}
  return(repu)
}

cv.lars1 <- function (x, y, K = 10, index, trace = FALSE, plot.it = TRUE,
                      se = TRUE, type = c("lasso", "lar", "forward.stagewise",
                                          "stepwise"), mode = c("fraction", "step"), cv.fun
#                      , cv.fun.name
                      , ...)
{
  #  requireNamespace("lars")
#  cat(cv.fun.name)
  type = match.arg(type)
  if (missing(mode)) {
    mode = switch(type, lasso = "fraction", lar = "step",
                  forward.stagewise = "fraction", stepwise = "step")
  }
  else mode = match.arg(mode)
  all.folds <- cv.fun(length(y), K)
  #  cat(all.folds[[1]],"\n")
  if (missing(index)) {
    index = seq(from = 0, to = 1, length = 100)
    if (mode == "step") {
      fit = lars::lars(x, y, type = type, ...)
      nsteps = nrow(fit$beta)
      maxfold = max(sapply(all.folds, length))
      nsteps = min(nsteps, length(y) - maxfold)
      index = seq(nsteps)
    }
  }
  residmat <- matrix(0, length(index), K)
  for (i in seq(K)) {
    omit <- all.folds[[i]]
    fit <- lars::lars(x[-omit, , drop = FALSE], y[-omit], trace = trace,
                      type = type, ...)
    fit <- lars::predict.lars(fit, x[omit, , drop = FALSE], mode = mode,
                              s = index)$fit
    if (length(omit) == 1)
      fit <- matrix(fit, nrow = 1)
    residmat[, i] <- apply((y[omit] - fit)^2, 2, mean)
    if (trace)
      cat("\n CV Fold", i, "\n\n")
  }
  cv <- apply(residmat, 1, mean)
  cv.error <- sqrt(apply(residmat, 1, var)/K)
  object <- list(index = index, cv = cv, cv.error = cv.error,
                 mode = mode)
  if (plot.it)
    lars::plotCVLars(object, se = se)
  invisible(object)
}




#' @title Variable selection functions (all)
#'
#' @description Compute coefficient vector after variable selection for the fitting criteria of a given model. May be used for a step by step use of Selectboost.
#'
#' @name var_select_all
#' @param X A numeric matrix. The predictors matrix.
#' @param Y A binary factor. The 0/1 classification response.
#' @param penalty A character value to select the penalty term in msgps
#' (Model Selection Criteria via Generalized Path Seeking). Defaults to "enet".
#' "genet" is the generalized elastic net and "alasso" is the adaptive lasso,
#' which is a weighted version of the lasso.
#' @param alpha A numeric value to set the value of \eqn{\alpha} on "enet" and "genet" penalty in msgps
#' (Model Selection Criteria via Generalized Path Seeking).
#' @param K A numeric value. Number of folds to use.
#' @param K.seq A numeric vector. Number of components to test.
#' @param eta.seq A numeric vector. Eta sequence to test.
#' @param fold.val A numeric value. Number of folds to use.
#' @param include.threshold.list A numeric vector. Vector of threshold to use.
#'
#' @return A vector or matrix of coefficients.
#' @family Variable selection functions
#' @author Frederic Bertrand, \email{frederic.bertrand@@math.unistra.fr}
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Frédéric Bertrand, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Bioinformatics, 2020. \url{https://doi:10.1093/bioinformatics/btaa855}
#' @seealso \code{\link[glmnet]{glmnet}}, \code{\link[glmnet]{cv.glmnet}}, \code{\link[msgps]{msgps}}, \code{\link{AICc_BIC_glmnetB}}, \code{\link[spls]{spls}}, \code{\link[spls]{cv.spls}}, \code{\link[spls]{correct.spls}}, \code{\link[spls]{splsda}}, \code{\link[spls]{cv.splsda}}, \code{\link[spls]{sgpls}}, \code{\link[spls]{cv.sgpls}}, \code{\link[varbvs]{varbvs}}
#' @examples
#' set.seed(314)
#' xran <- matrix(rnorm(100*6),100,6)
#' beta0 <- c(3,1.5,0,0,2,0)
#' epsilon <- rnorm(100,sd=3)
#' yran <- c(xran %*% beta0 + epsilon)
#' ybin <- ifelse(yran>=0,1,0)
NULL
#> NULL

#' @rdname var_select_all
#'
#' @details \code{lasso_msgps_all} returns the matrix of coefficients
#' for an optimal linear model estimated by the LASSO estimator and selected
#' by model selection criteria including Mallows' Cp, bias-corrected AIC (AICc),
#' generalized cross validation (GCV) and BIC.
#' The the \code{msgps} function of the \code{msgps} package implements
#' Model Selection Criteria via Generalized Path Seeking to compute the degrees
#' of freedom of the LASSO.
#'
#' @examples
#' set.seed(314)
#' lasso_msgps_all(xran,yran)
#'
#' @export
lasso_msgps_all <- function (X, Y, penalty = "enet")
{
  fit <- msgps::msgps(X, Y, penalty = penalty)
  round(coef(fit)[-1, ], 6)
}

#' @rdname var_select_all
#'
#' @details \code{enet_msgps_all} returns the matrix of coefficients
#' for an optimal linear model estimated by the ELASTIC NET estimator and selected
#' by model selection criteria including Mallows' Cp, bias-corrected AIC (AICc),
#' generalized cross validation (GCV) and BIC.
#' The the \code{msgps} function of the \code{msgps} package implements
#' Model Selection Criteria via Generalized Path Seeking to compute the degrees
#' of freedom of the ELASTIC NET.
#'
#' @examples
#' set.seed(314)
#' enet_msgps_all(xran,yran)
#'
#' @export
enet_msgps_all <- function (X,
                            Y,
                            penalty = "enet",
                            alpha = 0.5)
{
  fit <- msgps::msgps(X, Y, penalty = penalty, alpha = alpha)
  round(coef(fit)[-1, ], 6)
}

#' @rdname var_select_all
#'
#' @details \code{alasso_msgps_all} returns the matrix of coefficients
#' for an optimal linear model estimated by the adaptive LASSO estimator and selected
#' by model selection criteria including Mallows' Cp, bias-corrected AIC (AICc),
#' generalized cross validation (GCV) and BIC.
#' The the \code{msgps} function of the \code{msgps} package implements
#' Model Selection Criteria via Generalized Path Seeking to compute the degrees
#' of freedom of the adaptive LASSO.
#'
#' @examples
#' set.seed(314)
#' alasso_msgps_all(xran,yran)
#'
#' @export
alasso_msgps_all = function(X, Y, penalty = "alasso") {
  fit <- msgps::msgps(X, Y, penalty = "alasso")
  round(coef(fit)[-1, ], 6)
}

#' @rdname var_select_all
#'
#' @details \code{alasso_enet_msgps_all} returns the matrix of coefficients
#' for an optimal linear model estimated by the adaptive ELASTIC NET estimator and selected
#' by model selection criteria including Mallows' Cp, bias-corrected AIC (AICc),
#' generalized cross validation (GCV) and BIC.
#' The the \code{msgps} function of the \code{msgps} package implements
#' Model Selection Criteria via Generalized Path Seeking to compute the degrees
#' of freedom of the adaptive ELASTIC NET.
#'
#' @examples
#' set.seed(314)
#' alasso_enet_msgps_all(xran,yran)
#'
#' @export
alasso_enet_msgps_all = function(X,
                                 Y,
                                 penalty = "alasso",
                                 alpha = 0.5) {
  fit <- msgps::msgps(X, Y, penalty = "alasso", alpha = alpha)
  round(coef(fit)[-1, ], 6)
}

#' @rdname var_select_all
#'
#' @details \code{lasso_cv_glmnet_all_5f} returns the matrix of coefficients
#' for a linear model estimated by the LASSO using the \code{lambda.min} and \code{lambda.1se}
#' (lambda.min+1se) values computed by 5 fold cross validation. It uses the \code{glmnet}
#' and \code{cv.glmnet} functions of the \code{glmnet} package.
#'
#' @examples
#' set.seed(314)
#' lasso_cv_glmnet_all_5f(xran,yran)
#'
#' @export
lasso_cv_glmnet_all_5f <- function (X, Y) {
  requireNamespace("glmnet")
  resultat <- glmnet::cv.glmnet(X, Y, nfolds = 5)
  coefvec.1se <-
    try(as.vector(coef(resultat, s = "lambda.1se")[-1]))
  if (!is.vector(coefvec.1se)) {
    coefvec.1se <- rep(0, ncol(X))
  }
  coefvec.min <-
    try(as.vector(coef(resultat, s = "lambda.min")[-1]))
  if (!is.vector(coefvec.min)) {
    coefvec.min <- rep(0, ncol(X))
  }
  return(cbind(lambda.min = coefvec.min, lambda.1se = coefvec.1se))
}

#' @rdname var_select_all
#'
#' @details \code{spls_spls_all} returns the matrix of the raw (\code{coef.spls})
#' and \code{correct.spls} and bootstrap corrected coefficients
#' for a linear model estimated by the SPLS (sparse partial least squares) and 5 fold cross validation.
#' It uses the \code{spls}, \code{cv.spls}, \code{ci.spls}, \code{coef.spls} and
#' \code{correct.spls} functions of the \code{spls} package.
#'
#' @examples
#' set.seed(314)
#' spls_spls_all(xran,yran)
#'
#' @export
spls_spls_all <-
  function(X,
           Y,
           K.seq = c(1:5),
           eta.seq = (1:9) / 10,
           fold.val = 5) {
    cv <-
      spls::cv.spls(
        X,
        Y,
        K = K.seq,
        eta = eta.seq,
        fold = fold.val,
        plot.it = FALSE
      )
    f <- spls::spls(X, Y, eta = cv$eta.opt, K = cv$K.opt)
    ci.f <- spls::ci.spls(f)
    cf <- spls::correct.spls(ci.f, plot.it = FALSE)
    tempres = cbind(raw_coefs = spls::coef.spls(f),
                    bootstrap_corrected_coefs = cf)
    colnames(tempres) <-
      c(
        paste("raw_coefs", "K.opt", cv$K.opt, "eta.opt", cv$eta.opt, sep = "_"),
        paste(
          "bootstrap_corrected_coefs",
          "K.opt",
          cv$K.opt,
          "eta.opt",
          cv$eta.opt,
          sep = "_"
        )
      )
    return(tempres)
  }

#' @rdname var_select_all
#'
#' @details \code{varbvs_linear_all} returns the matrix of the coefficients
#' for a linear model estimated by the varbvs (variational approximation for Bayesian
#' variable selection in linear regression, \code{family = gaussian}) and the requested threshold values.
#' It uses the \code{varbvs}, \code{coef} and \code{variable.names} functions of the \code{varbvs} package.
#'
#' @examples
#' set.seed(314)
#' varbvs_linear_all(xran,yran)
#'
#' @export
varbvs_linear_all <-
  function (X, Y, include.threshold.list = (1:19) / 20) {
    fit <- varbvs::varbvs(
      X,
      NULL,
      Y,
      family = "gaussian",
      logodds = seq(-3.5, -1, 0.1),
      sa = 1,
      verbose = FALSE
    )
    selecvar <- function(fit, include.threshold.val) {
      res <- coef(fit)[, "averaged"]
      res[!(
        rownames(coef(fit)) %in% varbvs::variable.names.varbvs(fit, include.threshold = include.threshold.val)
      )] <- 0
      if (length(grep("(Intercept)", names(res))) > 0) {
        res <- res[-grep("(Intercept)", names(res))]
      }
      if (length(grep("Z", names(res))) > 0) {
        res <- res[-grep("Z", names(res))]
      }
      res <- data.frame(res)
      colnames(res) <-
        paste("coef", "varbvs", include.threshold.val, sep = "_")
      return(res)
    }
    tempres = NULL
    for (thres in include.threshold.list) {
      tempres <- abind::abind(tempres, selecvar(fit, thres))
    }
    return(tempres)
  }

############# end of continuous response criterions ###############


############# begin of binomial response criterions ###############

#' @rdname var_select_all
#'
#' @details \code{lasso_cv_glmnet_bin_all} returns the matrix of coefficients
#' for a logistic model estimated by the LASSO using the \code{lambda.min} and \code{lambda.1se}
#' (lambda.min+1se) values computed by 5 fold cross validation. It uses the \code{glmnet} and \code{cv.glmnet}
#' functions of the \code{glmnet} package.
#'
#' @examples
#' set.seed(314)
#' lasso_cv_glmnet_bin_all(xran,ybin)
#'
#' @export
lasso_cv_glmnet_bin_all <- function (X, Y) {
  requireNamespace("glmnet")
  resultat <- glmnet::cv.glmnet(X,
                                Y,
                                family = "binomial",
                                type.measure = "class",
                                nfolds = 5)
  coefvec.1se <-
    try(as.vector(coef(resultat, s = "lambda.1se")[-1]))
  if (!is.vector(coefvec.1se)) {
    coefvec.1se <- rep(0, ncol(X))
  }
  coefvec.min <-
    try(as.vector(coef(resultat, s = "lambda.min")[-1]))
  if (!is.vector(coefvec.min)) {
    coefvec.min <- rep(0, ncol(X))
  }
  return(cbind(lambda.min = coefvec.min, lambda.1se = coefvec.1se))
}

#' @rdname var_select_all
#'
#' @details \code{lasso_glmnet_bin_all} returns the matrix of coefficients
#' for a logistic model estimated by the LASSO using the \code{AICc_glmnetB} and \code{BIC_glmnetB}
#' information criteria. It uses the \code{glmnet} function of the \code{glmnet} package and the
#' \code{AICc_glmnetB} and \code{BIC_glmnetB} functions of the \code{SelectBoost} package that were
#' adapted from the \code{AICc_glmnetB} and \code{BIC_glmnetB} functions of the \code{rLogistic}
#' (\url{https://github.com/echi/rLogistic}) package.
#'
#' @examples
#' set.seed(314)
#' lasso_glmnet_bin_all(xran,ybin)
#'
#' @export
lasso_glmnet_bin_all <- function (X, Y) {
  requireNamespace("glmnet")
  glmnet.fit <-
    glmnet::glmnet(X, Y, family = "binomial", standardize = F)
  subSample = 1:min(ncol(X), 100)
  if (is.factor(Y)) {
    Ynum = unclass(Y) - 1
  }
  else {
    Ynum = Y
  }
  AICc.gn.median <- SelectBoost::AICc_glmnetB(X,
                                 Ynum,
                                 glmnet.fit,
                                 alpha = 1,
                                 subSample,
                                 reducer = "median")
  resultat.AICc <- vector("numeric", ncol(X))
  resultat.AICc[AICc.gn.median$bestSet] <- AICc.gn.median$model$beta
  BIC.gn.median <- SelectBoost::BIC_glmnetB(X,
                               Ynum,
                               glmnet.fit,
                               alpha = 1,
                               subSample,
                               reducer = "median")
  resultat.BIC <- vector("numeric", ncol(X))
  resultat.BIC[BIC.gn.median$bestSet] <- BIC.gn.median$model$beta
  return(cbind(AICc = resultat.AICc, BIC = resultat.BIC))
}

#' @rdname var_select_all
#'
#' @details \code{splsda_spls_all} returns the matrix of the raw (\code{coef.splsda}) coefficients
#' for logistic regression model estimated by the SGPLS (sparse généralized partial least squares) and
#' 5 fold cross validation. It uses the \code{splsda}, \code{cv.splsda} and \code{coef.splsda} functions
#' of the \code{sgpls} package.
#'
#' @examples
#' set.seed(314)
#' \donttest{
#' splsda_spls_all(xran,ybin, K.seq=1:3)
#'}
#'
#' @export
splsda_spls_all <- function(X,
                            Y,
                            K.seq = c(1:10),
                            eta.seq = (1:9) / 10) {
  cv <-
    spls::cv.splsda(
      X,
      Y,
      fold = 5,
      K = K.seq,
      eta = eta.seq,
      plot.it = FALSE,
      n.core = 1
    )
  f <- spls::splsda(X, Y, eta = cv$eta.opt, K = cv$K.opt)
  tempres = cbind(raw_coefs = spls::coef.splsda(f))
  colnames(tempres) <-
    c(paste("raw_coefs", "K.opt", cv$K.opt, "eta.opt", cv$eta.opt, sep = "_"))#,
  return(tempres)
}

#' @rdname var_select_all
#'
#' @details \code{sgpls_spls_all} returns the matrix of the raw (\code{coef.sgpls}) coefficients
#' for logistic regression model estimated by the SGPLS (sparse généralized partial least squares) and
#' 5 fold cross validation. It uses the \code{sgpls}, \code{cv.sgpls} and \code{coef.sgpls} functions
#' of the \code{sgpls} package.
#'
#' @examples
#' set.seed(314)
#' \donttest{
#' sgpls_spls_all(xran,ybin, K.seq=1:3)
#'}
#'
#' @export
sgpls_spls_all <- function(X,
                           Y,
                           K.seq = c(1:10),
                           eta.seq = (1:9) / 10) {
  cv <-
    spls::cv.sgpls(
      X,
      Y,
      fold = 5,
      K = K.seq,
      eta = eta.seq,
      plot.it = FALSE,
      n.core = 1
    )
  f <- spls::sgpls(X, Y, eta = cv$eta.opt, K = cv$K.opt)
  tempres = cbind(raw_coefs = spls::coef.sgpls(f))[-1, , drop = FALSE]
  colnames(tempres) <-
    c(paste("raw_coefs", "K.opt", cv$K.opt, "eta.opt", cv$eta.opt, sep = "_"))#,
  return(tempres)
}

#' @rdname var_select_all
#'
#' @details \code{varbvs_binomial_all} returns the matrix of the coefficients
#' for a linear model estimated by the varbvs (variational approximation for Bayesian
#' variable selection in logistic regression, \code{family = binomial}) and the requested threshold values.
#' It uses the \code{varbvs}, \code{coef} and \code{variable.names} functions of the \code{varbvs} package.
#'
#' @examples
#' set.seed(314)
#' varbvs_binomial_all(xran,ybin)
#'
#' @export
varbvs_binomial_all <-
  function (X, Y, include.threshold.list = (1:19) / 20) {
    fit <- varbvs::varbvs(
      X,
      NULL,
      Y,
      family = "binomial",
      logodds = seq(-3.5, -1, 0.1),
      sa = 1,
      verbose = FALSE
    )
    selecvar <- function(fit, include.threshold.val) {
      res <- coef(fit)[, "averaged"]
      res[!(
        rownames(coef(fit)) %in% varbvs::variable.names.varbvs(fit, include.threshold = include.threshold.val)
      )] <- 0
      if (length(grep("(Intercept)", names(res))) > 0) {
        res <- res[-grep("(Intercept)", names(res))]
      }
      if (length(grep("Z", names(res))) > 0) {
        res <- res[-grep("Z", names(res))]
      }
      res <- data.frame(res)
      colnames(res) <-
        paste("coef", "varbvs", include.threshold.val, sep = "_")
      return(res)
    }
    tempres = NULL
    for (thres in include.threshold.list) {
      tempres <- abind::abind(tempres, selecvar(fit, thres))
    }
    return(tempres)
  }

############# end of binomial response criterions ###############
