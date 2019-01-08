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
#' @param alpha A numeric value to set the value of α on "enet" and "genet" penalty in msgps
#' (Model Selection Criteria via Generalized Path Seeking).
#'
#' @return A vector of coefficients.
#' @family Variable selection functions
#' @author Frederic Bertrand, \email{frederic.bertrand@@math.unistra.fr}
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Frédéric Bertrand, \url{https://arxiv.org/abs/1810.01670}
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
  if(!is.vector(coefvec)){repu<-rep(0,ncol(X))}
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
  if(!is.vector(coefvec)){repu<-rep(0,ncol(X))}
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
  if(!is.vector(coefvec)){repu<-rep(0,ncol(X))}
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
  if(!is.vector(coefvec)){repu<-rep(0,ncol(X))}
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

