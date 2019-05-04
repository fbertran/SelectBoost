#' @title Variable selection functions
#'
#' @description Compute coefficient vector after variable selection.
#'
#' @name var_select
#' @param X A numeric matrix. The predictors matrix or the transposed predictors matrix for lasso_cascade.
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
#' @param K A numeric value. Number of folds to use.
#' @param eps A numeric value. Threshold to set to 0 the inferred value of a parameter.
#' @param cv.fun A function. Fonction used to create folds. Used to perform corss-validation subkectwise.
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

#' @rdname var_select
#'
#' @details \code{lasso_cascade} returns the vector of coefficients
#' for a linear model estimated by the lasso.
#' It uses the \code{lars} function of the \code{lars} package.
#'
#' @examples
#' set.seed(314)
#' \dontrun{
#' #Not ment to used out outside of man function.
#' lasso_cascade(t(xran),yran,5,cv.fun=lars::cv.folds)
#' }
#'
#' @export
lasso_cascade<-function(X,Y,K,eps=10^-5,cv.fun#=lars::cv.folds
#                    ,cv.fun.name#="lars::cv.folds" #X = M
){
  #  require(lars)
#  cat("lasso_reg",cv.fun.name,"\n")
#cv.subjects
  #  if(!missing("cv.subjects")){cv.subjects=eval(expression(cv.subjects),envir = sys.frame(sys.nframe()-12))}
  if(!missing("eps")){eps=eval(expression(eps),envir = sys.frame(sys.nframe()-12))}
  if(!missing("K")){K=eval(expression(K),envir = sys.frame(sys.nframe()-12))}
  if(!missing("cv.fun")){cv.fun=eval(expression(cv.folds1),envir = sys.frame(sys.nframe()-12))}
  if(!missing("cv.fun")){cv.folds1=eval(expression(cv.folds1),envir = sys.frame(sys.nframe()-12))}

  model<-try(cv.lars1(t(X),(Y),intercept=FALSE,K=K,plot.it=FALSE,eps=eps,cv.fun=cv.fun
#                      , cv.fun.name=cv.fun.name
  ))
  n<-try(model$index[which(model$cv %in% min(model$cv))])
  model<-try(lars::lars(t(X),(Y),intercept=FALSE,eps=eps))
  repu<-try(lars::coef.lars(model,s=n,mode="fraction"))
  if(!is.vector(repu)){repu<-rep(0,dim(X)[1])}
  return(repu)
}

cv.lars1 <- function (x, y, K = 10, index, trace = FALSE, plot.it = TRUE,
                      se = TRUE, type = c("lasso", "lar", "forward.stagewise",
                                          "stepwise"), mode = c("fraction", "step"), cv.fun
#                      , cv.fun.name
                      , ...)
{
  dots <- eval(substitute(alist(...)))

  if(is.element("cv.subjects",names(dots))){cv.subjects=eval(expression(cv.subjects),envir = sys.frame(sys.nframe()-18))}
  if(is.element("eps",names(dots))){eps=eval(expression(eps),envir = sys.frame(sys.nframe()-18))}
#TODO adapt for all lars arguments. Not useful only eps is used in the call from lasso_cascade<
  if(!missing("K")){K=eval(expression(K),envir = sys.frame(sys.nframe()-18))}
  if(!missing("cv.fun")){cv.fun=eval(expression(cv.folds1),envir = sys.frame(sys.nframe()-18))}
  if(!missing("cv.fun")){cv.folds1=eval(expression(cv.folds1),envir = sys.frame(sys.nframe()-18))}

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
      if(is.element("eps",names(dots))){
        fit <- lars::lars(x[-omit, , drop = FALSE], y[-omit], trace = trace,
                        type = type, eps = eps)
        } else {
        fit <- lars::lars(x[-omit, , drop = FALSE], y[-omit], trace = trace,
                          type = type)
        }
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
