#' @title Boost step by step functions
#'
#' @description Step by step functions to apply the selectboost algorithm.
#'
#' @name boost
#'
#' @return Various types depending on the function.
#' @family Selectboost functions
#' @author Frederic Bertrand, \email{frederic.bertrand@@math.unistra.fr}
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Frédéric Bertrand, \url{https://arxiv.org/abs/1810.01670}
#' @seealso \code{\link{fastboost}}, \code{\link{autoboost}}
#' @examples
#' set.seed(314)
#' xran=matrix(rnorm(200),20,10)
#' yran=rnorm(20)
globalVariables("iforeach")
#> NULL

#' @rdname boost
#'
#' @param X Numerical matrix. Matrix of the variables.
#' @param eps Numerical value. Response vector.
#'
#' @details \code{boost.normalize} returns a numeric matrix whose colun are centered and l2 normalized.
#'
#' @examples
#' xran_norm <- boost.normalize(xran)
#'
#' @export
boost.normalize<-function(X,eps=1e-8){
  tempcolnames <- colnames(X)
  center <- colMeans(X, na.rm = TRUE)
  X <- sweep(X, 2L, center, check.margin = FALSE)
  colnorm2 <- function(v) {
    v <- v[!is.na(v)]
    sqrt(sum(v^2))
  }
  scale <- apply(X, 2L, colnorm2)
  scale[scale<eps] <- 1
  Xnorm <- sweep(X, 2L, scale, "/", check.margin = FALSE)
  #Center columns and then set their SS to 1
  attr(Xnorm,"orig.colnames") <- tempcolnames
  colnames(Xnorm) <- 1:ncol(X)
  return(Xnorm)
}

#' @rdname boost
#'
#' @param Xnorm Numerical matrix. Needs to be centered and l2 normalized.
#' @param corrfunc Character value or function. The function to compute associations between the variables.
#' @param verbose Boolean.
#' Defaults to \code{FALSE}.
#' @param testvarindic Boolean vector. Compute associations for a subset of variables.
#' By default, the scope of the computation is the whole dataset, i.e. \code{rep(TRUE,ncol(Xnorm))}.
#'
#' @details \code{boost.compcorrs} returns a correlation like matrix computed using the \code{corrfunc} function.
#'
#' @examples
#' xran_corr<- boost.compcorrs(xran_norm)
#'
#' @export
boost.compcorrs<-function(Xnorm,corrfunc="cor",verbose=FALSE,testvarindic=rep(TRUE,ncol(Xnorm))){
  if (is.character(corrfunc)){
    tempcorr <- get(corrfunc, mode = "function", envir = parent.frame())
    if(all(testvarindic)){
      Correlation_matrice <- tempcorr(Xnorm)
    }else {
      Correlation_matrice <- tempcorr(Xnorm,Xnorm[,testvarindic])
    }
  }

  if (is.function(corrfunc)){
    if(all(testvarindic)){
      Correlation_matrice <- corrfunc(Xnorm)
    }else {
      Correlation_matrice <- corrfunc(Xnorm,Xnorm[,testvarindic])
    }
  }
  return(Correlation_matrice)
}

#' @rdname boost
#'
#' @param Correlation_matrice Numerical matrix.
# #' @param verbose Boolean.
# #' Defaults to \code{FALSE}.
#' @details \code{boost.Xpass} returns the transformation matrix.
#'
#' @examples
#' xran_corr_sign <- boost.correlation_sign(xran_corr)
#'
#' @export
boost.correlation_sign<-function(Correlation_matrice,verbose=FALSE) {
  return(sign(Correlation_matrice))
}

#' @rdname boost
#'
# #' @param Correlation_matrice Numerical matrix.
#' @param group Character value or function. The grouping function.
#' @param corr Numerical value. Thresholding value. Defaults to \code{1}.
# #' @param verbose Boolean. Compute associations a subset of variables. Defaults to \code{FALSE}.
#'
#' @details \code{boost.findgroups} returns a list of groups or communities found using the \code{group} function.
#'
#' @examples
#' xran_groups <- boost.findgroups(xran_corr, group=group_func_1, .3)
#' xran_groups_2 <- boost.findgroups(xran_corr, group=group_func_2, .3)
#'
#' @export
boost.findgroups<-function(Correlation_matrice,group,corr=1,verbose=FALSE){

  if (is.character(group)){
    tempgroup <- get(group, mode = "function", envir = parent.frame())
    group2 <- function(x) return(tempgroup(x,corr))
  }

  if (is.function(group)){
    group2 <- function(x) return(group(x,corr))
  }

#  Correlation_sign<-sign(Correlation_matrice)
  #diag(Correlation_sign)<-1
  groups<-group2(Correlation_matrice)

  if(attr(groups,"type")=="compact"){
    length.communities=lapply(groups$communities,length)
    length.groups=length.communities[unlist((groups)[setdiff(names(groups),"communities")])]
  } else {
    length.groups=sapply(groups,length)
  }
  attr(groups,"length.groups") <- unlist(length.groups)

  if(verbose){
    cat("Number of correlated variables :", attr(groups,"length.groups"), "\n")
    cat("Number of variables :", ncol(Correlation_matrice), "\n")
  }

  return(list(groups=groups
              #,Correlation_sign=Correlation_sign
              ))
}

#' @rdname boost
#'
#' @param nrowX Numerical value
#' @param ncolX Numerical value.
#'
#' @details \code{boost.Xpass} returns the transformation matrix.
#'
#' @examples
#' xran_Xpass <- boost.Xpass(nrow(xran_norm),ncol(xran_norm))
#'
#' @export
boost.Xpass<-function(nrowX,ncolX){
  Xpass<-matrix(0,nrowX,nrowX-1)
  Xpass[col(Xpass)>=row(Xpass)]<-1
  Xpass[col(Xpass)==(row(Xpass)-1)]<--(1:(nrowX-1))
  colnorm2 <- function(v) {
    v <- v[!is.na(v)]
    sqrt(sum(v^2))
  }
  scale <- apply(Xpass, 2L, colnorm2)
  Xpass <- sweep(Xpass, 2L, scale, "/", check.margin = FALSE)
  return(Xpass)
}

#' @rdname boost
#'
# #' @param X Numerical matrix. l2 normed matrix.
#' @param groups List. List of groups or communities (compact form).
#' @param Correlation_sign Numerical -1/1 matrix.
#' @param Xpass Numerical value. Transformation matrix.
#' Defaults to \code{boost.Xpass(nrowX,ncolX)}, with \code{nrowX=nrow(X)} and \code{ncolX=ncol(X)}.
# #' @param verbose Boolean.
# #' Defaults to \code{FALSE}.
#' @param use.parallel Boolean.
#' Defaults to \code{FALSE}.
#' @param ncores Numerical value. Number of cores to use.
#' Defaults to \code{4}.
#'
#' @details \code{boost.adjust} returns the list of the parameters ot the fitted von-Mises distributions.
#'
#' @examples
#' xran_adjust <- boost.adjust(xran_norm, xran_groups$groups, xran_corr_sign)
#'
#' @export
boost.adjust<-function(X,groups,Correlation_sign,Xpass=boost.Xpass(nrowX,ncolX),verbose=FALSE,use.parallel=FALSE,ncores=4) {
  if(attr(groups,"type")=="compact"){
    communities=groups$communities
    groups$communities <- NULL
    listnames=vector("list",length(groups))
  }
  ngroups=length(groups)
  nrowX=nrow(X)
  ncolX=ncol(X)
#  Xpass <- boost.Xpass(nrowX,ncolX)

  func_passage1<-function(x){
    return(crossprod(Xpass,x))
  }

  corr_set0<-func_passage1(X) #t(Xpass)%*%X
  fit1<-function(j){
    if(attr(groups,"length.groups")[j]>=2){
      if(verbose){
        cat(paste(j,": Random","\n"))
      }
      if(attr(groups,"type")=="compact"){
        indice<-unlist((communities)[groups[[j]]])
      } else {
        indice<-groups[[j]]
      }
      corr_set2<-sweep(corr_set0[,indice,drop=FALSE],2L,Correlation_sign[indice,j],"*")
      return(vmf.mle(t(corr_set2)))
    }else{
      if(verbose){
        print(paste(j,": NoRandom","\n"))
      }
      return("NoRandom")
    }
  }
  fit1<-Vectorize(fit1, SIMPLIFY = FALSE)

  if(use.parallel) {
    requireNamespace("doParallel")
    if (.Platform$OS.type != "windows") {
      workers=parallel::makeForkCluster(nnodes = ncores)
    }
    else {
      workers=parallel::makePSOCKcluster(names = ncores)
    }
    doParallel::registerDoParallel(workers)
    vmf.params=foreach(iforeach=1:ngroups, .combine=c, .errorhandling = 'remove', .verbose = verbose) %dopar% fit1(iforeach)
    parallel::stopCluster(workers)

  } else {
    vmf.params=fit1(1:ngroups)
  }

  if(attr(groups,"type")=="compact"){
    listnames=vector("list",ngroups)
    for(lll in 1:ngroups){
      listnames[[lll]]<-unlist((communities)[groups[[lll]]])
    }
    names(vmf.params) <- unlist(lapply(listnames,paste,collapse="."))
  } else {
    names(vmf.params) <- lapply(groups,paste,collapse=".")
  }

  return(list(Xpass=Xpass,vmf.params=vmf.params))
}

#' @rdname boost
#'
# #' @param X Numerical matrix. l2 normed matrix.
# #' @param Xpass Numerical value. Transformation matrix.
#' @param vmf.params List. List of the parameters ot the fitted von-Mises distributions.
# #' @param verbose Boolean.
# #' Defaults to \code{FALSE}.
#' @param B Integer value. Number of resampling.
# #' @param use.parallel Boolean.
# #' @param ncores Numerical value. Number of cores to use.
#'
#' @details \code{boost.random} returns an array with the resampled datasets.
#'
#' @examples
#' #Not meaningful, should be run with B>=100
#' xran_random <- boost.random(xran_norm, xran_Xpass, xran_adjust$vmf.params, B=5)
#'
#' \dontrun{
#' xran_random <- boost.random(xran_norm, xran_Xpass, xran_adjust$vmf.params, B=100)
#' }
#'
#' @export
boost.random<-function(X,Xpass,vmf.params,verbose=FALSE,B=100,use.parallel=FALSE,ncores=4) {
  nvmf.params=length(vmf.params)
  nrowX=nrow(X)
  ncolX=ncol(X)

  func_passage2<-function(x){
    return(Xpass%*%t(x)) #tcrossprod(Xpass,x)
  }

  findcols<-function(j){
    if(is.list(vmf.params[[j]])){
      return(j)
    }
  }

  colstosimul<-unlist(lapply(1:nvmf.params,findcols))

  simul1<-function(j){
    newv<-rvmf(1,mu=vmf.params[[j]]$mu,k=vmf.params[[j]]$kappa)
    newv<-func_passage2(newv) #Xpass%*%t(newv)
    return(newv)
  }

  simul1<-Vectorize(simul1)
  if(B>1){
    simul2<-function(){simul1(colstosimul)}
    if(use.parallel & !is.null(colstosimul)) {
      requireNamespace("doParallel")
      if (.Platform$OS.type != "windows") {
        workers=parallel::makeForkCluster(nnodes = ncores)
      }
      else {
        workers=parallel::makePSOCKcluster(names = ncores)
      }
      registerDoParallel(workers)
      res<-foreach(iforeach=1:B, .combine=list, .multicombine=TRUE, .errorhandling = 'remove', .verbose = verbose) %dopar% simul2()
      parallel::stopCluster(workers)
      res<-simplify2array(res)
    } else {
      res<-replicate(B,simul2())
    }
  } else {
    if(use.parallel & !is.null(colstosimul)) {
      requireNamespace("doParallel")
      if (.Platform$OS.type != "windows") {
        workers=parallel::makeForkCluster(nnodes = ncores)
      }
      else {
        workers=parallel::makePSOCKcluster(names = ncores)
      }
      registerDoParallel(workers)
      res<-foreach(iforeach=colstosimul, .combine=cbind, .errorhandling = 'remove', .verbose = verbose) %dopar% simul1(iforeach)
      parallel::stopCluster(workers)
    } else {
      res<-simul1(colstosimul)
    }
  }
  if(is.null(colstosimul)){
    colstosimul<-NA
    attr(res,"nosimul")<-TRUE
  } else {
    attr(res,"nosimul")<-FALSE
  }
  attr(res,"colstosimul")<-colstosimul
  attr(res,"nsimul")<-B
  return(res)
}

#' @rdname boost
#'
# #' @param X Numerical matrix. l2 normed matrix of predictors.
#' @param cols.simul Numerical value. Transformation matrix.
#' @param Y Numerical vector or factor. Response.
#' @param func Function. Variable selection function.
# #' @param verbose Boolean.
# #' Defaults to \code{FALSE}.
# #' @param use.parallel Boolean.
# #' Defaults to \code{FALSE}.
# #' @param ncores Numerical value. Number of cores to use.
# #' Defaults to \code{4}.
#' @param ... . Additionnal parameters passed to the \code{func} function.
#'
#' @details \code{boost.apply} returns a matrix with the coefficients estimated using the resampled datasets.
#'
#' @examples
#' xran_apply <- boost.apply(xran_norm, xran_random, yran, lasso_msgps_AICc)
#'
#' @export
boost.apply<-function(X,cols.simul,Y,func,verbose=FALSE,use.parallel=FALSE,ncores=4,exportlist=NULL,...){

dots <- eval(substitute(alist(...)))

if (is.character(func)){
  funcgroup <- get(func, mode = "function", envir = parent.frame())
}
if (is.function(func)){
  funcgroup <- func
}

if(attr(cols.simul,"nosimul")) {
    if(attr(cols.simul,"nsimul")==1) {
      return(do.call(what=funcgroup, args=c(X=list(X),Y=list(Y), dots)))
    } else {
      applyfunction<-function(k){
        return(do.call(what=funcgroup, args=c(X=list(X),Y=list(Y), dots)))
      }
      if(use.parallel) {
        requireNamespace("doParallel")
        if (.Platform$OS.type != "windows") {
          workers=parallel::makeForkCluster(nnodes = ncores)
        }
        else {
          workers=parallel::makePSOCKcluster(names = ncores)
        }
        registerDoParallel(workers)
        resres <- foreach(iforeach=1:attr(cols.simul,"nsimul"), .combine=cbind, .errorhandling = 'remove', .verbose = verbose) %dopar% applyfunction(iforeach)
        parallel::stopCluster(workers)
        return(resres)
      } else {
        return(sapply(1:attr(cols.simul,"nsimul"),applyfunction))
      }
    }
  } else {
    if(attr(cols.simul,"nsimul")==1) {
      X[,attr(cols.simul,"colstosimul")] <-cols.simul
      return(do.call(what=funcgroup, args=c(X=list(X),Y=list(Y),dots)))
    } else {
      applyfunction<-function(k){
        X[,attr(cols.simul,"colstosimul")] <-cols.simul[,,k]
        return(do.call(what=funcgroup, args=c(X=list(X),Y=list(Y),dots)))
      }
      if(use.parallel) {
        requireNamespace("doParallel")
        if (.Platform$OS.type != "windows") {
          workers=parallel::makeForkCluster(nnodes = ncores)
        }
        else {
          workers=parallel::makePSOCKcluster(names = ncores)
        }
        registerDoParallel(workers)
        resres <- foreach(iforeach=1:attr(cols.simul,"nsimul"), .combine=cbind, .errorhandling = 'remove', .verbose = verbose, .export="X") %dopar% applyfunction(iforeach)
        parallel::stopCluster(workers)
        return(resres)
      } else {
        return(sapply(1:attr(cols.simul,"nsimul"),applyfunction))
      }
    }
  }
}

#' @rdname boost
#'
#' @param Boost.coeffs Numerical matrix. l2 normed matrix of predictors.
# #' @param eps Numerical value.
# #' Defaults to \code{eps=10^(-4)}.
#' @param version Character value. "lars" (no intercept value) or "glmnet" (first coefficient is the intercept value).
# #' @param verbose Boolean.
# #' Defaults to \code{FALSE}.
#'
#' @details \code{boost.select} returns a vector with the proportion of times each variable was selected.
#'
#' @examples
#' xran_select <- boost.select(xran_apply)
#'
#' @export
boost.select<-function(Boost.coeffs,eps=10^(-4),version="lars",verbose=FALSE){
  nsim=ncol(Boost.coeffs)
  if(version=="glmnet") {
    Boost.coeffs<-Boost.coeffs[-1,]

  }
  if(is.null(dim(Boost.coeffs))){Boost.coeffs<-as.matrix(Boost.coeffs)
  Fs<-apply(abs(Boost.coeffs)>eps,1,sum)
  return(Fs)} else{
  Fs<-apply(abs(Boost.coeffs)>eps,1,sum)
#  names(Fs)<-NULL
  return(Fs/nsim)
  }
}

