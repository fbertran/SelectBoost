#' @title Autoboost
#'
#' @description All in one use of selectboost that avoids redondant fitting of distributions
#' and saves some memory.
#'
#' @name autoboost
#'
#' @param X Numerical matrix. Matrix of the variables.
#' @param Y Numerical vector or factor. Response vector.
#' @param ncores Numerical value. Number of cores for parallel computing.
#' Defaults to \code{4}.
#' @param group Function. The grouping function.
#' Defaults to \code{group_func_1}.
#' @param func Function. The variable selection function.
#' Defaults to \code{lasso_msgps_AICc}.
#' @param corrfunc Character value or function. Used to compute associations between
#' the variables. Defaults to \code{"cor"}.
#' @param use.parallel Boolean. Use parallel computing (doParallel).
#' Defaults to \code{TRUE}.
#' @param B Numerical value. Number of resampled fits of the model.
#' Defaults to \code{100}.
#' @param step.num Numerical value. Step value for the c0 sequence.
#' Defaults to \code{0.1}.
#' @param step.limit Character value. If "Pearson", truncates the c0 sequence using a
#' Pearson based p-value.
#' Defaults to \code{"none"}.
#' @param risk Numerical value. Risk level when finding limits based on c0=0 values.
#' Defaults to \code{0.05}.
#' @param verbose Boolean.
#' Defaults to \code{FALSE}.
#' @param step.scale Character value. How to compute the c0 sequence if not user-provided:
#' either "quantile" or "linear".
#' Defaults to \code{"quantile"}.
#' @param normalize Boolean. Shall the X matrix be centered and scaled?
#' Defaults to \code{TRUE}.
#' @param steps.seq Numeric vector. User provided sequence of c0 values to use.
#' Defaults to \code{NULL}.
#' @param debug Boolean value. If more results are required. Defaults to \code{FALSE}.
#' @param version Character value. Passed to the \code{boost.select} function.
#' Defaults to \code{lars}
#' @param ... . Arguments passed to the variable selection function used in \code{boost.apply}.
#'
#' @return A numeric matrix with attributes.
#' @family Selectboost functions
#' @author Frederic Bertrand, \email{frederic.bertrand@@math.unistra.fr}
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Frédéric Bertrand, \url{https://arxiv.org/abs/1810.01670}
#' @seealso \code{\link{boost}}, \code{\link{fastboost}}, \code{\link{plot.selectboost}}
#' @examples
#' set.seed(314)
#' xran=matrix(rnorm(75),15,5)
#' ybin=sample(0:1,15,replace=TRUE)
#' yran=rnorm(15)
NULL
#> NULL

#' @rdname autoboost
#'
#' @details \code{autoboost} returns a numeric matrix. For each of the variable (column)
#' and each of the c0 (row), the entry is proportion of times that the variable was
#' selected among the B resampled fits of the model. Fitting to the same group of variables is
#' only perfomed once (even if it occured for another value of c0), which greatly speeds up
#' the algorithm.
#'
#' @examples
#' set.seed(314)
#' #For quick test purposes, not meaningful, should be run with greater value of B
#' #and disabling parallel computing as well
#' res.autoboost <- autoboost(xran,yran,B=3,use.parallel=FALSE)
#'
#' \donttest{
#' autoboost(xran,yran)
#' #Customize resampling levels
#' autoboost(xran,yran,steps.seq=c(.99,.95,.9))
#'
#' #Binary logistic regression
#' autoboost(xran,ybin,func=lasso_cv_glmnet_bin_min)
#'}
#'
#' @export
autoboost<-function(X,Y,ncores=4,group=group_func_1,func=lasso_msgps_AICc,corrfunc="cor",use.parallel=TRUE,B=100,step.num=0.1,step.limit="none",risk=0.05,verbose=FALSE,step.scale="quantile",normalize=TRUE,steps.seq=NULL,debug=FALSE,version="lars",...){

if(use.parallel){
  requireNamespace("doParallel")
  if (.Platform$OS.type != "windows") {
    workers=parallel::makeForkCluster(nnodes = ncores)
  }
  else {
    workers=parallel::makePSOCKcluster(names = ncores)
  }
  registerDoParallel(workers)
}

if(normalize){
  if(verbose){cat("Normalizing X","\n")}
  X<-boost.normalize(X)
}

#Compute the corr matrix
if(verbose){cat("Computing X cors","\n")}

n_corr <- crossprod(!is.na(X))
if(step.limit=="Pearson"){
  #Step limit Pearson only with Pearson corr
  X_corr <- cor(X, use = "pairwise.complete.obs") # MUCH MUCH faster than corr.test()
} else {
  X_corr <- boost.compcorrs(X,corrfunc=corrfunc,verbose=FALSE)
}

tmpcorrelation_sign <- boost.correlation_sign(X_corr)

diag(X_corr)<-NA

#if(verbose){print(X_corr)}

#Compute the step sequence
if(step.limit=="Pearson"){ #Step limit Pearson only with Pearson corr
  # https://stackoverflow.com/questions/13112238/a-matrix-version-of-cor-test
  # find (pairwise complete) correlation matrix between two matrices x and y
  # compare to corr.test(x, y, adjust = "none")
  cor2pvalue = function(r, n) {
    t <- (r*sqrt(n-2))/sqrt(1-r^2)
    p <- 2*(1 - pt(abs(t),(n-2)))
    se <- sqrt((1-r*r)/(n-2))
    out <- list(r, p)
    names(out) <- c("r.cor", "p.cor")
    #    out <- list(r, n, t, p, se)
    #    names(out) <- c("r.cor", "n.cor", "t.cor", "p.cor", "se.cor")
    return(out)
  }
  # get a list with matrices of correlation, pvalues, standard error, etc.
  if(verbose){cat("Computing cor2pvalue","\n")}
  result.cor = cor2pvalue(X_corr,n_corr)
  #Bonferroni corrected Pearson correlation (do not take into account self correlations)
  result.cor$p.cor<.05/(nrow(n_corr)*(nrow(n_corr)-1)/2)

  steps<-abs(result.cor$r.cor[as.vector(result.cor$p.cor<.05/(nrow(n_corr)*(nrow(n_corr)-1)/2))])
  steps<-steps[order(steps)]
  steps<-steps[!is.na(steps)]
  steps <-unique(steps)
}

if(step.limit=="none"){
  #if(sum(result_sans)!=0){
  #X_corr<-X_corr[,which(result_sans==1)]
  #}
  steps<-c(abs(X_corr))
  steps<-steps[order(steps)]
  steps<-steps[!is.na(steps)]
  steps <-unique(steps)
}

if(is.null(steps.seq)){
  custom.steps=FALSE
  etape<-max(step.num,1/(length(steps)-2))
  steps.seq<-rev(unique(c(0,seq(from=0,to=1,by=etape),1)))
if(step.scale=="quantile"){
  c0.seq=round(quantile(steps,pmax(steps.seq,0)),6)
}
if(step.scale=="linear"){
  if(step.limit=="Pearson"){
    cat("Option Pearson limit for correlation is not used with a linear scale.")
  }
  c0.seq=pmax(round(steps.seq[-c(1,length(steps.seq))],6),0)
}
} else {
  custom.steps=TRUE
  steps.seq <- unique(c(0,sort(steps.seq, decreasing = TRUE),1))
  c0.seq = pmax(round(steps.seq[-c(1,length(steps.seq))],6),0)
}

diag(X_corr)<-1
P<-dim(X)[2]
if(verbose){cat("Computing Xpass","\n")}
Xpass <- boost.Xpass(nrow(X),ncol(X))
result<-NULL
knowgroups=NULL
knowdist=NULL
qq<-1
kk<-1
ppp<-0

#Sequence of c0s
if(verbose){
  cat("c0.seq","\n")
  cat(1:length(steps.seq),"\n")
  cat(names(c0.seq),"\n")
  cat(c0.seq,"\n")
  cat("++++++++++++ B =",B,"++++++++++++","\n")
  cat("++++++++++++ Start loop ++++++++++++","\n")
  #print(c0.seq)
}

for(iii in c(1,0,c0.seq)){#,rev(steps)
  if(verbose){
    cat("++++++++++++ c0 =",iii,"++++++++++++","\n")
    if(!(iii %in% c(1,0))){
      cat("max",result[dim(result)[1],which(result_sans==1)],"\n")
      cat("limi_alea",limi_alea[which(result_sans==1)],"\n")
      cat("ppp",ppp,"\n")
      cat("kk",kk,"\n")
      cat("qq",qq,"\n")
      cat("quantile(steps,qq)",quantile(steps,qq),"\n")
      cat("c0.seq[kk]",iii,"\n")
    }
  }
  if(verbose){cat("Computing X groups","\n")}
  tmpgroups=boost.findgroups(X_corr,group=group,corr=iii)
  tmpgroups.orig=tmpgroups$groups
  if(attr(tmpgroups$groups,"type") %in% c("compact")){
    tmpcommunities=tmpgroups.orig$communities
    tmpgroups$groups$communities <- NULL
    tmpgroups.orig$communities <- NULL
    distduplicated=duplicated(unlist(lapply(tmpcommunities,paste,collapse=".")))
    distknown=(unlist(lapply(tmpcommunities,paste,collapse=".")) %in% knowgroups)
  } else {
    distduplicated=duplicated(unlist(lapply(tmpgroups$groups,paste,collapse=".")))
    distknown=(unlist(lapply(tmpgroups$groups,paste,collapse=".")) %in% knowgroups)
  }
  distknownordup=distknown|distduplicated
  distnotknownordup=!distknownordup
  if(attr(tmpgroups$groups,"type") %in% c("compact")){
    attr(tmpcommunities,"type") <- "normal"
    length.communities=lapply(tmpcommunities,length)
    attr(tmpcommunities,"length.groups") <- unlist(length.communities)
    } else {
    tempattr <- attributes(tmpgroups$groups)
    tmpgroups$groups[distknownordup]<-as.list(1:length(colnames(X_corr)))[distknownordup]
    tmp.tmpgroups.groups <- attr(tmpgroups$groups,"length.groups")
    tmp.tmpgroups.groups[distknownordup] <- 1
    attr(tmpgroups$groups,"length.groups") <- tmp.tmpgroups.groups
    if(is.null(attr(tmpgroups$groups,"type"))){
      attributes(tmpgroups$groups) <- tempattr
      tmp.tmpgroups.groups <- attr(tmpgroups$groups,"length.groups")
      tmp.tmpgroups.groups[distknownordup] <- 1
      attr(tmpgroups$groups,"length.groups") <- tmp.tmpgroups.groups
  }
  }
  if(verbose){cat("Computing fitting distributions","\n")}
  if(attr(tmpgroups$groups,"type") %in% c("compact")){
    knowdist=c(knowdist,(boost.adjust(X,tmpcommunities,tmpcorrelation_sign,Xpass=Xpass,use.parallel=use.parallel,ncores=ncores)$vmf.params)[distnotknownordup])
    knowgroups=c(knowgroups,(unlist(lapply(tmpcommunities,paste,collapse=".")))[distnotknownordup])
  }else{
      knowdist=c(knowdist,(boost.adjust(X,tmpgroups$groups,tmpcorrelation_sign,Xpass=Xpass,use.parallel=use.parallel,ncores=ncores)$vmf.params)[distnotknownordup])
      knowgroups=c(knowgroups,(unlist(lapply(tmpgroups$groups,paste,collapse=".")))[distnotknownordup])
    }
    if(verbose){
      if(sum(distnotknownordup)!=0){
        if(attr(tmpgroups$groups,"type") %in% c("compact")){
          cat("new:",(unlist(lapply(tmpcommunities,paste,collapse=".")))[distnotknownordup],"\n")
        } else {
        cat("new:",(unlist(lapply(tmpgroups$groups,paste,collapse=".")))[distnotknownordup],"\n")
        }
      cat("knowgroups:",knowgroups,"\n")
      } else {
        cat("no new group.","\n")
      }
    }
  if(verbose){cat("Generating X random sets","\n")}
  if(attr(tmpgroups$groups,"type") %in% c("compact")){
    distforcommunities=unlist(lapply(tmpcommunities,paste,collapse="."))
    distforgroups=rep(NA,length(tmpgroups.orig))
    for(mmm in 1:length(tmpgroups.orig)){
      distforgroups[mmm]<-distforcommunities[tmpgroups.orig[[mmm]]]
    }
  } else {
    distforgroups=unlist(lapply(tmpgroups.orig,paste,collapse="."))
  }
  Xrandom=boost.random(X,Xpass,knowdist[distforgroups],B=B,use.parallel=use.parallel,ncores=ncores)
  if(verbose){cat("Applying to random sets","\n")}
  result=rbind(result,boost.select(boost.apply(X,Xrandom,Y,func=func,use.parallel=use.parallel,ncores=ncores,...),version=version))
  if(verbose){cat("Finalizing functions","\n")}
  row.names(result)[dim(result)[1]]<-paste("c0 =",round(iii,3))
  if(iii==1){result_sans<-result[1,,drop=FALSE]}
  if(iii==0){result_alea<-result[2,,drop=FALSE]
  limi_alea<-qbinom(risk,B,(result_alea))/B
  if(verbose){
    cat("sum(result_sans)!=0",sum(result_sans)!=0,"\n")
  }
  }

  if(!(iii %in% c(1,0))){
    ppp<-max(apply(result[,which(result_sans==1),drop=FALSE],2,min))
    if(length(which(result_sans==1))>1){
      if(custom.steps){
        tempcond=(any(mapply('>', result[dim(result)[1],which(result_sans==1)], limi_alea[which(result_sans==1)])) ||  ppp>min(limi_alea))&&(kk<=length(steps.seq)+1)
      } else {
        tempcond=(any(mapply('>', result[dim(result)[1],which(result_sans==1)], limi_alea[which(result_sans==1)])) ||  ppp>min(limi_alea))&&(kk<=length(steps.seq)+1)&&(qq-etape>=0)
        qq<-qq-etape
      }
      if(verbose){
        cat("cond:",tempcond,"\n")
      }
      if(!tempcond){break}
        kk<-kk+1

  }else{
    ppp<-max(result[dim(result)[1],])
    if(custom.steps){
      tempcond=(kk<=length(steps.seq)+1)
    } else {
      tempcond=(kk<=length(steps.seq)+1)&&(qq-etape)>=0
      qq<-qq-etape
    }
    if(verbose){
      cat("cond:",tempcond,"\n")
    }
    if(!tempcond){break}
    kk<-kk+1

  }
  }

}
if(nrow(result)>2){
  result<-result[c(c(1,3:nrow(result)),2),]
}

if(use.parallel){
  parallel::stopCluster(workers)
}

if(debug){
  attr(result,"dimX")<-dim(X)
  attr(result,"Xrandom")<-Xrandom
  attr(result,"X_corr")<-X_corr
  attr(result,"Xpass")<-Xpass
  attr(result,"tmpcorrelation_sign")<-tmpcorrelation_sign
}
attr(result,"c0.seq")<-c(1,c0.seq,0)
attr(result,"steps.seq")<-steps.seq
attr(result,"typeboost")<-"autoboost"
attr(result,"limi_alea")<-NA
attr(result,"B")<-B

class(result) <- "selectboost"
return(result)
}


#' @title Fastboost
#'
#' @description All in one use of selectboost that prevents redondant fitting of distributions
#' and saves some memory.
#'
#' @name fastboost
#'
#' @param X Numerical matrix. Matrix of the variables.
#' @param Y Numerical vector or factor. Response vector.
#' @param ncores Numerical value. Number of cores for parallel computing.
#' Defaults to \code{4}.
#' @param group Function. The grouping function.
#' Defaults to \code{group_func_1}.
#' @param func Function. The variable selection function.
#' Defaults to \code{lasso_msgps_AICc}.
#' @param corrfunc Character value or function. Used to compute associations between
#' the variables. Defaults to \code{"cor"}.
#' @param use.parallel Boolean. Use parallel computing (doParallel).
#' Defaults to \code{TRUE}.
#' @param B Numerical value. Number of resampled fits of the model.
#' Defaults to \code{100}.
#' @param step.num Numerical value. Step value for the c0 sequence.
#' Defaults to \code{0.1}.
#' @param step.limit
#' Defaults to \code{"none"}.
#' @param verbose Boolean.
#' Defaults to \code{FALSE}.
#' @param step.scale Character value. How to compute the c0 sequence if not user-provided:
#' either "quantile" or "linear".
#' Defaults to \code{"quantile"}.
#' @param normalize Boolean. Shall the X matrix be centered and scaled?
#' Defaults to \code{TRUE}.
#' @param steps.seq Numeric vector. User provided sequence of c0 values to use.
#' Defaults to \code{NULL}.
#' @param debug Boolean value. If more results are required. Defaults to \code{FALSE}.
#' @param version Character value. Passed to the \code{boost.select} function.
#' Defaults to \code{lars}
#' @param c0lim Boolean. Shall the c0=0 and c0=1 values be used?
#' Defaults to \code{TRUE}
#' @param ... . Arguments passed to the variable selection function used in \code{boost.apply}.
#'
#' @return A numeric matrix with attributes.
#' @family Selectboost functions
#' @author Frederic Bertrand, \email{frederic.bertrand@@math.unistra.fr}
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Frédéric Bertrand, \url{https://arxiv.org/abs/1810.01670}
#' @seealso \code{\link{boost}}, \code{\link{autoboost}}, \code{\link{plot.selectboost}}
#' @examples
#' set.seed(314)
#' xran=matrix(rnorm(75),15,5)
#' ybin=sample(0:1,15,replace=TRUE)
#' yran=rnorm(15)
NULL
#> NULL

#' @rdname fastboost
#'
#' @details \code{fastboost} returns a numeric matrix. For each of the variable (column)
#' and each of the c0 (row), the entry is proportion of times that the variable was
#' selected among the B resampled fits of the model. Fitting to the same group of variables is
#' only perfomed once (even if it occured for another value of c0), which greatly speeds up
#' the algorithm. In order to limit memory usage, \code{fastboost} uses a compact way to
#' save the group memberships, which is especially useful with community grouping function
#' and fairly big datasets.
#'
#' @examples
#' set.seed(314)
#' #For quick test purpose, not meaningful, should be run with greater value of B
#' #and disabling parallel computing as well
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
#' @export
fastboost<-function(X,Y,ncores=4,group=group_func_1,func=lasso_msgps_AICc,corrfunc="cor",use.parallel=TRUE,B=100,step.num=0.1,step.limit="none",verbose=FALSE,step.scale="quantile",normalize=TRUE,steps.seq=NULL,debug=FALSE,version="lars",c0lim=TRUE,...){
  # ncores=4
  # group=group_func_1
  # func=lasso_msgps_AICc
  # corrfunc="cor"
  # use.parallel=TRUE
  # B=100
  # step.num=0.1
  # step.limit="none"
  # risk=0.05
  # verbose=FALSE
  # step.scale="quantile"
  # normalize=TRUE

  if(use.parallel){
    requireNamespace("doParallel")
    if (.Platform$OS.type != "windows") {
      workers=parallel::makeForkCluster(nnodes = ncores)
    }
    else {
      workers=parallel::makePSOCKcluster(names = ncores)
    }
    registerDoParallel(workers)
  }

  if(normalize){
    if(verbose){cat("Normalizing X","\n")}
    X<-boost.normalize(X)
  }

  #Compute the corr matrix
  if(verbose){cat("Computing X cors","\n")}

  n_corr <- crossprod(!is.na(X))
  if(step.limit=="Pearson"){
    #Step limit Pearson only with Pearson corr
    X_corr <- cor(X, use = "pairwise.complete.obs") # MUCH MUCH faster than corr.test()
  } else {
    X_corr <- boost.compcorrs(X,corrfunc=corrfunc,verbose=FALSE)
  }

  tmpcorrelation_sign <- boost.correlation_sign(X_corr)

  diag(X_corr)<-NA

  #if(verbose){print(X_corr)}

  #Compute the step sequence
  if(step.limit=="Pearson"){ #Step limit Pearson only with Pearson corr
    # https://stackoverflow.com/questions/13112238/a-matrix-version-of-cor-test
    # find (pairwise complete) correlation matrix between two matrices x and y
    # compare to corr.test(x, y, adjust = "none")
    cor2pvalue = function(r, n) {
      t <- (r*sqrt(n-2))/sqrt(1-r^2)
      p <- 2*(1 - pt(abs(t),(n-2)))
      se <- sqrt((1-r*r)/(n-2))
      out <- list(r, p)
      names(out) <- c("r.cor", "p.cor")
      #    out <- list(r, n, t, p, se)
      #    names(out) <- c("r.cor", "n.cor", "t.cor", "p.cor", "se.cor")
      return(out)
    }
    # get a list with matrices of correlation, pvalues, standard error, etc.
    if(verbose){cat("Computing cor2pvalue","\n")}
    result.cor = cor2pvalue(X_corr,n_corr)
    #Bonferroni corrected Pearson correlation (do not take into account self correlations)
    result.cor$p.cor<.05/(nrow(n_corr)*(nrow(n_corr)-1)/2)

    steps<-abs(result.cor$r.cor[as.vector(result.cor$p.cor<.05/(nrow(n_corr)*(nrow(n_corr)-1)/2))])
    steps<-steps[order(steps)]
    steps<-steps[!is.na(steps)]
    steps <-unique(steps)
  }

  if(step.limit=="none"){
    #if(sum(result_sans)!=0){
    #X_corr<-X_corr[,which(result_sans==1)]
    #}
    steps<-c(abs(X_corr))
    steps<-steps[order(steps)]
    steps<-steps[!is.na(steps)]
    steps <-unique(steps)
  }

  if(is.null(steps.seq)){
    custom.steps=FALSE
    etape<-max(step.num,1/(length(steps)-2))
    steps.seq<-rev(unique(c(0,seq(from=0,to=1,by=etape),1)))
    if(step.scale=="quantile"){
      c0.seq=round(quantile(steps,pmax(steps.seq,0)),6)
    }
    if(step.scale=="linear"){
      if(step.limit=="Pearson"){
        cat("Option Pearson limit for correlation is not used with a linear scale.")
      }
      c0.seq=pmax(round(steps.seq[-c(1,length(steps.seq))],6),0)
    }
  } else {
    custom.steps=TRUE
    steps.seq <- unique(c(0,sort(steps.seq, decreasing = TRUE),1))
    c0.seq = pmax(round(steps.seq[-c(1,length(steps.seq))],6),0)
  }

  diag(X_corr)<-1
  P<-dim(X)[2]
  if(verbose){cat("Computing Xpass","\n")}
  Xpass <- boost.Xpass(nrow(X),ncol(X))
  result<-NULL
  knowgroups=NULL
  knowdist=NULL
  qq<-1
  kk<-1
  ppp<-0

  #Sequence of c0s
  if(verbose){
    cat("c0.seq","\n")
    cat(1:length(steps.seq),"\n")
    cat(names(c0.seq),"\n")
    cat(c0.seq,"\n")
    cat("++++++++++++ B =",B,"++++++++++++","\n")
    cat("++++++++++++ Start loop ++++++++++++","\n")
    #print(c0.seq)
  }

  if(c0lim){valc0for<-c(1,0,c0.seq)} else {valc0for<-c0.seq}
  for(iii in valc0for){#,rev(steps)
    if(verbose){
      cat("++++++++++++ c0 =",iii,"++++++++++++","\n")
      if(!(iii %in% c(1,0))){
        if(!custom.steps){
#          cat("max",result[dim(result)[1],which(result_sans==1)],"\n")
#          cat("limi_alea",limi_alea[which(result_sans==1)],"\n")
          cat("ppp",ppp,"\n")
        }
        cat("kk",kk,"\n")
        if(!custom.steps){
          cat("qq",qq,"\n")
          cat("quantile(steps,qq)",quantile(steps,qq),"\n")
        }
        cat("c0.seq[kk]",iii,"\n")
      }
    }
    if(verbose){cat("Computing X groups","\n")}
    tmpgroups=boost.findgroups(X_corr,group=group,corr=iii)
    tmpgroups.orig=tmpgroups$groups
    if(attr(tmpgroups$groups,"type") %in% c("compact")){
      tmpcommunities=tmpgroups.orig$communities
      tmpgroups$groups$communities <- NULL
      tmpgroups.orig$communities <- NULL
      distduplicated=duplicated(unlist(lapply(tmpcommunities,paste,collapse=".")))
      distknown=(unlist(lapply(tmpcommunities,paste,collapse=".")) %in% knowgroups)
    } else {
      distduplicated=duplicated(unlist(lapply(tmpgroups$groups,paste,collapse=".")))
      distknown=(unlist(lapply(tmpgroups$groups,paste,collapse=".")) %in% knowgroups)
    }
    distknownordup=distknown|distduplicated
    distnotknownordup=!distknownordup
    if(attr(tmpgroups$groups,"type") %in% c("compact")){
      attr(tmpcommunities,"type") <- "normal"
      length.communities=lapply(tmpcommunities,length)
      attr(tmpcommunities,"length.groups") <- unlist(length.communities)
    } else {
      tempattr <- attributes(tmpgroups$groups)
      tmpgroups$groups[distknownordup]<-as.list(1:length(colnames(X_corr)))[distknownordup]
      tmp.tmpgroups.groups <- attr(tmpgroups$groups,"length.groups")
      tmp.tmpgroups.groups[distknownordup] <- 1
      attr(tmpgroups$groups,"length.groups") <- tmp.tmpgroups.groups
      if(is.null(attr(tmpgroups$groups,"type"))){
        attributes(tmpgroups$groups) <- tempattr
        tmp.tmpgroups.groups <- attr(tmpgroups$groups,"length.groups")
        tmp.tmpgroups.groups[distknownordup] <- 1
        attr(tmpgroups$groups,"length.groups") <- tmp.tmpgroups.groups
      }
    }
    if(verbose){cat("Fitting distributions","\n")}
    if(attr(tmpgroups$groups,"type") %in% c("compact")){
      knowdist=c(knowdist,(boost.adjust(X,groups=tmpcommunities,Correlation_sign=tmpcorrelation_sign,Xpass=Xpass,use.parallel=use.parallel,ncores=ncores)$vmf.params)[distnotknownordup])
      knowgroups=c(knowgroups,(unlist(lapply(tmpcommunities,paste,collapse=".")))[distnotknownordup])
    }else{
      knowdist=c(knowdist,(boost.adjust(X,groups=tmpgroups$groups,Correlation_sign=tmpcorrelation_sign,Xpass=Xpass,use.parallel=use.parallel,ncores=ncores)$vmf.params)[distnotknownordup])
      knowgroups=c(knowgroups,(unlist(lapply(tmpgroups$groups,paste,collapse=".")))[distnotknownordup])
    }
    if(verbose){
      if(sum(distnotknownordup)!=0){
        if(attr(tmpgroups$groups,"type") %in% c("compact")){
          cat("new:",(unlist(lapply(tmpcommunities,paste,collapse=".")))[distnotknownordup],"\n")
        } else {
          cat("new:",(unlist(lapply(tmpgroups$groups,paste,collapse=".")))[distnotknownordup],"\n")
        }
        cat("knowgroups:",knowgroups,"\n")
      } else {
        cat("no new group.","\n")
      }
    }
    if(verbose){cat("Generating X random sets","\n")}
    if(attr(tmpgroups$groups,"type") %in% c("compact")){
      distforcommunities=unlist(lapply(tmpcommunities,paste,collapse="."))
      distforgroups=rep(NA,length(tmpgroups.orig))
      for(mmm in 1:length(tmpgroups.orig)){
        distforgroups[mmm]<-distforcommunities[tmpgroups.orig[[mmm]]]
      }
    } else {
      distforgroups=unlist(lapply(tmpgroups.orig,paste,collapse="."))
    }
    Xrandom=boost.random(X,Xpass,knowdist[distforgroups],B=B,use.parallel=use.parallel,ncores=ncores)
    if(verbose){cat("Applying to random sets","\n")}
    result=rbind(result,boost.select(boost.apply(X,Xrandom,Y,func=func,use.parallel=use.parallel,ncores=ncores,...),version=version))
    if(verbose){cat("Finalizing functions","\n")}
    row.names(result)[dim(result)[1]]<-paste("c0 =",round(iii,3))
#    if(iii==1){result_sans<-result[1,,drop=FALSE]}
#    if(iii==0){result_alea<-result[2,,drop=FALSE]
#    limi_alea<-qbinom(risk,B,(result_alea))/B
#    if(verbose){
#      cat("sum(result_sans)!=0",sum(result_sans)!=0,"\n")
#    }
#    }

    if(!(iii %in% c(1,0))){
      if(!custom.steps){
        tempcond=(qq-etape>=0)
        qq<-qq-etape
        if(verbose){
          cat("cond:",tempcond,"\n")
        }
        if(!tempcond){break}
      }
      kk<-kk+1

#      if(length(which(result_sans==1))>1){
#        ppp<-max(apply(result[,which(result_sans==1)],2,min))
#
#        if(custom.steps){
#          tempcond=kk<=length(steps.seq)+1
#        } else {
#          tempcond=(kk<=length(steps.seq)+1)&&(qq-etape>=0)
#          qq<-qq-etape
#        }
#        cat("cond:",tempcond,"\n")
#        if(!tempcond){break}
#        kk<-kk+1
#      }else{
#        ppp<-max(result[dim(result)[1],])
#
#        if(custom.steps){
#          tempcond=kk<=length(steps.seq)+1
#        } else {
#          tempcond=(kk<=length(steps.seq)+1)&&(qq-etape>=0)
#          qq<-qq-etape
#        }
#        cat("cond:",tempcond,"\n")
#        if(!tempcond){break}
#        kk<-kk+1
#      }
    }

  }
  if(nrow(result)>2){
    result<-result[c(c(1,3:nrow(result)),2),]
  }

  if(use.parallel){
    parallel::stopCluster(workers)
  }

  if(debug){
    attr(result,"dimX")<-dim(X)
    attr(result,"Xrandom")<-Xrandom
    attr(result,"X_corr")<-X_corr
    attr(result,"Xpass")<-Xpass
    attr(result,"tmpcorrelation_sign")<-tmpcorrelation_sign
  }
  attr(result,"c0.seq")<-c(1,c0.seq,0)
  attr(result,"steps.seq")<-steps.seq
  attr(result,"typeboost")<-"fastboost"
  attr(result,"limi_alea")<-NA
  attr(result,"B")<-B

  class(result) <- "selectboost"
  return(result)
}


#' @title Plot selectboost object
#'
#' @description Plot a selectboostboost object.
#'
#' @name plot.selectboost
#'
#' @param x Numerical matrix. Result of selectboost (autoboost, fastboost, ...).
#' @param verbose Boolean.
#' Defaults to \code{FALSE}.
#' @param prop.level Numeric value. Used to compute the proportion of selection is
#' greater than prop.level. Defaults to \code{.95}.
#' @param conf.int.level Numeric value. Confidence level for confidence intervals on estimated
#' proportions of selection. Defaults to \code{.95}.
#' @param conf.threshold Numeric value. Used to compute the number of steps (c0) for which
#' the proportion of selection remains greater than conf.threshold. Defaults to \code{.95}.
#' @param ... . Passed to the plotting functions.
#'
#' @return An invisible list.
#' @family Selectboost analyse functions
#' @author Frederic Bertrand, \email{frederic.bertrand@@math.unistra.fr}
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Frédéric Bertrand, \url{https://arxiv.org/abs/1810.01670}
#' @seealso \code{\link{fastboost}}, \code{\link{autoboost}}
#' @examples
#' set.seed(314)
#' xran=matrix(rnorm(75),15,5)
#' ybin=sample(0:1,15,replace=TRUE)
#' yran=rnorm(15)
NULL
#> NULL

#' @rdname plot.selectboost
#' @method plot selectboost
#' @S3method plot selectboost
#'
#' @details \code{plot.selectboost} returns an invisible list and creates four graphics.
#' Two plots the proportion of selection with respect to c0 (by step or according to real scale).
#' On the third graph, no bar means a proportion of selection less than prop.level.
#' Confidence intervals are computed at the conf.int.level level.
#' Barplot of the confidence index (1-min(c0, such that proportion|c0>conf.threshold)).
#'
#' @examples
#' layout(matrix(1:4,2,2))
#'
#' data(autoboost.res.x)
#' plot(autoboost.res.x)
#'
#' data(autoboost.res.x2)
#' plot(autoboost.res.x2)
#'
#' @export
plot.selectboost <- function(x,verbose=FALSE,prop.level=.95,conf.int.level=.95,conf.threshold=.95,...){

  c0.seq <- attr(x,"c0.seq")
  limi_alea <- attr(x,"limi_alea")
  B <- attr(x,"B")
  result_c1<-x["c0 = 1",]
  result_c0<-x["c0 = 0",]

  par(mar=c(2,2,1,1))
  NN<-dim(x)[1]
  result2<-rbind(x[2:(NN-1),])

  matplot(x=1-c0.seq,y=x,type="b",lty=1,col=result_c1+2,xaxt="n",ylim=0:1,...)
  mtext("c0",1,0)
  axis(1,1-c0.seq,round(c0.seq,3))
  abline(h=max(limi_alea),lwd=2,col="blue")
  matplot(x,type="b",lty=1,col=result_c1+2,ylim=0:1,...)
  mtext("steps",1,0)
  abline(h=max(limi_alea),lwd=2,col="blue")

  #proportion of selection > prop.level and its evolution along the path of c0.
  #No bar means a proportion of selection less than prop.level
  result.greater=x>prop.level
  pos.bar<-barplot(colMeans(result.greater),beside=TRUE,col=(x[1,]>prop.level)+2,ylim=0:1,...)
  cool<-c("red","#096A09","#9EFD38")

  #Compute confidence intervals on the proportion of c0 values for which proportion of selection of a variable in the model is > .95
  confints.list<-simplify2array(lapply(lapply(colSums(result.greater),binom.test,nrow(x),p=.95,conf.level = conf.int.level),getElement,"conf.int"))
  segments(pos.bar,confints.list[1,],pos.bar,confints.list[2,],lwd=2)
  abline(h=conf.threshold,lty=2,lwd=3,col="green")
  abline(h=max(0,conf.threshold-0.05),lty=3,lwd=1,col="orange")
  abline(h=max(0,conf.threshold-.1),lty=3,lwd=1,col="red")
  mtext("Pathwise confidence intervals",1,0)

  confidence.index=apply(rbind(x,rep(-1,ncol(x)))>=conf.threshold,2,which.min)-1

  barplot(1-c0.seq[confidence.index],names.arg=names(confidence.index)[confidence.index>.5],ylim=0:1,...)
  mtext("Confidence index for variables",1,0)
  plot.result <- list(result=x,supp_boost=confints.list[2,]>=conf.threshold,
                      supp_c1=result_c1>=conf.threshold,supp_c0=result_c0>=conf.threshold,
                      c0.seq=c0.seq,confints.list=confints.list,
                      confidence.index=confidence.index)
  class(plot.result) <- "plot.selectboost"
  return(invisible(plot.result))
}


#' @title Summarize a selectboost analysis
#'
#' @description Summarize a selectboost analysis.
#'
#' @name summary.selectboost
#'
#' @param object Numerical matrix. Result of selectboost (autoboost, fastboost, ...).
#' @param crit.func Function . Defaults to the \code{mean} function.
#' @param crit.int Character value. Mean or median based confidence intervals. Defaults to \code{"mean"} based confidence intervals.
#' @param custom.values.lim Vector of numeric values. Defults to \code{NULL}.
#' @param index.lim Vector of numeric values. Defults to \code{NULL}.
#' @param alpha.conf.level Numeric value. Defults to \code{0.99}.
#' @param force.dec Boolean. Force trajectories to be non-increasing.
#' @param ... Additionnal arguments. Passed to the \code{crit.func} function.
#'
#' @return A list with the results.
#' @family Selectboost analyse functions
#' @author Frederic Bertrand, \email{frederic.bertrand@@math.unistra.fr}
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Frédéric Bertrand, \url{https://arxiv.org/abs/1810.01670}
#' @seealso \code{\link{fastboost}}, \code{\link{autoboost}}
NULL
#> NULL

#' @rdname summary.selectboost
#'
#' @details \code{summary.selectboost} returns a list with the results.
#'
#' @examples
#' data(autoboost.res.x)
#' summary(autoboost.res.x)
#' summary(autoboost.res.x, force.dec=FALSE)
#'
#' data(autoboost.res.x.adapt)
#' summary(autoboost.res.x.adapt)
#'
#' data(autoboost.res.x2)
#' summary(autoboost.res.x2)
#' summary(autoboost.res.x2, force.dec=FALSE)
#'
#' data(autoboost.res.x2.adapt)
#' summary(autoboost.res.x2.adapt)
#'
#' data(fastboost.res.x)
#' summary(fastboost.res.x)
#' summary(fastboost.res.x, force.dec=FALSE)
#'
#' data(fastboost.res.x.adapt)
#' summary(fastboost.res.x.adapt)
#'
#' data(fastboost.res.x2)
#' summary(fastboost.res.x2)
#' summary(fastboost.res.x2, force.dec=FALSE)
#'
#' data(fastboost.res.x2.adapt)
#' summary(fastboost.res.x2.adapt)
#' @export
summary.selectboost <- function(object,crit.func=mean,crit.int="mean",custom.values.lim=NULL,index.lim=NULL,alpha.conf.level=0.99,force.dec=TRUE,...){

  c0.seq=attr(object,"c0.seq")
  tdiff=apply(object,2,diff)
  tdiff[tdiff>0]<-0
  selectboost_result.dec<-pmax(rbind(0,apply(tdiff,2,cumsum))+matrix(object[1,],nrow(object),ncol(object),byrow=TRUE),0)
  rownames(object) <- rownames(object)

  if(force.dec){
    crit.func.values=apply(selectboost_result.dec,1,crit.func,...)
  } else {
    crit.func.values=apply(object,1,crit.func,...)
  }

  if(!is.null(custom.values.lim)) {crit.func.values.lim<-custom.values.lim} else {
    if(crit.int=="median"){
      warn.opt<-options("warn")$warn
      options(warn=-1)
      crit.func.values.lim=c(wilcox.test(crit.func.values,conf.int=TRUE, conf.level = alpha.conf.level)$conf.int[1],median(crit.func.values),wilcox.test(crit.func.values,conf.int=TRUE, conf.level = alpha.conf.level)$conf.int[2])
      options(warn=warn.opt)
    }
    if(crit.int=="mean"){
      crit.func.values.lim=c(t.test(crit.func.values, conf.level = alpha.conf.level)$conf.int[1],mean(crit.func.values),t.test(crit.func.values, conf.level = alpha.conf.level)$conf.int[2])
    }
  }
  names(crit.func.values.lim) <- c("crit.func.values.lim.red","crit.func.values.lim.orange","crit.func.values.lim.green")
  if(!is.null(index.lim)) {index.lim=index.lim} else {
    index.lim <- c(which.min(crit.func.values<crit.func.values.lim["crit.func.values.lim.red"])-1,which.min(crit.func.values<crit.func.values.lim["crit.func.values.lim.orange"])-1,which.min(crit.func.values<crit.func.values.lim["crit.func.values.lim.green"])-1)
    #  index.lim <- pmax(index.lim,rep(1,3))
    index.lim <- pmax(index.lim,1:3)
  }
  names(index.lim) <- c("index.lim.red","index.lim.orange","index.lim.green")
  col.crit.func.values<-rep(NA,length(crit.func.values))
  col.crit.func.values[1:index.lim["index.lim.green"]]<-"green"
  col.crit.func.values[1:index.lim["index.lim.orange"]]<-"orange"
  col.crit.func.values[1:index.lim["index.lim.red"]]<-"red"

  tempfreq=NULL
  for(iii in 0:20){
    tempfreq<-rbind(tempfreq,
                    table(factor(colSums(selectboost_result.dec>=1-iii*.05),levels=0:nrow(selectboost_result.dec)))
    )
  }
  rownames(tempfreq) <- paste("thres =",round(1-(0:20)*.05,2))

  tempfreq.lims=NULL
  for(iii in crit.func.values.lim){
    tempfreq.lims<-rbind(tempfreq.lims,
                         table(factor(colSums(selectboost_result.dec>=iii),levels=0:nrow(selectboost_result.dec)))
    )
  }
  rownames(tempfreq.lims) <- paste("thres =",round(crit.func.values.lim,6))
  res<-list(crit.func.values=crit.func.values,crit.func.values.lim=crit.func.values.lim,force.dec=force.dec,index.lim=index.lim,col.crit.func.values=col.crit.func.values,selectboost_result.dec=selectboost_result.dec,freq.dec=tempfreq,freq.dec.lims=tempfreq.lims)
  class(res) <- "summary.selectboost"

  return(res)
}


#' @title Plot a summary of selectboost results
#'
#' @description Plot a summary of selectboost results.
#'
#' @name plot.summary.selectboost
#'
#' @param x Numerical matrix. Summary of selectboost object.
#' @param ... . Passed to the plotting functions.
#'
#' @return An invisible list.
#' @family Selectboost analyze functions
#' @author Frederic Bertrand, \email{frederic.bertrand@@math.unistra.fr}
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Frédéric Bertrand, \url{https://arxiv.org/abs/1810.01670}
#' @seealso \code{\link{fastboost}}, \code{\link{autoboost}} and \code{\link{summary.selectboost}}
NULL
#> NULL

#' @rdname plot.summary.selectboost
#' @method plot summary.selectboost
#' @S3method plot summary.selectboost
#'
#' @details \code{plot.summary.selectboost} returns an invisible list and creates four graphics.
#' Two plots the proportion of selection with respect to c0 (by step or according to real scale).
#' On the third graph, no bar means a proportion of selection less than prop.level.
#' Confidence intervals are computed at the conf.int.level level.
#' Barplot of the confidence index (1-min(c0, such that proportion|c0>conf.threshold)).
#'
#' @examples
#' data(autoboost.res.x)
#' plot(summary(autoboost.res.x))
#'
#' data(autoboost.res.x2)
#' plot(summary(autoboost.res.x2))
#'
#' @export
plot.summary.selectboost <- function(x,...)
{
  plot(x$crit.func.values,...)
  abline(h=x$crit.func.values.lim,lty=2,lwd=3,col=c("red","orange","green"))
  points(1:length(x$crit.func.values),x$crit.func.values,col=x$col.crit.func.values,pch=19)
}


#' @title Plot trajectories
#'
#' @description Plot trajectories.
#'
#' @name trajC0
#'
#' @param x Numerical matrix. Selectboost object.
#' @param ... . Passed to the plotting functions.
#'
#' @return An invisible list.
#' @family Selectboost analyze functions
#' @author Frederic Bertrand, \email{frederic.bertrand@@math.unistra.fr}
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Frédéric Bertrand, \url{https://arxiv.org/abs/1810.01670}
#' @seealso \code{\link{fastboost}}, \code{\link{autoboost}} and \code{\link{summary.selectboost}}
NULL
#> NULL

#' @rdname trajC0
#'
#' @details \code{trajC0} returns an invisible list and creates four graphics.
#'
#' @export
trajC0 = function(x, ...){
  UseMethod("trajC0")
}

#' @return invisible list.
#'
#' @param summary.selectboost.res List. Summary of selectboost object.
#' @param lasso.coef.path List. Result of \code{predict.lars}.
#' @param type.x.axis Character value. "scale" or "noscale" for the X axis.
#' @param type.graph Character value. Type of graphs: "bars", "lasso" and "boost".
#' @param threshold.level Numeric value. Threshold for the graphs.
#'
#' @rdname trajC0
#' @method trajC0 selectboost
#' @S3method trajC0 selectboost
#'
#' @examples
#'
#' data(autoboost.res.x)
#' data(diabetes, package="lars")
#'
#' ### With lasso trajectories
#' m.x<-lars::lars(diabetes$x,diabetes$y)
#' plot(m.x)
#' mm.x<-predict(m.x,type="coef",mode="lambda")
#' autoboost.res.x.mean = summary(autoboost.res.x)
#'
#' par(mfrow=c(2,2),mar=c(4,4,1,1))
#' trajC0(autoboost.res.x,autoboost.res.x.mean,lasso.coef.path=mm.x,type.graph="lasso")
#' trajC0(autoboost.res.x,autoboost.res.x.mean)
#' trajC0(autoboost.res.x,autoboost.res.x.mean,type.graph="bars")
#' trajC0(autoboost.res.x,autoboost.res.x.mean,type.x.axis ="scale")
#'
trajC0.selectboost <- function(x,summary.selectboost.res,lasso.coef.path,type.x.axis="noscale",type.graph="boost",
                               threshold.level=NULL, ...) {
  if(is.null(threshold.level)){threshold.level= (summary.selectboost.res$crit.func.values.lim)["crit.func.values.lim.green"]}
  if(summary.selectboost.res$force.dec){x[,]<-summary.selectboost.res$selectboost_result.dec}
  c0.seq<-attr(x,"c0.seq")
  coul.traj<-rep("black",ncol(x))
  coul.traj[apply(x[1:summary.selectboost.res$index.lim["index.lim.red"],,drop=FALSE],2,min)>=threshold.level]<-"red"
  coul.traj[apply(x[1:summary.selectboost.res$index.lim["index.lim.orange"],,drop=FALSE],2,min)>=threshold.level]<-"orange"
  coul.traj[apply(x[1:summary.selectboost.res$index.lim["index.lim.green"],,drop=FALSE],2,min)>=threshold.level]<-"green"
  llwd.traj<-rep(1,ncol(x))
  llwd.traj[apply(x[1:summary.selectboost.res$index.lim["index.lim.red"],,drop=FALSE],2,min)>=threshold.level]<-3
  if(type.graph=="bars"){
    barplot(table(factor(coul.traj[coul.traj!="black"],levels=c("green","orange","red"))),col=c("green","orange","red"))
  }
  if(type.graph=="lasso"){
    matplot(lasso.coef.path$fraction,lasso.coef.path$coefficients,type="l",lty=1,col=coul.traj,xlim=c(0,max(lasso.coef.path$fraction)),
            ylim=range(lasso.coef.path$coefficients),
            #            ylim=c(-300-400,550+200),
            lwd=llwd.traj,xlab="lambda",ylab="coefficients")
  }
  if(type.graph=="boost"){
    if(type.x.axis=="noscale"){
      matplot(x,type="l",lty=1,col=coul.traj,lwd=llwd.traj,xaxt="n",xlab="confidence index",ylab="probability of being in the support")
      axis(1,1:length(c0.seq),labels=signif(1-c0.seq,6))
      abline(h=threshold.level,col="black",lwd=4,lty=3)
      points(1:length(c0.seq),rep(1.035,length(c0.seq)),col=summary.selectboost.res$col.crit.func.values,pch=19)
    }

    if(type.x.axis=="scale"){
      matplot(x=1-c0.seq,y=x,type="l",lty=1,col=coul.traj,lwd=llwd.traj,xaxt="n",xlab="confidence index",ylab="probability of being in the support")
      axis(1,1-c0.seq,labels=signif(1-c0.seq,6))
      abline(h=threshold.level,col="black",lwd=4,lty=3)
      points(1-c0.seq,rep(1.035,length(c0.seq)),col=summary.selectboost.res$col.crit.func.values,pch=19)
    }
  }
  invisible(list(coul.traj=coul.traj,select.traj=x[,coul.traj!="black"]))
}

#' @title Find limits for selectboost analysis
#'
#' @description Find limits for selectboost analysis.
#'
#' @name auto.analyze
#'
#' @param x Numerical matrix. Selectboost object.
#' @param ... . Passed to the summary.selectboost function.
#'
#' @family Selectboost analyze functions
#'
#' @author Frederic Bertrand, \email{frederic.bertrand@@math.unistra.fr}
#'
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Frédéric Bertrand, \url{https://arxiv.org/abs/1810.01670}
#' @seealso \code{\link{fastboost}} and \code{\link{autoboost}}
NULL
#> NULL

#' @rdname auto.analyze
#'
#' @details \code{plot.summary.selectboost} returns an invisible list and creates four graphics.
#' Two plots the proportion of selection with respect to c0 (by step or according to real scale).
#' On the third graph, no bar means a proportion of selection less than prop.level.
#' Confidence intervals are computed at the conf.int.level level.
#' Barplot of the confidence index (1-min(c0, such that proportion|c0>conf.threshold)).
#'
#' @export
auto.analyze = function(x, ...){
  UseMethod("auto.analyze")
}

#' @return list of results.
#'
#' @rdname auto.analyze
#' @method auto.analyze selectboost
#' @S3method auto.analyze selectboost
#'
#' @examples
#' data(autoboost.res.x)
#' auto.analyze(autoboost.res.x)
#'
#' data(autoboost.res.x2)
#' auto.analyze(autoboost.res.x2)
#'
auto.analyze.selectboost = function(x, ...) {
  selectboost_result.custom.findlim <- summary.selectboost(x,force.dec=FALSE,...)
  selectboost_result.custom.emplim <- summary.selectboost(x,custom.values.lim=selectboost_result.custom.findlim$crit.func.values.lim,index.lim=selectboost_result.custom.findlim$index.lim,force.dec=TRUE,...)
  return(selectboost_result.custom.emplim)
}


