#' @title Selectboost_cascade
#'
#' @description Selectboost for Cascade inference.
#'
#' @name selectboost_cascade
#'
#' @param M Microarray class from the Cascade package.
#' @param Fabhat F matrix inferred using the inference function from the Cascade package.
#' @param K Number of crossvalidation folds.
#' @param eps Threshold for assinging a zero value to an inferred parameter. Defaults to 10^-5.
#' @param cv.subjects Crossvalidation is made subjectwise using leave one out. Discards the K option.
#' @param ncores Numerical value. Number of cores for parallel computing.
#' Defaults to \code{4}.
#' @param use.parallel Boolean. Use parallel computing (doMC).
#' Defaults to \code{TRUE}.
#' @param verbose Boolean.
#' Defaults to \code{FALSE}.
#' @param group Function. The grouping function.
#' Defaults to \code{group_func_2}.
#' @param c0value Numeric. c0 value to use for confidence computation.
#' Defaults to \code{TRUE}
#' @param ... Additionnal arguments. Not used.
#'
#' @return A \code{network.confidence} object.
#' @family Selectboost functions
#' @author Frederic Bertrand, \email{frederic.bertrand@@math.unistra.fr}
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Frédéric Bertrand, \url{https://arxiv.org/abs/1810.01670}
#' @seealso \code{\link{boost}}, \code{\link{fastboost}}, \code{\link{plot.selectboost}}, \code{\link[Cascade]{inference}}
#' @examples
#' set.seed(314)
NULL
#> NULL

#' @rdname selectboost_cascade
#'
#' @details Extending results from the Cascade package: providing confidence indices for the reverse engineered links.
#'
#' Reference for the Cascade modelling
#' Vallat, L., Kemper, C. a., Jung, N., Maumy-Bertrand, M., Bertrand, F.,
#' Meyer, N., Pocheville, A., Fisher, J. W., Gribben, J. G. et Bahram, S.
#' (2013). Reverse-engineering the genetic circuitry of a cancer cell with predicted
#' intervention in chronic lymphocytic leukemia. Proceedings of the National
#' Academy of Sciences of the United States of America, 110(2), 459-64.
#'
#' Reference for the Cascade package
#' Jung, N., Bertrand, F., Bahram, S., Vallat, L. et Maumy-Bertrand, M. (2014).
#' Cascade : A R package to study, predict and simulate the diffusion of a signal
#' through a temporal gene network. Bioinformatics. ISSN 13674803..
#'
#' @importClassesFrom Cascade micro_array
#' @docType methods
#' @export

#selectboost.microarray=

setGeneric("selectboost",package="SelectBoost",def = function(M,... ){standardGeneric("selectboost")})

#' @rdname selectboost_cascade
#' @aliases selectboost,micro_array,micro_array-method
#' @examples
#' set.seed(314)
#'
#' \donttest{
#' data(Cascade_example)
#' Fab_inf_C <- Net_inf_C@F
#' #By default community grouping of variables
#' set.seed(1)
#' net_confidence <- selectboost(M, Fab_inf_C)
#' net_confidence_.5 <- selectboost(M, Fab_inf_C, c0value = .5)
#' #With group_func_1, variables are grouped by thresholding the correlation matrix
#' net_confidence_thr <- selectboost(M, Fab_inf_C, group = group_func_1)
#'}
#'

setMethod(f="selectboost"
,signature=c("micro_array")
,definition=function(M
                     ,Fabhat
                     ,K=5
                     ,eps=10^-5
                     ,cv.subjects=TRUE
                     ,ncores=4
                     ,use.parallel=TRUE
                     ,verbose=FALSE
                     ,group = group_func_2
                     ,c0value = .95
          ){
  mat<-M@microarray
            #Data matrix
            gr<-M@group
            #Group vector
            N<-dim(mat)[1]
            #Number of genes
            T<-length(unique(M@group))
            #Number of repeated measurements
            P<-M@subject
            #Subject number

            #Fab estimated by Cascade
            F<-Fabhat

            Gamma<-array(1,c(N,N))

            sup_pred<-rep(1:(T-1),P)+rep(seq(0,T*(P-1),T),each=T-1)

            u<-0

            for(peak in 2:T){
              cat("\n")
              cat(paste("We are at peak : ", peak))
              cat("\n")
              #Peak is the number of the response group

              IND<-which(gr %in% 1:(peak-1))
              grIND<-gr[IND]
              pred<-mat[IND,sup_pred]

              #Building the predictor matrix
              for(k in 1:(peak-1)) {
                ind<-which(grIND %in% k)
                u<-u+1
                f<-function(x){(F[,,u]%*%(x))}
                for(i in 1:P){
                  pred[ind,1:(T-1)+(i-1)*(T-1)]<-t(apply(pred[ind,1:(T-1)+(i-1)*(T-1)],1,f))
                }
              }

              pred[is.na(pred)]<-0

              #Response matrix
              Y<-mat[which(gr %in% peak),sup_pred+1]
              Gamma[IND, which(gr %in% peak)]<-0 #Gamma[IND, which(gr %in% peak)]*0

              #Confidence
              if(cv.subjects==TRUE){
                cv.folds1=function(n,folds){
                  split(1:dim(pred)[2]
                        ,rep(1:P,each=dim(pred)[2]/P))}
#                cv.fun.name="cv.folds1"
              } else {
                cv.folds1=lars::cv.folds
#                cv.fun.name="lars::cv.folds"
              }
#                         cat(cv.fun.name)

              fun_lasso_selectboost<-function(x){cat(".");fastboost(X = pred, Y = x, ncores = ncores, group = group, func = lasso_cascade, corrfunc="cor", verbose= verbose, use.parallel = use.parallel, K=K, eps=eps, cv.fun=cv.folds1
                                                                    #, cv.fun.name=cv.fun.name
                                                                    , steps.seq = c0value, c0lim = FALSE)}
              Gamma[IND, which(gr %in% peak)]<-apply(Y,1,fun_lasso_selectboost)

            }

            result<-methods::new("network.confidence"
                        ,network.confidence=Gamma
                        ,name=M@name
                        ,F=F
                        ,time_pt=M@time
                        ,cv.subjects=cv.subjects
#                        ,cv.fun.name=cv.fun.name
            )
            return(result)
          }
)

