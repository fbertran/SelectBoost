#' @title Generate groups by thresholding.
#'
#' @description \code{group_func_1} creates groups of variables based on thresholding the input matrix.
#'
#' @details This is a function used to create a list of groups using an input matrix and a
#' thresholding value c0. A group is made, for every column in the input matrix.
#'
#' @param absXcor A numeric matrix. The absolute value of a correlation or distance matrix.
#' @param c0 A numeric scalar. The thresholding
#' @return A list with one entry: the list of groups.
#' Attributes:
#' \itemize{
#'   \item "type": "normal"
#'   \item "length.groups" the length of each groups.
#' }
#' @author Frederic Bertrand, \email{frederic.bertrand@@math.unistra.fr}
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Frédéric Bertrand, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Bioinformatics, 2020. \url{https://doi:10.1093/bioinformatics/btaa855}
#' @seealso \code{\link{group_func_2}} and \code{\link{boost.findgroups}}
#' @examples
#' set.seed(314)
#' group_func_1(cor(matrix(rnorm(50),10,5)),.4)
#'
#' @export
group_func_1<-function(absXcor,c0){

  absXcor[absXcor<c0]<-0
  diag(absXcor)<-1
  if(c0>0){
  absXcor[absXcor!=0]<-1
  } else {
  absXcor[,] <- 1
  }

dete<-function(x){
  which(x == 1)
  }
res<-apply(absXcor,2,dete)

if(is.vector(res)){
  res <- sapply(res,list)
}
if(is.matrix(res)){
#  res <- sapply(res,list)
  cnames <- colnames(res)
  res <- lapply(seq_len(ncol(res)), function(i) res[, i])
  names(res) <- cnames
#https://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-of-column-vectors-in-r
}
attr(res,"type")<-"normal"
return(res)
}
