#' @title Generate groups using community analysis.
#'
#' @description \code{group_func_2} creates groups of variables based on community analysis.
#'
#' @details This is a function used to create a list of groups using an input matrix and a
#' thresholding value c0. A group is made, for every column in the input matrix.
#' It uses the \code{infomap.community} function of the \code{igraph} package.
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
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Frédéric Bertrand, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, \url{https://arxiv.org/abs/1810.01670}
#' @seealso \code{\link{group_func_2}} \code{\link{boost.findgroups}}, \code{\link[igraph]{infomap.community}} and \code{\link[igraph]{igraph}}.
#' @examples
#' set.seed(314)
#' group_func_2(cor(matrix(rnorm(100),10,10)),.5)
#'
#' @export
group_func_2<-function(absXcor,c0){

  requireNamespace("igraph")
  absXcor[absXcor<c0]<-0
  if(c0>0){
    absXcor[absXcor!=0]<-1
  } else {
    absXcor[,] <- 1
  }
  if(is.null(attr(absXcor,"undirected"))){
    absXcor.undirected<-TRUE
  } else {
      absXcor.undirected<-attr(absXcor,"directed")
      }

  if(absXcor.undirected){
    Gr<-graph.adjacency(absXcor, weighted=TRUE, mode = "undirected")
  } else {
    Gr<-graph.adjacency(absXcor, weighted=TRUE, mode = "directed")
  }
  W<-infomap.community(Gr)
  ee<-communities(W)
  if(length(ee)==ncol(absXcor)){
    res<-1:ncol(absXcor)
    attr(res,"type")<-"normal"
  }else{
    res<-vector("list",0)
    for(i in 1:length(ee)){
      for(j in ee[[i]]){
        res[[j]]<-i
      }
      }
    res<-res[colnames(absXcor)]
    attr(res,"type")<-"compact"
    res[["communities"]]<-ee
  }
  return(res)
}






