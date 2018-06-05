group_func_2<-function(X,c0){
  
  require(igraph)
  Xcor<-abs(cor(X))
  Xcor[Xcor<c0]<-0
  diag(Xcor)<-0
  
  Gr<-graph.adjacency(Xcor, weighted=TRUE)
  W<-infomap.community(Gr)
  ee<-communities(W)
  if(length(ee)==ncol(X)){
    res<-1:ncol(X)
  }else{
    res<-vector("list",ncol(X))
    for(i in 1:length(ee)){
      for(j in ee[[i]]){
        res[[j]]<-ee[[i]]
      }     }
  }
  
  
  return(res)
  
  
}






