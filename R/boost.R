boost<-function(X,Y,group,func,corr=1,B=200,normalize=TRUE,eps=10^(-4)){
  
  
  group2<-eval(parse(text=paste("function(x){return(",group,"(x,corr))}",sep="")))
  
  
  #require(snowfall)
  #require(lars)
  
  
  #if(multi==FALSE){
  #if(cpu>1){
  #sfInit( parallel=TRUE, cpus=cpu )
  #}else{
  #sfInit( parallel=FALSE, cpus=cpu )	
  #	}
  #sfLibrary(movMF)
  #sfLibrary(lars)
  #sfLibrary(msgps)
  
  require(movMF)
  #require(lars)
  #require(msgps)
  require(glmnet)
  
  #sfLibrary(snowfall)}
  
  if(normalize==TRUE){
    
    X<-X-matrix(rep(apply(X,2,mean),dim(X)[1]),dim(X)[1],dim(X)[2],byrow=TRUE)	
    X<-t(t(X)/sqrt(diag(t(X)%*%X)))
    
  }
  
  Correlation_matrice<-t(X)%*%X
  Correlation_indice<-Correlation_matrice*0	
  Correlation_indice[which((Correlation_matrice)>=0)]<-1		
  Correlation_indice[which(-(Correlation_matrice)>=0)]<--1	
  diag(Correlation_indice)<-1
  
  groups<-group2(X)
  
  #nb_correlated_variables<-apply(abs(Correlation_indice),2,sum)
  #		#cat("Number of correlated variables : \n")
  
  
  Boot<-matrix(rep(0,dim(X)[2]*B),B,dim(X)[2])
  
  func_passage1<-function(x){
    
    Xpass<-matrix(0,dim(X)[1],dim(X)[1]-1)
    Xpass[col(Xpass)>row(Xpass)]<-1
    diag(Xpass)<-1
    Xpass[col(Xpass)==(row(Xpass)-1)]<--(1:(dim(X)[1]-1))
    Xpass<-t(t(Xpass)/sqrt(diag(t(Xpass)%*%Xpass)))
    return(solve(t(Xpass)%*%Xpass)%*%t(Xpass)%*%x)
  }	
  
  func_passage2<-function(x){
    
    Xpass<-matrix(0,dim(X)[1],dim(X)[1]-1)
    Xpass[col(Xpass)>row(Xpass)]<-1
    diag(Xpass)<-1
    Xpass[col(Xpass)==(row(Xpass)-1)]<--(1:(dim(X)[1]-1))
    Xpass<-t(t(Xpass)/sqrt(diag(t(Xpass)%*%Xpass)))
    return(apply( matrix(rep(x,dim(X)[1]),dim(X)[1],dim(X)[1]-1,byrow=TRUE)*Xpass,1,sum))
  }
  
  Xret<-array(0,c(dim(X)[1],dim(X)[2],B))
  Xb<-X
  
  
  
  simul1<-function(j){
    
    if(length(groups[[j]])>=2){
      
      indice<-groups[[j]]
      corr_set<-t(t(X[,indice])*Correlation_indice[j,indice])
      corr_set2<-apply(corr_set,2,func_passage1)
      BB<-coef(movMF(t(corr_set2),1))$theta
      newv<-rmovMF(1,BB)
      newv<-func_passage2(newv)
    }else{
      
      newv<-X[,j]
      
    }
    
    
    return(newv)			
  }
  
  simul1<-Vectorize(simul1)
  
  simul2<-function(){simul1(1:(dim(X)[2]))}
  
  Xret<-replicate(B,simul2())
  
  
  select<-function(k){
    rr<-eval(parse(text=paste(func,"(Xret[,,",k,"],Y)",sep="")))
    
    return(rr)	
  }
  
  
  Boot<-lapply(1:B,select)
  
  Boot2<-c()
  for(i in 1:B) {Boot2<-c(Boot2,unlist(Boot[[i]][-1,]))}
  
  Boot<-matrix(Boot2,B,dim(X)[2],byrow=TRUE)
  
  Fs<-apply(abs(Boot)>eps,2,sum)
  
  
  return(Fs/B)
}

