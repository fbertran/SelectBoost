lasso_AICc<-function(X,Y){
  
  glmnet.fit <- glmnet(X,Y,family="binomial",standardize=F)
  #cutOff = min(which(glmnet.fit$df > 30))
  #subSample = 1:cutOff
  subSample = 1:max(glmnet.fit$df)
  AICc.gn.median <- AICc.glmnetB(X,Y,glmnet.fit,alpha=1,subSample, reducer='median')
  resultat<-matrix(0,ncol(X),1)
  resultat[AICc.gn.median$bestSet]<-AICc.gn.median$model$beta
  return(resultat)
  
}

