lasso_cv<-function(X,Y){
  
  resultat<-cv.glmnet(X,Y,family="binomial", type.measure = "class",nfolds=10)
  indice<-resultat$lambda.min
  resultat<-glmnet(X,Y,family="binomial")
  predict(resultat,type="coef",s=indice)
  
}
