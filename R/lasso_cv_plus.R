
lasso_cv_plus<-function(X,Y){
  
  resultat<-cv.glmnet(X,Y,family="binomial", type.measure = "class",nfolds=10)
  indice<-resultat$lambda.1se
  resultat<-glmnet(X,Y,family="binomial")
  predict(resultat,type="coef",s=indice)
  
}
