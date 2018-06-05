group_func_1<-function(X,c0){

Xcor<-abs(cor(X))
Xcor[Xcor<c0]<-0
Xcor[Xcor!=0]<-1

dete<-function(x){which(x == 1)}
res<-apply(Xcor,2,dete)
return(res)


}



