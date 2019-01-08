######## Demo selectboost Diabetes dataset first order linear model ########
######## with Parallel support ########

# Change ncores to match the number of cores you want to use
ncores.value = 4

library(SelectBoost)
require(lars)
data(diabetes)

#### Simple linear model for diabetes dataset
###
### 442 observations
### 10 variables
### 10 terms
## 10 principal effects
# "age" "sex" "bmi" "map" "tc" "ldl" "hdl"
# "tch" "ltg" "glu" "age^2"

B1.x.array<-boost(diabetes$x,diabetes$y,group_func_1,lasso_cv_lars_min,B=25,version = "lars", verbose=TRUE,method="Xarrays", use.parallel=TRUE, ncores=ncores.value)
str(B1.x.array)

B1.x<-boost(diabetes$x,diabetes$y,group_func_1,lasso_cv_lars_min,B=25,version = "lars", verbose=TRUE, use.parallel=TRUE, ncores=ncores.value)
B1.x
B2.x<-boost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corr=1.0,B=25,version = "lars", verbose=TRUE, use.parallel=TRUE, ncores=ncores.value)
B2.x
B3.x<-boost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corr=0.95,B=25,version = "lars", verbose=TRUE, use.parallel=TRUE, ncores=ncores.value)
B3.x
B4.x<-boost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corr=0.9,B=25,version = "lars", verbose=TRUE, use.parallel=TRUE, ncores=ncores.value)
B4.x
B5.x<-boost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corr=0.85,B=250,version = "lars", verbose=TRUE, use.parallel=TRUE, ncores=ncores.value)
B5.x
B6.x<-boost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corr=0.8,B=250,version = "lars", verbose=TRUE, use.parallel=TRUE, ncores=ncores.value)
B6.x
B7.x<-boost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corr=0.75,B=250,version = "lars", verbose=TRUE, use.parallel=TRUE, ncores=ncores.value)
B7.x
B8.x<-boost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corr=0.7,B=250,version = "lars", verbose=TRUE, use.parallel=TRUE, ncores=ncores.value)
B8.x
B9.x<-boost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corr=0.65,B=25,version = "lars", verbose=TRUE, use.parallel=TRUE, ncores=ncores.value)
B9.x
B10.x<-boost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corr=0.60,B=25,version = "lars", verbose=TRUE, use.parallel=TRUE, ncores=ncores.value)
B10.x
B11.x<-boost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corr=0.55,B=25,version = "lars", verbose=TRUE, use.parallel=TRUE, ncores=ncores.value)
B11.x
B12.x<-boost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corr=0.5,B=25,version = "lars", verbose=TRUE, use.parallel=TRUE, ncores=ncores.value)
B12.x
B12.x.array<-boost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corr=0.5,B=25,version = "lars",method="Xarrays", use.parallel=TRUE, ncores=ncores.value)
B12.x.array


### Coefficients in the different models
browstot.x<-rbind(B1.x,B2.x,B3.x,B4.x,B5.x,B6.x,B7.x,B8.x,B9.x,B10.x,B11.x,B12.x)
brows.x<-rbind(B2.x,B3.x,B4.x,B5.x,B6.x,B7.x,B8.x,B9.x,B10.x,B11.x,B12.x)
bcols.x<-cbind(B2.x,B3.x,B4.x,B5.x,B6.x,B7.x,B8.x,B9.x,B10.x,B11.x,B12.x)

### Coefficients in the different models
barplot(browstot.x,lty=1,type="h",col=rainbow(12),beside = TRUE)

### Selected Terms
cbind(coef(b.x,s=a.x$index[which.min(a.x$cv)],mode="fraction")!=0,bcols.x==1)

### Distribution of confidence indices
layout(t(1:4))
hist(B1.x,xlim=c(0,1),ylim=c(0,10),main="Histogram lasso_cv_min")
hist(B2.x,xlim=c(0,1),ylim=c(0,10),main="Histogram lasso_msgps_AICc, c=1")
hist(B3.x,xlim=c(0,1),ylim=c(0,10),main="Histogram lasso_msgps_AICc, c=.95")
hist(B4.x,xlim=c(0,1),ylim=c(0,10),main="Histogram lasso_msgps_AICc, c=.90")
hist(B5.x,xlim=c(0,1),ylim=c(0,10),main="Histogram lasso_msgps_AICc, c=.85")
hist(B6.x,xlim=c(0,1),ylim=c(0,10),main="Histogram lasso_msgps_AICc, c=.80")
hist(B7.x,xlim=c(0,1),ylim=c(0,10),main="Histogram lasso_msgps_AICc, c=.75")
hist(B8.x,xlim=c(0,1),ylim=c(0,10),main="Histogram lasso_msgps_AICc, c=.70")
hist(B9.x,xlim=c(0,1),ylim=c(0,10),main="Histogram lasso_msgps_AICc, c=.65")
hist(B10.x,xlim=c(0,1),ylim=c(0,10),main="Histogram lasso_msgps_AICc, c=.60")
hist(B11.x,xlim=c(0,1),ylim=c(0,10),main="Histogram lasso_msgps_AICc, c=.55")
hist(B12.x,xlim=c(0,1),ylim=c(0,10),main="Histogram lasso_msgps_AICc, c=.50")
layout(1)

### Lasso trajectories
m.x<-lars(diabetes$x,diabetes$y)
plot(m.x,breaks=FALSE,ylim=c(-100,100)*5,xlim=c(0,0.1))

mm.x<-predict(m.x,type="coef",mode="lambda")

coul.x<-rep("black",10)
coul.x[apply(brows.x[1:7,],2,min)>0.95]<-"red"
coul.x[apply(brows.x[1:9,],2,min)>0.95]<-"orange"
coul.x[apply(brows.x[1:11,],2,min)>0.95]<-"green"
matplot(mm.x$fraction,mm.x$coefficients,type="l",lty=1,col=coul.x)


llwd.x<-rep(1,10)
llwd.x[apply(brows.x[1:7,],2,min)>0.95]<-3


par(mfrow=c(1,2),mar=c(4,4,1,1))
matplot(mm.x$fraction,mm.x$coefficients,type="l",lty=1,col=coul.x,xlim=c(0,0.8),ylim=c(-300-400,550+200),lwd=llwd.x,xlab="lambda",ylab="coefficients")

matplot(brows.x,type="l",lty=1,col=coul.x,lwd=llwd.x,xaxt="n",xlab="confidence index",ylab="probability of being in the support")
axis(1,1:14,label=1-seq(1,0.35,-0.05))
abline(h=0.95,col="black",lwd=4,lty=3)
