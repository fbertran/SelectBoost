######## Demo selectboost Diabetes dataset quadratic model ########

library(SelectBoost)
require(lars)
data(diabetes)
attach(diabetes)

#### Quadratic model for diabetes dataset
###
### 442 observations
### 10 variables
### 64 terms
## 10 principal effects
# "age" "sex" "bmi" "map" "tc" "ldl" "hdl"
# "tch" "ltg" "glu" "age^2"
## 9 squared effects
# "age^2" "bmi^2" "map^2" "tc^2" "ldl^2"
# "hdl^2" "tch^2" "ltg^2" "glu^2"
## 45 interactions
# "age:sex" "age:bmi" "age:map" "age:tc"
# "age:ldl" "age:hdl" "age:tch" "age:ltg"
# "age:glu" "sex:bmi" "sex:map" "sex:tc"
# "sex:ldl" "sex:hdl" "sex:tch" "sex:ltg"
# "sex:glu" "bmi:map" "bmi:tc"  "bmi:ldl"
# "bmi:hdl" "bmi:tch" "bmi:ltg" "bmi:glu"
# "map:tc"  "map:ldl" "map:hdl" "map:tch"
# "map:ltg" "map:glu" "tc:ldl"  "tc:hdl"
# "tc:tch"  "tc:ltg"  "tc:glu"  "ldl:hdl"
# "ldl:tch" "ldl:ltg" "ldl:glu" "hdl:tch"
# "hdl:ltg" "hdl:glu" "tch:ltg" "tch:glu"
# "ltg:glu"

a<-cv.lars(x2,y,trace=TRUE,max.steps=99,mode="fraction")
b<-lars(x2,y,max.steps=99)
coef(b,s=a$index[which.min(a$cv)],mode="fraction")!=0

#run with larger B values (at least 100)
B1<-boost(diabetes$x2,diabetes$y,"group_func_1","lasso_cv_lars_min",B=10,version = "lars")
B1
B2<-boost(diabetes$x2,diabetes$y,"group_func_1","lasso_msgps_AICc",corr=1,B=10,version = "lars")
B2
B3<-boost(diabetes$x2,diabetes$y,"group_func_1","lasso_msgps_AICc",corr=0.95,B=10,version = "lars")
B3
B4<-boost(diabetes$x2,diabetes$y,"group_func_1","lasso_msgps_AICc",corr=0.9,B=10,version = "lars")
B4
B5<-boost(diabetes$x2,diabetes$y,group_func_1,lasso_msgps_AICc,corr=0.85,B=100,version = "lars")
B5
B6<-boost(diabetes$x2,diabetes$y,"group_func_1","lasso_msgps_AICc",corr=0.8,B=10,version = "lars")
B6
B7<-boost(diabetes$x2,diabetes$y,"group_func_1","lasso_msgps_AICc",corr=0.75,B=10,version = "lars")
B7
B8<-boost(diabetes$x2,diabetes$y,"group_func_1","lasso_msgps_AICc",corr=0.7,B=10,version = "lars")
B8
B9<-boost(diabetes$x2,diabetes$y,"group_func_1","lasso_msgps_AICc",corr=0.65,B=10,version = "lars")
B9
B10<-boost(diabetes$x2,diabetes$y,"group_func_1","lasso_msgps_AICc",corr=0.60,B=10,version = "lars")
B10
B11<-boost(diabetes$x2,diabetes$y,"group_func_1","lasso_msgps_AICc",corr=0.55,B=10,version = "lars")
B11
B12<-boost(diabetes$x2,diabetes$y,"group_func_1","lasso_msgps_AICc",corr=0.5,B=10,version = "lars")
B12

browstot<-rbind(B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12)
brows<-rbind(B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12)
bcols<-cbind(B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12)

### Coefficients in the different models
barplot(browstot,lty=1,type="h",col=rainbow(nrow(browstot)),beside = TRUE)

### Selected Terms
cbind(coef(b,s=a$index[which.min(a$cv)],mode="fraction")!=0,B1==1,B2==1,B3==1,B4==1,B5==1,B6==1,B7==1,B8==1,B9==1,B10==1,B11==1,B12==1)

### Distribution of confidence indices
layout(t(1:4))
hist(B1,xlim=c(0,1),ylim=c(0,55),main="Histogram lasso_cv_min")
hist(B2,xlim=c(0,1),ylim=c(0,55),main="Histogram lasso_msgps_AICc, c=1")
hist(B3,xlim=c(0,1),ylim=c(0,55),main="Histogram lasso_msgps_AICc, c=.95")
hist(B4,xlim=c(0,1),ylim=c(0,55),main="Histogram lasso_msgps_AICc, c=.90")
hist(B5,xlim=c(0,1),ylim=c(0,55),main="Histogram lasso_msgps_AICc, c=.85")
hist(B6,xlim=c(0,1),ylim=c(0,55),main="Histogram lasso_msgps_AICc, c=.80")
hist(B7,xlim=c(0,1),ylim=c(0,55),main="Histogram lasso_msgps_AICc, c=.75")
hist(B8,xlim=c(0,1),ylim=c(0,55),main="Histogram lasso_msgps_AICc, c=.70")
hist(B9,xlim=c(0,1),ylim=c(0,55),main="Histogram lasso_msgps_AICc, c=.65")
hist(B10,xlim=c(0,1),ylim=c(0,55),main="Histogram lasso_msgps_AICc, c=60")
hist(B11,xlim=c(0,1),ylim=c(0,55),main="Histogram lasso_msgps_AICc, c=.55")
hist(B12,xlim=c(0,1),ylim=c(0,55),main="Histogram lasso_msgps_AICc, c=.50")
layout(1)

### Lasso trajectories
m<-lars(diabetes$x2,diabetes$y)
plot(m,breaks=FALSE,ylim=c(-100,100)*5,xlim=c(0,0.1))

mm<-predict(m,type="coef",mode="lambda")

coul<-rep("black",64)
coul[apply(brows[1:1,,drop=FALSE],2,min)>0.95]<-"red"
coul[apply(brows[1:2,],2,min)>0.95]<-"orange"
coul[apply(brows[1:3,],2,min)>0.95]<-"green"

#matplot(mm$fraction,mm$coefficients,type="l",lty=1,col=coul)

llwd<-rep(1,64)
llwd[apply(brows,2,min)>0.95]<-3

par(mfrow=c(1,2),mar=c(4,4,1,1))
matplot(mm$fraction,mm$coefficients,type="l",lty=1,col=coul,xlim=c(0,0.8),ylim=c(-300-400,550+200),lwd=llwd,xlab="lambda",ylab="coefficients")

matplot(brows,type="l",lty=1,col=coul,lwd=llwd,xaxt="n",xlab="confidence index",ylab="probability of being in the support")
axis(1,1:14,label=1-seq(1,0.35,-0.05))
abline(h=0.95,col="black",lwd=4,lty=3)
layout(1)

