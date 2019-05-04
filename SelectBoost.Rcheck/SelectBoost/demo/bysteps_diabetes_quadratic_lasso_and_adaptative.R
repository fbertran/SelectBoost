######## Demo selectboost.findgroups Diabetes dataset second order polynomial model ########

library(SelectBoost)
data(diabetes,package="lars")

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

diab.x2.norm<-boost.normalize(diabetes$x2)

lll=5 #21
jjj=1 #1
#from corr=1-0.05*(1-1)=1 down to corr=1-0.05*(21-1)=0

cor.diab.x2.norm<-crossprod(diab.x2.norm)
cor.sign.diab.x2.norm <- boost.correlation_sign(cor.diab.x2.norm)

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("B",iii,".x2",sep=""),boost.findgroups(cor.diab.x2.norm,group_func_1,corr=1.0-(iii-1)*.05, verbose=TRUE))
}

len.groups.x2 <- matrix(NA,nrow=lll,ncol=ncol(diab.x2.norm))
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  len.groups.x2[iii,]<-sapply(get(paste("B",iii,".x2",sep=""))$groups,length)
}

#As expected group length grows from 1 (with Pearson correlation=1) to 10 (number of vars in the dataset, with Pearson correlation=0)
matplot(t(len.groups.x2),type="l",col=rev(rainbow(lll)),lwd=c(2,rep(1,lll-2),2))

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("B",iii,".x2.adjust",sep=""),boost.adjust(diab.x2.norm,get(paste("B",iii,".x2",sep=""))$groups,cor.sign.diab.x2.norm))
}

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("B",iii,".x2.random.1",sep=""),boost.random(diab.x2.norm,get(paste("B",iii,".x2.adjust",sep=""))$Xpass,get(paste("B",iii,".x2.adjust",sep=""))$vmf.params,B=1))
}

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("B",iii,".x2.apply.1",sep=""),boost.apply(diab.x2.norm,get(paste("B",iii,".x2.random.1",sep="")),diabetes$y,func=lasso_msgps_AICc))
  cat(get(paste("B",iii,".x2.apply.1",sep="")),"\n","\n")
}

#Now use the same random generator with adaptative lasso
alasso_msgps_AICc = function(X,Y,penalty="alasso"){
  #lasso_AICc

  fit <- msgps::msgps(X,Y,penalty="alasso")
  round(coef(fit)[-1,2],6)

}

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Badapt",iii,".x2.apply.1",sep=""),boost.apply(diab.x2.norm,get(paste("B",iii,".x2.random.1",sep="")),diabetes$y,func=alasso_msgps_AICc))
  cat(get(paste("Badapt",iii,".x2.apply.1",sep="")),"\n","\n")
}


BBB=100

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("B",iii,".x2.random.",BBB,sep=""),boost.random(diab.x2.norm,get(paste("B",iii,".x2.adjust",sep=""))$Xpass,get(paste("B",iii,".x2.adjust",sep=""))$vmf.params,B=BBB))
}

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("B",iii,".x2.apply.",BBB,sep=""),boost.apply(diab.x2.norm,get(paste("B",iii,".x2.random.",BBB,sep="")),diabetes$y,func=lasso_msgps_AICc))
  cat(get(paste("B",iii,".x2.apply.",BBB,sep="")),"\n","\n")
}

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("B",iii,".x2.select.",BBB,sep=""),boost.select(get(paste("B",iii,".x2.apply.",BBB,sep=""))))
  cat(get(paste("B",iii,".x2.select.",BBB,sep="")),"\n","\n")
}

path_probs=NULL
for(iii in jjj:lll){
  path_probs=rbind(path_probs,get(paste("B",iii,".x2.select.",BBB,sep="")))
}
path_probs

#the proportion of selections of a given variable xi can increase or decrease when c0 decreases (variables are added to the "correlated" group of xi).
matplot(path_probs,type="l")
#some variables are selected even if c0 decreases (the size of "correlated" group increases)
boxplot(path_probs,type="l")
#proportion of selection > .95 and its evolution along the path of c0 (21 steps from 1 to 0 by .05). No bar means a proportion of selection less than .95
path_probs.greater=path_probs>.95
barplot(path_probs.greater,beside=TRUE)
#number of selections for the tested path of c0 (21 steps from 1 to 0 by .05)
barplot(colSums(path_probs.greater),beside=TRUE)
#proportion of selections for the tested path of c0 (21 steps from 1 to 0 by .05)
barplot(colSums(path_probs.greater)/length(jjj:lll),beside=TRUE,ylim=0:1)
#some variables are always selected, the red ones were discarted with in the initial model with a proportion of inclusions less than .95, the green ones were included.
pos.bar<-barplot(colSums(path_probs.greater)/length(jjj:lll),beside=TRUE,col=(path_probs[1,]>.95)+2,ylim=0:1)

#Compute confidence intervals on the proportion of c0 for which proportion of selection of a variable in the model is > .95
confints.list<-simplify2array(lapply(lapply(colSums(path_probs.greater),binom.test,length(jjj:lll),p=.95),getElement,"conf.int"))
segments(pos.bar,confints.list[1,],pos.bar,confints.list[2,],lwd=2)
abline(h=.95,lty=2,lwd=3,col="green")
abline(h=.90,lty=3,lwd=1,col="orange")
abline(h=.85,lty=3,lwd=1,col="red")
# BMI, MAP and HDL successfully passed the pertubation tests, with a proportion of selection not less than .95. GLU performed worse with a proportion of selection less than .95 but not less than .90. SEX, LTG with a proportion of selection less than .90 but not less than .85.

#On that example, the algorithm helps selecting reliable variables using the regular lasso.


#Now use the same random generator with adaptative lasso
alasso_msgps_AICc = function(X,Y,penalty="alasso"){
  #lasso_AICc

  fit <- msgps::msgps(X,Y,penalty="alasso")
  round(coef(fit)[-1,2],6)

}

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Badapt",iii,".x2.apply.",BBB,sep=""),boost.apply(diab.x2.norm,get(paste("B",iii,".x2.random.",BBB,sep="")),diabetes$y,func=alasso_msgps_AICc))
  cat(get(paste("Badapt",iii,".x2.apply.",BBB,sep="")),"\n","\n")
}

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Badapt",iii,".x2.select.",BBB,sep=""),boost.select(get(paste("Badapt",iii,".x2.apply.",BBB,sep=""))))
  cat(get(paste("Badapt",iii,".x2.select.",BBB,sep="")),"\n","\n")
}

path_probs.adapt=NULL
for(iii in jjj:lll){
  path_probs.adapt=rbind(path_probs.adapt,get(paste("Badapt",iii,".x2.select.",BBB,sep="")))
}
path_probs.adapt

#the proportion of selections of a given variable xi can increase or decrease when c0 decreases (variables are added to the "correlated" group of xi).
matplot(path_probs.adapt,type="l")
#some variables are selected even if c0 decreases (the size of "correlated" group increases)
boxplot(path_probs.adapt,type="l")
#proportion of selection > .95 and its evolution along the path of c0 (21 steps from 1 to 0 by .05). No bar means a proportion of selection less than .95
path_probs.adapt.greater=path_probs.adapt>.95
barplot(path_probs.adapt.greater,beside=TRUE)
#number of selections for the tested path of c0 (21 steps from 1 to 0 by .05)
barplot(colSums(path_probs.adapt.greater),beside=TRUE)
#proportion of selections for the tested path of c0 (21 steps from 1 to 0 by .05)
barplot(colSums(path_probs.adapt.greater)/length(jjj:lll),beside=TRUE,ylim=0:1)
#some variables are always selected, the red ones were discarted with in the initial model with a proportion of inclusions less than .95, the green ones were included.
pos.bar<-barplot(colSums(path_probs.adapt.greater)/length(jjj:lll),beside=TRUE,col=(path_probs[1,]>.95)+2,ylim=0:1)

#Compute confidence intervals on the proportion of c0 for which proportion of selection of a variable in the model is > .95
confints.list<-simplify2array(lapply(lapply(colSums(path_probs.adapt.greater),binom.test,length(jjj:lll),p=.95),getElement,"conf.int"))
segments(pos.bar,confints.list[1,],pos.bar,confints.list[2,],lwd=2)
abline(h=.95,lty=2,lwd=3,col="green")
abline(h=.90,lty=3,lwd=1,col="orange")
abline(h=.85,lty=3,lwd=1,col="red")
# BMI successfully passed the pertubation tests, with a proportion of selection not less than .95, and MAP and HDL performed slighly worse with a proportion of selection less than .95 but not less than .90. SEX performed worse with a proportion of selection less than .90 but not less than .85.

#On that example again, the algorithm helps selecting reliable variables using the adaptative lasso.

