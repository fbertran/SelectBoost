######## Demo selectboost.findgroups Diabetes dataset second order model ########

library(SelectBoost)

# To use all the "cores" of the host. It is often two times the real number of cores of the CPU.
# ncores.found=parallel::detectCores()
# ncores.found

# Set to two by default to match CRAN requirements
ncores.found = 2

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

#run with larger B values (at least 100)
#X,group,corr=1,normalized=TRUE,verbose=FALSE
lll=21 #21
jjj=1 #1
#from corr=1-0.05*(1-1)=1 down to corr=1-0.05*(21-1)=0

cor.diab.x2.norm<-crossprod(diab.x2.norm)
cor.sign.diab.x2.norm <- boost.correlation_sign(cor.diab.x2.norm)

#With the two provided group_func, no need for parallel computing at this step. If it was required for another user-written function, it should be directly integrated within that function code.
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

# parallel fitting of distributions
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Bpar",iii,".x2.adjust",sep=""),boost.adjust(diab.x2.norm,get(paste("B",iii,".x2",sep=""))$groups,cor.sign.diab.x2.norm,use.parallel=TRUE,ncores=ncores.found))
}

# parallel random generation of datasets
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Bpar",iii,".x2.random.1",sep=""),boost.random(diab.x2.norm,get(paste("Bpar",iii,".x2.adjust",sep=""))$Xpass,get(paste("Bpar",iii,".x2.adjust",sep=""))$vmf.params,B=1,use.parallel=TRUE,ncores=ncores.found))
}

# parallel selection using one randomly generated datasets: parallel computation is made with respect to the variables of the dataset.
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Bpar",iii,".x2.apply.1",sep=""),boost.apply(diab.x2.norm,get(paste("Bpar",iii,".x2.random.1",sep="")),diabetes$y,func=lasso_msgps_AICc))
  cat("coefs:",get(paste("Bpar",iii,".x2.apply.1",sep="")),"\n","\n")
}

#Now use the same random generator with adaptative lasso
alasso_msgps_AICc = function(X,Y,penalty="alasso"){
  #lasso_AICc

  fit <- msgps::msgps(X,Y,penalty="alasso")
  round(coef(fit)[-1,2],6)

}

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Badaptpar",iii,".x2.apply.1",sep=""),boost.apply(diab.x2.norm,get(paste("Bpar",iii,".x2.random.1",sep="")),diabetes$y,func=alasso_msgps_AICc))
  cat("coefs:",get(paste("Badaptpar",iii,".x2.apply.1",sep="")),"\n","\n")
}


BBB=100
# parallel selection using BBB randomly generated datasets: parallel computation is made with respect to BBB datasets.
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Bpar",iii,".x2.random.",BBB,sep=""),boost.random(diab.x2.norm,get(paste("Bpar",iii,".x2.adjust",sep=""))$Xpass,get(paste("Bpar",iii,".x2.adjust",sep=""))$vmf.params,B=BBB,use.parallel=TRUE,ncores=ncores.found))
}

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Bpar",iii,".x2.apply.",BBB,sep=""),boost.apply(diab.x2.norm,get(paste("Bpar",iii,".x2.random.",BBB,sep="")),diabetes$y,func=lasso_msgps_AICc,use.parallel=TRUE,ncores=ncores.found))
  #cat(get(paste("Bpar",iii,".x2.apply.",BBB,sep="")),"\n","\n")
}

#No need of parallel computing for that final step
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("B",iii,".x2.select.",BBB,sep=""),boost.select(get(paste("Bpar",iii,".x2.apply.",BBB,sep=""))))
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

#On that example, the algorithm helps selecting reliable variables using the regular lasso.


#Now use the same random generator with adaptative lasso
alasso_msgps_AICc = function(X,Y,penalty="alasso"){
  #lasso_AICc

  fit <- msgps::msgps(X,Y,penalty="alasso")
  round(coef(fit)[-1,2],6)

}

# parallel selection using BBB randomly generated datasets: parallel computation is made with respect to BBB datasets.
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Badaptpar",iii,".x2.apply.",BBB,sep=""),boost.apply(diab.x2.norm,get(paste("Bpar",iii,".x2.random.",BBB,sep="")),diabetes$y,func=alasso_msgps_AICc,use.parallel=TRUE,ncores=ncores.found))
  #  cat(get(paste("Badaptpar",iii,".x2.apply.",BBB,sep="")),"\n","\n")
}

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Badapt",iii,".x2.select.",BBB,sep=""),boost.select(get(paste("Badaptpar",iii,".x2.apply.",BBB,sep=""))))
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

#On that example again, the algorithm helps selecting reliable variables using the adaptative lasso.
