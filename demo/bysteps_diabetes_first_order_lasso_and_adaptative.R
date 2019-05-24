######## Demo selectboost.findgroups Diabetes dataset first order linear model ########
######## parallel ########

library(SelectBoost)
set.seed(2718)

data(diabetes,package="lars")

#### Simple linear model for diabetes dataset
###
### 442 observations
### 10 variables
### 10 terms
## 10 principal effects
# "age" "sex" "bmi" "map" "tc" "ldl" "hdl"
# "tch" "ltg" "glu" "age^2"

diab.x.norm<-boost.normalize(diabetes$x)

#run with larger B values (at least 100)
#X,group,corr=1,normalized=TRUE,verbose=FALSE
lll=5 #21
jjj=5 #1
#from corr=1-0.05*(1-1)=1 down to corr=1-0.05*(21-1)=0

cor.diab.x.norm<-crossprod(diab.x.norm)
cor.sign.diab.x.norm <- boost.correlation_sign(cor.diab.x.norm)

#With the two provided group_func, no need for parallel computing at this step. If it was required for another user-written function, it should be directly integrated within that function code.
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("B",iii,".x",sep=""),boost.findgroups(cor.diab.x.norm,group_func_1,corr=1.0-(iii-1)*.05, verbose=TRUE))
}

len.groups.X <- matrix(NA,nrow=lll,ncol=ncol(diab.x.norm))
for(iii in jjj:lll){
  len.groups.X[iii,]<-sapply(get(paste("B",iii,".x",sep=""))$groups,length)
  cat("corr:",1.0-(iii-1)*.05," -> ",len.groups.X[iii,],"\n")
}

#As expected group length grows from 1 (with Pearson correlation=1) to 10 (number of vars in the dataset, with Pearson correlation=0)
matplot(t(len.groups.X),type="l",col=rev(rainbow(lll)),lwd=c(2,rep(1,lll-2),2))

# parallel fitting of distributions
# use the autoboost function to avoid refitting distributions to the same datasets
# or keep trace of known distributions (see the auto boost code for an example)
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Bpar",iii,".x.adjust",sep=""),boost.adjust(diab.x.norm,get(paste("B",iii,".x",sep=""))$groups,cor.sign.diab.x.norm))
}

# parallel random generation of datasets
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Bpar",iii,".x.random.1",sep=""),boost.random(diab.x.norm,get(paste("Bpar",iii,".x.adjust",sep=""))$Xpass,get(paste("Bpar",iii,".x.adjust",sep=""))$vmf.params,B=1))
}

# parallel selection using one randomly generated datasets: parallel computation is made with respect to the variables of the dataset.
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Bpar",iii,".x.apply.1",sep=""),boost.apply(diab.x.norm,get(paste("Bpar",iii,".x.random.1",sep="")),diabetes$y,func=lasso_msgps_AICc))
  cat("coefs:",get(paste("Bpar",iii,".x.apply.1",sep="")),"\n","\n")
}

BBB=100
# parallel selection using BBB randomly generated datasets: parallel computation is made with respect to BBB datasets.
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Bpar",iii,".x.random.",BBB,sep=""),boost.random(diab.x.norm,get(paste("Bpar",iii,".x.adjust",sep=""))$Xpass,get(paste("Bpar",iii,".x.adjust",sep=""))$vmf.params,B=BBB))
}

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Bpar",iii,".x.apply.",BBB,sep=""),boost.apply(diab.x.norm,get(paste("Bpar",iii,".x.random.",BBB,sep="")),diabetes$y,func=lasso_msgps_AICc))
  #cat(get(paste("Bpar",iii,".x.apply.",BBB,sep="")),"\n","\n")
}

#No need of parallel computing for that final step
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("B",iii,".x.select.",BBB,sep=""),boost.select(get(paste("Bpar",iii,".x.apply.",BBB,sep=""))))
  cat(get(paste("B",iii,".x.select.",BBB,sep="")),"\n","\n")
}

path_probs=NULL
for(iii in jjj:lll){
  path_probs=rbind(path_probs,get(paste("B",iii,".x.select.",BBB,sep="")))
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

set.seed(1)
# parallel selection using BBB randomly generated datasets: parallel computation is made with respect to BBB datasets.
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Badaptpar",iii,".x.apply.",BBB,sep=""),boost.apply(diab.x.norm,get(paste("Bpar",iii,".x.random.",BBB,sep="")),diabetes$y,func=alasso_msgps_AICc))
  #  cat(get(paste("Badaptpar",iii,".x.apply.",BBB,sep="")),"\n","\n")
}

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Badapt",iii,".x.select.",BBB,sep=""),boost.select(get(paste("Badaptpar",iii,".x.apply.",BBB,sep=""))))
  cat(get(paste("Badapt",iii,".x.select.",BBB,sep="")),"\n","\n")
}

path_probs.adapt=NULL
for(iii in jjj:lll){
  path_probs.adapt=rbind(path_probs.adapt,get(paste("Badapt",iii,".x.select.",BBB,sep="")))
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
pos.bar<-barplot(colSums(path_probs.adapt.greater)/length(jjj:lll),beside=TRUE,col=(path_probs.adapt[1,]>.95)+2,ylim=0:1)

#Compute confidence intervals on the proportion of c0 for which proportion of selection of a variable in the model is > .95
confints.list<-simplify2array(lapply(lapply(colSums(path_probs.adapt.greater),binom.test,length(jjj:lll),p=.95),getElement,"conf.int"))
segments(pos.bar,confints.list[1,],pos.bar,confints.list[2,],lwd=2)
abline(h=.95,lty=2,lwd=3,col="green")
abline(h=.90,lty=3,lwd=1,col="orange")
abline(h=.85,lty=3,lwd=1,col="red")
# BMI, SEX, MAP and HDL had a proportion of selection less than .95 but not less than .90.

#On that example again, the algorithm helps selecting reliable variables using the adaptative lasso.









#Now use the same random generator with weighted lasso
#always first, highly penalize 2nd and 3rd
priors.val<-c(0,1e8,1e8,rep(1,ncol(diabetes$x)-3))

set.seed(1)
# parallel selection using BBB randomly generated datasets: parallel computation is made with respect to BBB datasets.
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("Bpar.weighted",iii,".x.apply.",BBB,sep=""),boost.apply(diab.x.norm,get(paste("Bpar",iii,".x.random.",BBB,sep="")),diabetes$y,func=lasso_cv_glmnet_min_weighted,priors=priors.val))
  #  cat(get(paste("Badaptpar",iii,".x.apply.",BBB,sep="")),"\n","\n")
}

for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  assign(paste("B.weighted",iii,".x.select.",BBB,sep=""),boost.select(get(paste("Bpar.weighted",iii,".x.apply.",BBB,sep=""))))
  cat(get(paste("B.weighted",iii,".x.select.",BBB,sep="")),"\n","\n")
}

path_probs.weighted=NULL
for(iii in jjj:lll){
  path_probs.weighted=rbind(path_probs.weighted,get(paste("B.weighted",iii,".x.select.",BBB,sep="")))
}
path_probs.weighted
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#  [1,]    1    0    0 1.00 0.99 0.51 0.79 0.51 1.00  1.00
#  [2,]    1    0    0 1.00 1.00 0.62 0.69 0.62 1.00  1.00
#  [3,]    1    0    0 1.00 0.99 0.58 0.63 0.58 1.00  1.00
#  [4,]    1    0    0 1.00 0.55 0.59 1.00 0.03 1.00  1.00
#  [5,]    1    0    0 1.00 0.54 0.55 1.00 0.02 1.00  1.00
#  [6,]    1    0    0 1.00 0.56 0.57 1.00 0.02 1.00  1.00
#  [7,]    1    0    0 1.00 0.55 0.62 1.00 0.02 1.00  1.00
#  [8,]    1    0    0 1.00 0.72 0.35 1.00 0.32 1.00  1.00
#  [9,]    1    0    0 1.00 0.80 0.45 1.00 0.78 1.00  1.00
# [10,]    1    0    0 1.00 0.79 0.54 1.00 0.76 1.00  1.00
# [11,]    1    0    0 1.00 0.79 0.76 1.00 0.77 1.00  1.00
# [12,]    1    0    0 1.00 0.81 0.54 1.00 0.74 0.85  1.00
# [13,]    1    0    0 1.00 0.70 0.83 1.00 0.96 0.99  1.00
# [14,]    1    0    0 1.00 0.78 0.79 1.00 0.94 1.00  1.00
# [15,]    1    0    0 1.00 0.65 0.63 1.00 0.61 0.98  0.98
# [16,]    1    0    0 1.00 0.56 0.93 1.00 0.86 0.94  0.95
# [17,]    1    0    0 0.96 0.99 0.92 1.00 0.84 0.98  0.91
# [18,]    1    0    0 0.83 0.97 0.99 1.00 0.87 0.97  0.94
# [19,]    1    0    0 0.95 1.00 0.91 1.00 0.93 0.91  0.90
# [20,]    1    0    0 0.98 0.99 0.95 1.00 0.98 0.99  1.00
# [21,]    1    0    0 0.97 0.89 0.99 0.97 0.99 0.98  0.99

#compare with unweighted results (aka priors.val<-rep(1,ncol(diabetes$x)))
#          1    2    3    4    5    6    7    8    9   10
#  [1,] 0.00 1.00 1.00 1.00 1.00 0.00 1.00 1.00 1.00 1.00
#  [2,] 0.00 1.00 1.00 1.00 1.00 0.00 1.00 1.00 1.00 1.00
#  [3,] 0.00 1.00 1.00 1.00 1.00 0.00 1.00 1.00 1.00 1.00
#  [4,] 0.00 1.00 1.00 1.00 0.62 0.64 1.00 0.54 1.00 1.00
#  [5,] 0.00 1.00 1.00 1.00 0.59 0.66 1.00 0.49 1.00 1.00
#  [6,] 0.00 1.00 1.00 1.00 0.68 0.68 1.00 0.36 1.00 1.00
#  [7,] 0.00 1.00 1.00 1.00 0.62 0.62 1.00 0.56 1.00 1.00
#  [8,] 0.00 1.00 1.00 1.00 0.93 0.41 1.00 0.35 1.00 1.00
#  [9,] 0.06 1.00 1.00 1.00 0.96 0.46 1.00 0.80 1.00 1.00
# [10,] 0.03 1.00 1.00 1.00 0.97 0.56 1.00 0.78 1.00 1.00
# [11,] 0.02 1.00 1.00 1.00 0.67 0.77 1.00 0.66 0.99 1.00
# [12,] 0.02 1.00 1.00 1.00 0.75 0.66 1.00 0.66 0.77 1.00
# [13,] 0.27 1.00 1.00 1.00 0.47 0.91 1.00 0.89 0.98 0.99
# [14,] 0.81 1.00 0.98 1.00 0.59 0.81 1.00 0.89 1.00 1.00
# [15,] 0.89 0.99 1.00 1.00 0.61 0.58 1.00 0.63 0.95 0.96
# [16,] 0.97 0.99 1.00 1.00 0.52 0.91 1.00 0.85 0.88 0.97
# [17,] 0.74 0.61 0.99 0.94 0.93 0.86 1.00 0.79 0.97 0.80
# [18,] 0.76 0.39 0.95 0.79 0.96 0.96 1.00 0.84 0.97 0.82
# [19,] 0.91 0.59 0.99 0.91 0.97 0.82 1.00 0.85 0.84 0.83
# [20,] 0.95 1.00 0.94 0.94 0.93 0.94 1.00 0.94 0.97 0.93
# [21,] 0.93 0.96 0.93 0.92 0.72 0.94 0.96 0.96 0.94 0.95
# effect of weighting for the first values of c0 is coherent with the weighting vector
# priors.val<-c(0,1e8,1e8,rep(1,ncol(diabetes$x)-3))
# -> confidence for var 2 and var 3 is equal to one and confidence for var 1 is about .5.


#the proportion of selections of a given variable xi can increase or decrease when c0 decreases (variables are added to the "correlated" group of xi).
matplot(path_probs.weighted,type="l")
#some variables are selected even if c0 decreases (the size of "correlated" group increases)
boxplot(path_probs.weighted,type="l")
#proportion of selection > .95 and its evolution along the path of c0 (21 steps from 1 to 0 by .05). No bar means a proportion of selection less than .95
path_probs.weighted.greater=path_probs.adapt>.95
barplot(path_probs.weighted.greater,beside=TRUE)
#number of selections for the tested path of c0 (21 steps from 1 to 0 by .05)
barplot(colSums(path_probs.weighted.greater),beside=TRUE)
#proportion of selections for the tested path of c0 (21 steps from 1 to 0 by .05)
barplot(colSums(path_probs.weighted.greater)/length(jjj:lll),beside=TRUE,ylim=0:1)
#some variables are always selected, the red ones were discarted with in the initial model with a proportion of inclusions less than .95, the green ones were included.
pos.bar<-barplot(colSums(path_probs.weighted.greater)/length(jjj:lll),beside=TRUE,col=(path_probs.weighted[1,]>.95)+2,ylim=0:1)

#Compute confidence intervals on the proportion of c0 for which proportion of selection of a variable in the model is > .95
confints.list<-simplify2array(lapply(lapply(colSums(path_probs.weighted.greater),binom.test,length(jjj:lll),p=.95),getElement,"conf.int"))
segments(pos.bar,confints.list[1,],pos.bar,confints.list[2,],lwd=2)
abline(h=.95,lty=2,lwd=3,col="green")
abline(h=.90,lty=3,lwd=1,col="orange")
abline(h=.85,lty=3,lwd=1,col="red")
# BMI, SEX, MAP and HDL had a proportion of selection less than .95 but not less than .90.

#On that example again, the algorithm helps selecting reliable variables using the adaptative lasso.

