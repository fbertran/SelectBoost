rm(list = ls())

######## Demo selectboost Huntington dataset first order linear model ########
######## parallel ########

library(SelectBoost)
set.seed(2718)
# To use all the "cores" of the host. It is often two times the real number of cores of the CPU.
ncores.found=parallel::detectCores()
ncores.found

working_dir="/Users/fbertran/GitHub/temp_files_selectboost/Huntington/"

# huntington data
#### logistic regression model for huntington dataset
###
### 69 observations
### 28087 variables

# HTSFilter
if(!file.exists(paste(working_dir,"Huntington_disease_norm_counts_adjust_HTSFiltered.RData",sep=""))){
  data<-read.table(paste(working_dir,"Huntington_disease_norm_counts_adjust.txt",sep=""),sep = "\t",na.strings = "",header=TRUE,stringsAsFactors = FALSE)
  data<-setNames(data.frame(t(data[,-1])), data[,1])
  #install.packages("HTSFilter")

  library(HTSFilter)
  mat2 <- t(data)
  conds2 <- c(rep("control",49),rep("disease",20))
  filter <- HTSFilter(mat2, conds2, s.min=1, s.max=200, s.len=25)
  save(filter,file=paste(working_dir,"Huntington_disease_norm_counts_adjust_HTSFiltered.RData",sep=""))} else {
    load(file=paste(working_dir,"Huntington_disease_norm_counts_adjust_HTSFiltered.RData",sep=""))
  }
mat3 <- filter$filteredData
dim(mat3)
data <- data.frame(t(mat3))

Y<-factor(c(rep(0,49),rep(1,20)))   # 49 neurologically normal controls & 20 Huntington's disease cases
X<-data
X<-as.matrix(X)


#### logistic regression model for huntington dataset
###
### 69 observations
### 17717 variables
### 17717 terms
## 17717 principal effects

hunt.x.norm<-boost.normalize(X)

#run with larger B values (at least 100)
#X,group,corr=1,normalized=TRUE,verbose=FALSE
lll=3 #21 too big for hunt so 20
jjj=3 #1
#from corr=1-0.05*(1-1)=1 down to corr=1-0.05*(21-1)=0

#Warning two 2.3 Gb matrices
cor.hunt.x.norm<-crossprod(hunt.x.norm)
cor.sign.hunt.x.norm <- boost.correlation_sign(cor.hunt.x.norm)

#Several minutes per step.
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  if(!file.exists(paste(working_dir,paste("B",iii,".x",sep=""),".RData",sep=""))){
    #compute the results
    assign(paste("B",iii,".x",sep=""),boost.findgroups(cor.hunt.x.norm,group_func_1,corr=1.0-(iii-1)*.05, verbose=TRUE))
    #save the results
    save(list=paste("B",iii,".x",sep=""),file=paste(working_dir,paste("B",iii,".x",sep=""),".RData",sep=""))} else {
      #load the results
      load(file=paste(working_dir,paste("B",iii,".x",sep=""),".RData",sep=""))
    }
}

if(!file.exists(paste(working_dir,"len.groups.X",".RData",sep=""))){
  len.groups.X <- matrix(NA,nrow=lll,ncol=ncol(hunt.x.norm))
  for(iii in jjj:lll){
    len.groups.X[iii,]<-attr(get(paste("B",iii,".x",sep=""))$groups,"length.groups")
    cat("corr:",1.0-(iii-1)*.05," -> ",len.groups.X[iii,],"\n")
  }
  save(len.groups.X,file=paste(working_dir,"len.groups.X",".RData",sep=""))} else {
    load(file=paste(working_dir,"len.groups.X",".RData",sep=""))
  }

#As expected group length grows from 1 (with Pearson correlation=1) to 17717 (number of vars in the dataset, with Pearson correlation=0)
#matplot(t(len.groups.X),type="l",col=rev(rainbow(lll)),lwd=c(2,rep(1,lll-2),2))
pdf(paste(working_dir,"len.groups.X.pdf",sep=""))
matplot(t(len.groups.X),type="l",col=rev(rainbow(lll,alpha=.3)),lwd=c(2,rep(1,lll-2),2))
dev.off()

# parallel fitting of distributions
# use the autoboost function to avoid refitting distributions to the same datasets
# or keep trace of known distributions (see the auto boost code for an example)
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  if(!file.exists(paste(working_dir,paste("Bpar",iii,".x.adjust",sep=""),".RData",sep=""))){
    assign(paste("Bpar",iii,".x.adjust",sep=""),boost.adjust(hunt.x.norm,get(paste("B",iii,".x",sep=""))$groups,cor.sign.hunt.x.norm,use.parallel=TRUE,ncores=ncores.found))
    #save the results
    save(list=paste("Bpar",iii,".x.adjust",sep=""),file=paste(working_dir,paste("Bpar",iii,".x.adjust",sep=""),".RData",sep=""))} else {
      #load the results
      load(file=paste(working_dir,paste("Bpar",iii,".x.adjust",sep=""),".RData",sep=""))
    }
}

# parallel random generation of datasets
for(iii in jjj:lll){
  cat("corr:",1.0-(iii-1)*.05,"\n")
  if(!file.exists(paste(working_dir,paste("Bpar",iii,".x.random.1",sep=""),".RData",sep=""))){
    assign(paste("Bpar",iii,".x.random.1",sep=""),boost.random(hunt.x.norm,get(paste("Bpar",iii,".x.adjust",sep=""))$Xpass,get(paste("Bpar",iii,".x.adjust",sep=""))$vmf.params,B=1,use.parallel=TRUE,ncores=ncores.found))
    #save the results
    save(list=paste("Bpar",iii,".x.random.1",sep=""),file=paste(working_dir,paste("Bpar",iii,".x.random.1",sep=""),".RData",sep=""))} else {
      #load the results
      load(file=paste(working_dir,paste("Bpar",iii,".x.random.1",sep=""),".RData",sep=""))
    }
}

# parallel selection using one randomly generated datasets: parallel computation is made with respect to the variables of the dataset.
for(iii in jjj:lll){
  if(!file.exists(paste(working_dir,paste("Bpar",iii,".x.apply.1",sep=""),".RData",sep=""))){
    cat("corr:",1.0-(iii-1)*.05,"\n")
    assign(paste("Bpar",iii,".x.apply.1",sep=""),boost.apply(hunt.x.norm,get(paste("Bpar",iii,".x.random.1",sep="")),Y,func=lasso_cv_glmnet_bin_min))
    cat("coefs:",get(paste("Bpar",iii,".x.apply.1",sep=""))@x,"\n","\n")
    #save the results
    save(list=paste("Bpar",iii,".x.apply.1",sep=""),file=paste(working_dir,paste("Bpar",iii,".x.apply.1",sep=""),".RData",sep=""))} else {
      #load the results
      load(file=paste(working_dir,paste("Bpar",iii,".x.apply.1",sep=""),".RData",sep=""))
      cat("coefs:",get(paste("Bpar",iii,".x.apply.1",sep=""))@x,"\n","\n")
    }
}

#Now use the same random generator with AICc
for(iii in jjj:lll){
  if(!file.exists(paste(working_dir,paste("BAICcpar",iii,".x.apply.1",sep=""),".RData",sep=""))){
    cat("corr:",1.0-(iii-1)*.05,"\n")
    assign(paste("BAICcpar",iii,".x.apply.1",sep=""),boost.apply(hunt.x.norm,get(paste("Bpar",iii,".x.random.1",sep="")),Y,func=lasso_glmnet_bin_AICc))
    cat("coefs:",get(paste("BAICcpar",iii,".x.apply.1",sep=""))[abs(get(paste("BAICcpar",iii,".x.apply.1",sep="")))>0],"\n","\n")
    #save the results
    save(list=paste("BAICcpar",iii,".x.apply.1",sep=""),file=paste(working_dir,paste("BAICcpar",iii,".x.apply.1",sep=""),".RData",sep=""))} else {
      #load the results
      load(file=paste(working_dir,paste("BAICcpar",iii,".x.apply.1",sep=""),".RData",sep=""))
      cat("coefs:",get(paste("BAICcpar",iii,".x.apply.1",sep=""))[abs(get(paste("BAICcpar",iii,".x.apply.1",sep="")))>0],"\n","\n")
    }
}

#Now use the same random generator with BIC
for(iii in jjj:lll){
  if(!file.exists(paste(working_dir,paste("BBICpar",iii,".x.apply.1",sep=""),".RData",sep=""))){
    cat("corr:",1.0-(iii-1)*.05,"\n")
    assign(paste("BBICpar",iii,".x.apply.1",sep=""),boost.apply(hunt.x.norm,get(paste("Bpar",iii,".x.random.1",sep="")),Y,func=lasso_glmnet_bin_BIC))
    cat("coefs:",get(paste("BBICpar",iii,".x.apply.1",sep=""))[abs(get(paste("BBICpar",iii,".x.apply.1",sep="")))>0],"\n","\n")
    #save the results
    save(list=paste("BBICpar",iii,".x.apply.1",sep=""),file=paste(working_dir,paste("BBICpar",iii,".x.apply.1",sep=""),".RData",sep=""))} else {
      #load the results
      load(file=paste(working_dir,paste("BBICpar",iii,".x.apply.1",sep=""),".RData",sep=""))
      cat("coefs:",get(paste("BBICpar",iii,".x.apply.1",sep=""))[abs(get(paste("BBICpar",iii,".x.apply.1",sep="")))>0],"\n","\n")
    }
}

BBB=100
# parallel selection using BBB randomly generated datasets: parallel computation is made with respect to BBB datasets.
for(iii in jjj:lll){
  if(!file.exists(paste(working_dir,paste("Bpar",iii,".x.random.",BBB,sep=""),".RData",sep=""))){
    cat("corr:",1.0-(iii-1)*.05,"\n")
    assign(paste("Bpar",iii,".x.random.",BBB,sep=""),boost.random(hunt.x.norm,get(paste("Bpar",iii,".x.adjust",sep=""))$Xpass,get(paste("Bpar",iii,".x.adjust",sep=""))$vmf.params,B=BBB,use.parallel=TRUE,ncores=ncores.found))
    #save the results
    save(list=paste("Bpar",iii,".x.random.",BBB,sep=""),file=paste(working_dir,paste("Bpar",iii,".x.random.",BBB,sep=""),".RData",sep=""))} else {
      #load the results
      load(file=paste(working_dir,paste("Bpar",iii,".x.random.",BBB,sep=""),".RData",sep=""))
    }
}

for(iii in jjj:lll){
  if(!file.exists(paste(working_dir,paste("Bpar",iii,".x.apply.",BBB,sep=""),".RData",sep=""))){
    cat("corr:",1.0-(iii-1)*.05,"\n")
    assign(paste("Bpar",iii,".x.apply.",BBB,sep=""),boost.apply(hunt.x.norm,get(paste("Bpar",iii,".x.random.",BBB,sep="")),Y,func=lasso_cv_glmnet_bin_min,use.parallel=TRUE,ncores=ncores.found))
    #cat(get(paste("Bpar",iii,".x.apply.",BBB,sep="")),"\n","\n")
    #save the results
    save(list=paste("Bpar",iii,".x.apply.",BBB,sep=""),file=paste(working_dir,paste("Bpar",iii,".x.apply.",BBB,sep=""),".RData",sep=""))} else {
      #load the results
      load(file=paste(working_dir,paste("Bpar",iii,".x.apply.",BBB,sep=""),".RData",sep=""))
    }
}

#Now use the same random generator with AICc
for(iii in jjj:lll){
  if(!file.exists(paste(working_dir,paste("BAICcpar",iii,".x.apply.",BBB,sep=""),".RData",sep=""))){
    cat("corr:",1.0-(iii-1)*.05,"\n")
    assign(paste("BAICcpar",iii,".x.apply.",BBB,sep=""),boost.apply(hunt.x.norm,get(paste("Bpar",iii,".x.random.",BBB,sep="")),Y,func=lasso_glmnet_bin_AICc))
    cat("coefs:",get(paste("BAICcpar",iii,".x.apply.",BBB,sep=""))[abs(get(paste("BAICcpar",iii,".x.apply.",BBB,sep="")))>0],"\n","\n")
    #save the results
    save(list=paste("BAICcpar",iii,".x.apply.",BBB,sep=""),file=paste(working_dir,paste("BAICcpar",iii,".x.apply.",BBB,sep=""),".RData",sep=""))} else {
      #load the results
      load(file=paste(working_dir,paste("BAICcpar",iii,".x.apply.",BBB,sep=""),".RData",sep=""))
      cat("coefs:",get(paste("BAICcpar",iii,".x.apply.",BBB,sep=""))[abs(get(paste("BAICcpar",iii,".x.apply.",BBB,sep="")))>0],"\n","\n")
    }
}


#Now use the same random generator with BIC
for(iii in jjj:lll){
  if(!file.exists(paste(working_dir,paste("BBICpar",iii,".x.apply.",BBB,sep=""),".RData",sep=""))){
    cat("corr:",1.0-(iii-1)*.05,"\n")
    assign(paste("BBICpar",iii,".x.apply.",BBB,sep=""),boost.apply(hunt.x.norm,get(paste("Bpar",iii,".x.random.",BBB,sep="")),Y,func=lasso_glmnet_bin_BIC))
    cat("coefs:",get(paste("BBICpar",iii,".x.apply.",BBB,sep=""))[abs(get(paste("BBICpar",iii,".x.apply.",BBB,sep="")))>0],"\n","\n")
    #save the results
    save(list=paste("BBICpar",iii,".x.apply.",BBB,sep=""),file=paste(working_dir,paste("BBICpar",iii,".x.apply.",BBB,sep=""),".RData",sep=""))} else {
      #load the results
      load(file=paste(working_dir,paste("BBICpar",iii,".x.apply.",BBB,sep=""),".RData",sep=""))
      cat("coefs:",get(paste("BBICpar",iii,".x.apply.",BBB,sep=""))[abs(get(paste("BBICpar",iii,".x.apply.",BBB,sep="")))>0],"\n","\n")
    }
}

#No need of parallel computing for that final step
for(iii in jjj:lll){
  if(!file.exists(paste(working_dir,paste("B",iii,".x.select.",BBB,sep=""),".RData",sep=""))){
    cat("corr:",1.0-(iii-1)*.05,"\n")
    assign(paste("B",iii,".x.select.",BBB,sep=""),boost.select(get(paste("Bpar",iii,".x.apply.",BBB,sep=""))))
    cat((get(paste("B",iii,".x.select.",BBB,sep="")))[abs(get(paste("B",iii,".x.select.",BBB,sep="")))>0],"\n","\n")
    save(list=paste("B",iii,".x.select.",BBB,sep=""),file=paste(working_dir,paste("B",iii,".x.select.",BBB,sep=""),".RData",sep=""))} else {
      #load the results
      load(file=paste(working_dir,paste("B",iii,".x.select.",BBB,sep=""),".RData",sep=""))
      cat((get(paste("B",iii,".x.select.",BBB,sep="")))[abs(get(paste("B",iii,".x.select.",BBB,sep="")))>0],"\n","\n")
    }
}

for(iii in jjj:lll){
  if(!file.exists(paste(working_dir,paste("BAICc",iii,".x.select.",BBB,sep=""),".RData",sep=""))){
    cat("corr:",1.0-(iii-1)*.05,"\n")
    assign(paste("BAICc",iii,".x.select.",BBB,sep=""),boost.select(get(paste("BAICcpar",iii,".x.apply.",BBB,sep=""))))
    cat((get(paste("BAICc",iii,".x.select.",BBB,sep="")))[abs(get(paste("BAICc",iii,".x.select.",BBB,sep="")))>0],"\n","\n")
    save(list=paste("BAICc",iii,".x.select.",BBB,sep=""),file=paste(working_dir,paste("BAICc",iii,".x.select.",BBB,sep=""),".RData",sep=""))} else {
      #load the results
      load(file=paste(working_dir,paste("BAICc",iii,".x.select.",BBB,sep=""),".RData",sep=""))
    }
}

for(iii in jjj:lll){
  if(!file.exists(paste(working_dir,paste("BBIC",iii,".x.select.",BBB,sep=""),".RData",sep=""))){
    cat("corr:",1.0-(iii-1)*.05,"\n")
    assign(paste("BBIC",iii,".x.select.",BBB,sep=""),boost.select(get(paste("BBICpar",iii,".x.apply.",BBB,sep=""))))
    cat((get(paste("BBIC",iii,".x.select.",BBB,sep="")))[abs(get(paste("BBIC",iii,".x.select.",BBB,sep="")))>0],"\n","\n")
    save(list=paste("BBIC",iii,".x.select.",BBB,sep=""),file=paste(working_dir,paste("BBIC",iii,".x.select.",BBB,sep=""),".RData",sep=""))} else {
      #load the results
      load(file=paste(working_dir,paste("BBIC",iii,".x.select.",BBB,sep=""),".RData",sep=""))
    }
}

path_probs=NULL
for(iii in jjj:lll){
  path_probs=rbind(path_probs,get(paste("B",iii,".x.select.",BBB,sep="")))
}
path_probs[,apply(path_probs>0,2,any)]

path_probs_AICc=NULL
for(iii in jjj:lll){
  path_probs_AICc=rbind(path_probs_AICc,get(paste("BAICc",iii,".x.select.",BBB,sep="")))
}
path_probs_AICc[,apply(path_probs_AICc>0,2,any)]

path_probs_BIC=NULL
for(iii in jjj:lll){
  path_probs_BIC=rbind(path_probs_BIC,get(paste("BBIC",iii,".x.select.",BBB,sep="")))
}
path_probs_BIC[,apply(path_probs_BIC>0,2,any)]


colnames(path_probs_AICc)<-colnames(path_probs)[-1]
colnames(path_probs_BIC)<-colnames(path_probs)[-1]


#the proportion of selections of a given variable xi can increase or decrease when c0 decreases (variables are added to the "correlated" group of xi).
matplot(path_probs_AICc,type="l")
#some variables are selected even if c0 decreases (the size of "correlated" group increases)
boxplot(path_probs_AICc,type="l")
#proportion of selection > .95 and its evolution along the path of c0 (21 steps from 1 to 0 by .05). No bar means a proportion of selection less than .95
path_probs_AICc.greater=path_probs_AICc>.95
barplot(path_probs_AICc.greater,beside=TRUE)
#number of selections for the tested path of c0 (21 steps from 1 to 0 by .05)
barplot(colSums(path_probs_AICc.greater),beside=TRUE)
#proportion of selections for the tested path of c0 (21 steps from 1 to 0 by .05)
barplot(colSums(path_probs_AICc.greater)/length(jjj:lll),beside=TRUE,ylim=0:1)
#some variables are always selected, the red ones were discarted with in the initial model with a proportion of inclusions less than .95, the green ones were included.
pos.bar_AICc<-barplot(colSums(path_probs_AICc.greater)/length(jjj:lll),beside=TRUE,col=(path_probs_AICc[1,]>.95)+2,ylim=0:1)

#Compute confidence intervals on the proportion of c0 for which proportion of selection of a variable in the model is > .95
confints.list_AICc<-simplify2array(lapply(lapply(colSums(path_probs_AICc.greater),binom.test,length(jjj:lll),p=.95),getElement,"conf.int"))
segments(pos.bar_AICc,confints.list_AICc[1,],pos.bar_AICc,confints.list_AICc[2,],lwd=2)
abline(h=.95,lty=2,lwd=3,col="green")
abline(h=.90,lty=3,lwd=1,col="orange")
abline(h=.85,lty=3,lwd=1,col="red")
# BMI, MAP and HDL successfully passed the pertubation tests, with a proportion of selection not less than .95. GLU, SEX and LTF performed worse with a proportion of selection less than .95 but not less than .90.














#the proportion of selections of a given variable xi can increase or decrease when c0 decreases (variables are added to the "correlated" group of xi).
matplot(path_probs_BIC,type="l")
#some variables are selected even if c0 decreases (the size of "correlated" group increases)
boxplot(path_probs_BIC,type="l")
#proportion of selection > .95 and its evolution along the path of c0 (21 steps from 1 to 0 by .05). No bar means a proportion of selection less than .95
path_probs_BIC.greater=path_probs_BIC>.95
barplot(path_probs_BIC.greater,beside=TRUE)
#number of selections for the tested path of c0 (21 steps from 1 to 0 by .05)
barplot(colSums(path_probs_BIC.greater),beside=TRUE)
#proportion of selections for the tested path of c0 (21 steps from 1 to 0 by .05)
barplot(colSums(path_probs_BIC.greater)/length(jjj:lll),beside=TRUE,ylim=0:1)
#some variables are always selected, the red ones were discarted with in the initial model with a proportion of inclusions less than .95, the green ones were included.
pos.bar_BIC<-barplot(colSums(path_probs_BIC.greater)/length(jjj:lll),beside=TRUE,col=(path_probs[1,]>.95)+2,ylim=0:1)

#Compute confidence intervals on the proportion of c0 for which proportion of selection of a variable in the model is > .95
confints.list<-simplify2array(lapply(lapply(colSums(path_probs_BIC.greater),binom.test,length(jjj:lll),p=.95),getElement,"conf.int"))
segments(pos.bar_BIC,confints.list_BIC[1,],pos.bar_BIC,confints.list_BIC[2,],lwd=2)
abline(h=.95,lty=2,lwd=3,col="green")
abline(h=.90,lty=3,lwd=1,col="orange")
abline(h=.85,lty=3,lwd=1,col="red")
# BMI, MAP and HDL successfully passed the pertubation tests, with a proportion of selection not less than .95. GLU, SEX and LTF performed worse with a proportion of selection less than .95 but not less than .90.








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
# BMI, MAP and HDL successfully passed the pertubation tests, with a proportion of selection not less than .95. GLU, SEX and LTF performed worse with a proportion of selection less than .95 but not less than .90.
