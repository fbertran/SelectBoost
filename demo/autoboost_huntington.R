rm(list = ls())

######## Demo selectboost Huntington dataset first order linear model ########
######## parallel ########

library(SelectBoost)
set.seed(2718)
# To use all the "cores" of the host. It is often two times the real number of cores of the CPU.
ncores.found=parallel::detectCores()
ncores.found

working_dir="/Users/fbertran/GitHub/temp_files_selectboost/Huntington_community/"

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
X<-as.matrix(X)[,1:50]
Ynum<-(c(rep(0,49),rep(1,20)))

resrautoboost_a1c = autoboost(X, Ynum, verbose = TRUE, steps.seq = c(0.999,0.99,0.9,rev(seq(.70,.84,0.01))))
resrautoboost_f1c = fastboost(X, Ynum, verbose = TRUE, steps.seq = c(0.999,0.99,0.9,rev(seq(.70,.84,0.01))))
resrautoboost_a2c = autoboost(X, Ynum, group = group_func_2, verbose = TRUE, steps.seq = c(0.999,0.99,0.9,rev(seq(.70,.84,0.01))))
resrautoboost_f2c = fastboost(X, Ynum, group = group_func_2, verbose = TRUE, steps.seq = c(0.999,0.99,0.9,rev(seq(.70,.84,0.01))))


layout(matrix(1:4,2))
plot(resrautoboost_a1c)
layout(matrix(1:4,2))
plot(resrautoboost_f1c)
layout(matrix(1:4,2))
plot(resrautoboost_a2c)
layout(matrix(1:4,2))
plot(resrautoboost_f2c)



par(mfrow=c(1,2),mar=c(4,4,1,1))
#Analysis example: trying to set the threshold using raw tajectories
autoboost.res.x.mean <- analyze.selectboost(resrautoboost_f1c, force.dec = FALSE)
autoboost.res.x.mean$crit.func.values.lim
autoboost.res.x.mean$index.lim
plot.analyze.selectboost(autoboost.res.x.mean)

#Analysis example: trying to set the threshold using decreasing tajectories
autoboost.res.x.mean.dec <- analyze.selectboost(resrautoboost_f1c, force.dec = TRUE)
plot.analyze.selectboost(autoboost.res.x.mean.dec)


### With lasso trajectories
m.x<-lars::lars(X, Ynum)
#plot(m.x,breaks=FALSE,ylim=c(-100,100)*5,xlim=c(0,0.1))
mm.x<-predict(m.x,type="coef",mode="lambda")
plot(mm.x$fraction,mm.x$coefficients)


#Custom threshold
autoboost.res.x.custom.final <- analyze.selectboost(resrautoboost_f1c,custom.values.lim=autoboost.res.x.mean$crit.func.values.lim,index.lim=c(4,9,11))

par(mfrow=c(2,2),mar=c(4,4,1,1))
plot.trajC0.selectboost(resrautoboost_f1c,autoboost.res.x.custom.final,lasso.coef.path=mm.x,type.graph="lasso")
plot.trajC0.selectboost(resrautoboost_f1c,autoboost.res.x.custom.final)
plot.trajC0.selectboost(resrautoboost_f1c,autoboost.res.x.custom.final,type.graph="bars")
plot.trajC0.selectboost(resrautoboost_f1c,autoboost.res.x.custom.final,type.x.axis ="scale")




##############






resrautoboost_a1q = autoboost(X, Ynum, verbose = TRUE)
resrautoboost_f1q = fastboost(X, Ynum, verbose = TRUE)
resrautoboost_a2q = autoboost(X, Ynum, group = group_func_2, verbose = TRUE)
resrautoboost_f2q = fastboost(X, Ynum, group = group_func_2, verbose = TRUE)

resrautoboost_a1c_bin = autoboost(X, Y, func=lasso_cv_glmnet_bin_min, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2, steps.seq = c(0.999,0.99,0.9,rev(seq(.70,.84,0.01))))
resrautoboost_f1c_bin = fastboost(X, Y, func=lasso_cv_glmnet_bin_min, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2, steps.seq = c(0.999,0.99,0.9,rev(seq(.70,.84,0.01))))
resrautoboost_a2c_bin = autoboost(X, Y, func=lasso_cv_glmnet_bin_min, group = group_func_2, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2, steps.seq = c(0.999,0.99,0.9,rev(seq(.70,.84,0.01))))
resrautoboost_f2c_bin = fastboost(X, Y, func=lasso_cv_glmnet_bin_min, group = group_func_2, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2, steps.seq = c(0.999,0.99,0.9,rev(seq(.70,.84,0.01))))

layout(matrix(1:4,2))
plot(resrautoboost_a1c_bin)
layout(matrix(1:4,2))
plot(resrautoboost_f1c_bin)
layout(matrix(1:4,2))
plot(resrautoboost_a2c_bin)
layout(matrix(1:4,2))
plot(resrautoboost_f2c_bin)

resrautoboost_a1q_bin = autoboost(X, Y, func=lasso_cv_glmnet_bin_min, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2)
resrautoboost_f1q_bin = fastboost(X, Y, func=lasso_cv_glmnet_bin_min, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2)
resrautoboost_a2q_bin = autoboost(X, Y, func=lasso_cv_glmnet_bin_min, group = group_func_2, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2)
resrautoboost_f2q_bin = fastboost(X, Y, func=lasso_cv_glmnet_bin_min, group = group_func_2, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2)

resrautoboost_a1c_bin_AICc = autoboost(X, Y, func=lasso_glmnet_bin_AICc, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2, steps.seq = c(0.999,0.99,0.9,rev(seq(.70,.84,0.01))))
resrautoboost_f1c_bin_AICc = fastboost(X, Y, func=lasso_glmnet_bin_AICc, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2, steps.seq = c(0.999,0.99,0.9,rev(seq(.70,.84,0.01))))
resrautoboost_a2c_bin_AICc = autoboost(X, Y, func=lasso_glmnet_bin_AICc, group = group_func_2, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2, steps.seq = c(0.999,0.99,0.9,rev(seq(.70,.84,0.01))))
resrautoboost_f2c_bin_AICc = fastboost(X, Y, func=lasso_glmnet_bin_AICc, group = group_func_2, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2, steps.seq = c(0.999,0.99,0.9,rev(seq(.70,.84,0.01))))

resrautoboost_a1q_bin_AICc = autoboost(X, Y, func=lasso_glmnet_bin_AICc, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2)
resrautoboost_f1q_bin_AICc = fastboost(X, Y, func=lasso_glmnet_bin_AICc, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2)
resrautoboost_a2q_bin_AICc = autoboost(X, Y, func=lasso_glmnet_bin_AICc, group = group_func_2, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2)
resrautoboost_f2q_bin_AICc = fastboost(X, Y, func=lasso_glmnet_bin_AICc, group = group_func_2, verbose = TRUE, version="glmnet", use.parallel = FALSE, B=2)





