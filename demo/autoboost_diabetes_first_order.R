######## Demo autoboost Diabetes dataset first order linear model ########
######## with Parallel support ########

# Change ncores to match the number of cores you want to use
# ncores.found=parallel::detectCores()
# ncores.found

# Set to two by default to match CRAN requirements
ncores.found = 2

library(SelectBoost)
data(diabetes,package="lars")

#### Simple linear model for diabetes dataset
###
### 442 observations
### 10 variables
### 10 terms
## 10 principal effects
# "age" "sex" "bmi" "map" "tc" "ldl" "hdl"
# "tch" "ltg" "glu" "age^2"

autoboost.res.x<-autoboost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corrfunc="crossprod",B=100, use.parallel=TRUE, ncores=ncores.value,verbose=TRUE)
layout(matrix(1:4,2))
plot(autoboost.res.x)
#save(autoboost.res.x,file="data/autoboost.res.x.RData",compress = "xz")
fastboost.res.x<-fastboost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corrfunc="crossprod",B=100, use.parallel=TRUE, ncores=ncores.value,verbose=TRUE)
layout(matrix(1:4,2))
plot(fastboost.res.x)
#save(fastboost.res.x,file="data/fastboost.res.x.RData",compress = "xz")

autoboost.res.x.lin<-autoboost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corrfunc="crossprod",B=100, use.parallel=TRUE, ncores=ncores.value,verbose=TRUE,step.scale = "linear")
plot(autoboost.res.x.lin)

autoboost.res.x.pear<-autoboost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corrfunc="crossprod",B=100, use.parallel=TRUE, ncores=ncores.value,verbose=TRUE,step.limit="Pearson")
plot(autoboost.res.x.pear)

autoboost.res.x.lin.pear<-autoboost(diabetes$x,diabetes$y,group_func_1,lasso_msgps_AICc,corrfunc="crossprod",B=100, use.parallel=TRUE, ncores=ncores.value,verbose=TRUE,step.scale = "linear",step.limit="Pearson")
plot(autoboost.res.x.lin.pear)

#Analysis example: trying to set the threshold
autoboost.res.x.mean <- summary(autoboost.res.x,crit.func = mean,crit.int="mean", alpha.conf.level = .99, trim = 0)
plot(autoboost.res.x.mean)
trajC0(autoboost.res.x,autoboost.res.x.mean)

### With lasso trajectories
m.x<-lars::lars(diabetes$x,diabetes$y)
#plot(m.x,breaks=FALSE,ylim=c(-100,100)*5,xlim=c(0,0.1))
mm.x<-lars::predict.lars(m.x,type="coef",mode="lambda")

par(mfrow=c(2,2),mar=c(4,4,1,1))
trajC0(autoboost.res.x,autoboost.res.x.mean,lasso.coef.path=mm.x,type.graph="lasso")
trajC0(autoboost.res.x,autoboost.res.x.mean)
trajC0(autoboost.res.x,autoboost.res.x.mean,type.graph="bars")
trajC0(autoboost.res.x,autoboost.res.x.mean,type.x.axis ="scale")


#Analysis example: custom threshold
autoboost.res.x.custom <- summary(autoboost.res.x,custom.values.lim=c(.85,.9,.95),index.lim=c(3,5,7))

par(mfrow=c(2,2),mar=c(4,4,1,1))
trajC0(autoboost.res.x,autoboost.res.x.custom,lasso.coef.path=mm.x,type.graph="lasso")
trajC0(autoboost.res.x,autoboost.res.x.custom)
trajC0(autoboost.res.x,autoboost.res.x.custom,type.graph="bars")
trajC0(autoboost.res.x,autoboost.res.x.custom,type.x.axis ="scale")


# pdf("autoboost.res.x.mean.pdf")
# par(mfrow=c(2,2),mar=c(4,4,1,1))
# trajC0(autoboost.res.x,autoboost.res.x.mean,lasso.coef.path=mm.x,type.graph="lasso")
# trajC0(autoboost.res.x,autoboost.res.x.mean)
# trajC0(autoboost.res.x,autoboost.res.x.mean,type.graph="bars")
# trajC0(autoboost.res.x,autoboost.res.x.mean,type.x.axis ="scale")
# dev.off()


# pdf("autoboost.res.x.custom.pdf")
# par(mfrow=c(2,2),mar=c(4,4,1,1))
# trajC0(autoboost.res.x,autoboost.res.x.custom,lasso.coef.path=mm.x,type.graph="lasso")
# trajC0(autoboost.res.x,autoboost.res.x.custom)
#
# trajC0(autoboost.res.x,autoboost.res.x.custom,type.graph="bars")
# trajC0(autoboost.res.x,autoboost.res.x.custom,type.x.axis ="scale")
# dev.off()


#Other criteria
autoboost.res.x.median <- summary(autoboost.res.x,crit.func = median,crit.int="median")
plot(autoboost.res.x.median)
trajC0(autoboost.res.x,autoboost.res.x.median)
trajC0(autoboost.res.x,autoboost.res.x.median,type.graph="bars")
trajC0(autoboost.res.x,autoboost.res.x.median,type.x.axis ="scale")

autoboost.res.x.mean2 <- summary(autoboost.res.x,crit.func = mean,crit.int="median", alpha.conf.level = .99, trim = 0)
plot(autoboost.res.x.mean2)
trajC0(autoboost.res.x,autoboost.res.x.mean2)
trajC0(autoboost.res.x,autoboost.res.x.mean2,type.graph="bars")
trajC0(autoboost.res.x,autoboost.res.x.mean2,type.x.axis ="scale")

autoboost.res.x.mean3 <- summary(autoboost.res.x,crit.func = mean,crit.int="mean", alpha.conf.level = .99, trim = 0.15)
plot(autoboost.res.x.mean3)
trajC0(autoboost.res.x,autoboost.res.x.mean3)
trajC0(autoboost.res.x,autoboost.res.x.mean3,type.graph="bars")
trajC0(autoboost.res.x,autoboost.res.x.mean3,type.x.axis ="scale")

#rm(autoboost.res.x)


#Adaptative lasso
alasso_msgps_AICc = function(X,Y,penalty="alasso"){
  #lasso_AICc

  fit <- msgps::msgps(X,Y,penalty="alasso")
  round(coef(fit)[-1,2],6)

}

autoboost.res.x.adapt<-autoboost(diabetes$x,diabetes$y,group_func_1,alasso_msgps_AICc,B=100, use.parallel=TRUE, ncores=ncores.value,verbose=TRUE)
#save(autoboost.res.x.adapt,file="data/autoboost.res.x.adapt.RData",compress = "xz")
fastboost.res.x.adapt<-fastboost(diabetes$x,diabetes$y,group_func_1,alasso_msgps_AICc,B=100, use.parallel=TRUE, ncores=ncores.value,verbose=TRUE)
#save(fastboost.res.x.adapt,file="data/fastboost.res.x.adapt.RData",compress = "xz")

autoboost.res.x.adapt.mean <- summary(autoboost.res.x.adapt)
plot(autoboost.res.x.adapt.mean)
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean)
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean,type.graph="bars")
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean,type.x.axis ="scale")


### With lasso trajectories
m.x<-lars::lars(diabetes$x,diabetes$y)
#plot(m.x,breaks=FALSE,ylim=c(-100,100)*5,xlim=c(0,0.1))
mm.x<-lars::predict.lars(m.x,type="coef",mode="lambda")

par(mfrow=c(2,2),mar=c(4,4,1,1))
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean,lasso.coef.path=mm.x,type.graph="lasso")
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean)
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean,type.graph="bars")
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean,type.x.axis ="scale")

#Analysis example: custom threshold
autoboost.res.x.adapt.custom <- summary(autoboost.res.x.adapt,custom.values.lim=c(.85,.9,.95),index.lim=c(3,5,7))

par(mfrow=c(2,2),mar=c(4,4,1,1))
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.custom,lasso.coef.path=mm.x,type.graph="lasso")
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.custom)
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.custom,type.graph="bars")
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.custom,type.x.axis ="scale")

# pdf("autoboost.res.x.adapt.mean.pdf")
# par(mfrow=c(2,2),mar=c(4,4,1,1))
# trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean,lasso.coef.path=mm.x,type.graph="lasso")
# trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean)
# trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.custom,type.graph="bars")
# trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean,type.x.axis ="scale")
# dev.off()

# pdf("autoboost.res.x.adapt.custom.pdf")
# par(mfrow=c(2,2),mar=c(4,4,1,1))
# trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.custom,lasso.coef.path=mm.x,type.graph="lasso")
# trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.custom)
# trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.custom,type.graph="bars")
# trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.custom,type.x.axis ="scale")
# dev.off()

#Other criteria
autoboost.res.x.adapt.median <- summary(autoboost.res.x.adapt,crit.func = median,crit.int="median", alpha.conf.level = .99, trim = 0)
plot(autoboost.res.x.adapt.median)
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.median)
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.median,type.graph="bars")
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.median,type.x.axis = "scale")

autoboost.res.x.adapt.mean2 <- summary(autoboost.res.x.adapt,crit.func = mean,crit.int="median", alpha.conf.level = .99, trim = 0)
plot(autoboost.res.x.adapt.mean2)
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean2)
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean2,type.graph="bars")
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean2,type.x.axis = "scale")

autoboost.res.x.adapt.mean3 <- summary(autoboost.res.x.adapt,crit.func = mean,crit.int="mean", alpha.conf.level = .99, trim = 0.15)
plot(autoboost.res.x.adapt.mean3)
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean3)
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean3,type.graph = "bars")
trajC0(autoboost.res.x.adapt,autoboost.res.x.adapt.mean3,type.x.axis = "scale")


#rm(autoboost.res.x.adapt)


#Diabetes second order
#setwd("your_working_directory")
#load("autoboost.res.x2.RData")
#load("autoboost.res.x2.adapt.RData")
#library(SelectBoost)

autoboost.res.x2<-autoboost(diabetes$x2,diabetes$y,group_func_1,lasso_msgps_AICc,B=100, use.parallel=TRUE, ncores=ncores.value,verbose=TRUE)
#save(autoboost.res.x2,file="data/autoboost.res.x2.RData",compress = "xz")
fastboost.res.x2<-fastboost(diabetes$x2,diabetes$y,group_func_1,lasso_msgps_AICc,B=100, use.parallel=TRUE, ncores=ncores.value,verbose=TRUE)
#save(fastboost.res.x2,file="data/fastboost.res.x2.RData",compress = "xz")

autoboost.res.x2.mean <- summary(autoboost.res.x2)
plot(autoboost.res.x2.mean)
trajC0(autoboost.res.x2,autoboost.res.x2.mean)
trajC0(autoboost.res.x2,autoboost.res.x2.mean,type.graph ="bars")
trajC0(autoboost.res.x2,autoboost.res.x2.mean,type.x.axis ="scale")


### With lasso trajectories
m.x2<-lars::lars(diabetes$x2,diabetes$y)
#plot(m.x2,breaks=FALSE,ylim=c(-100,100)*5,xlim=c(0,0.1))
mm.x2<-lars::predict.lars(m.x2,type="coef",mode="lambda")

autoboost.res.x2.mean <- summary(autoboost.res.x2,crit.func = mean,crit.int="mean", alpha.conf.level = .99)
plot(autoboost.res.x2.mean)
trajC0(autoboost.res.x2,autoboost.res.x2.mean)

par(mfrow=c(2,2),mar=c(4,4,1,1))
trajC0(autoboost.res.x2,autoboost.res.x2.mean,lasso.coef.path=mm.x2,type.graph="lasso")
trajC0(autoboost.res.x2,autoboost.res.x2.mean)
trajC0(autoboost.res.x2,autoboost.res.x2.mean,type.graph="bars")
trajC0(autoboost.res.x2,autoboost.res.x2.mean,type.x.axis ="scale")


# pdf("autoboost.res.x2.mean.pdf")
# par(mfrow=c(2,2),mar=c(4,4,1,1))
# trajC0(autoboost.res.x2,autoboost.res.x2.mean,lasso.coef.path=mm.x2,type.graph="lasso")
# trajC0(autoboost.res.x2,autoboost.res.x2.mean)
# trajC0(autoboost.res.x2,autoboost.res.x2.mean,type.graph="bars")
# trajC0(autoboost.res.x2,autoboost.res.x2.mean,type.x.axis ="scale")
# dev.off()

#Custom analysis
autoboost.res.x2.custom <- summary(autoboost.res.x2,custom.values.lim=c(.85,.9,.95),index.lim=c(3,5,7))
plot(autoboost.res.x2.custom)
trajC0(autoboost.res.x2,autoboost.res.x2.custom)

par(mfrow=c(2,2),mar=c(4,4,1,1))
trajC0(autoboost.res.x2,autoboost.res.x2.custom,lasso.coef.path=mm.x2,type.graph="lasso")
trajC0(autoboost.res.x2,autoboost.res.x2.custom)
trajC0(autoboost.res.x2,autoboost.res.x2.custom,type.graph="bars")
trajC0(autoboost.res.x2,autoboost.res.x2.custom,type.x.axis ="scale")


# pdf("autoboost.res.x2.mean.pdf")
# par(mfrow=c(1,2),mar=c(4,4,1,1))
# trajC0(autoboost.res.x2,autoboost.res.x2.mean,lasso.coef.path=mm.x2,type.graph="lasso")
# trajC0(autoboost.res.x2,autoboost.res.x2.mean)
# trajC0(autoboost.res.x2,autoboost.res.x2.mean,type.graph="bars")
# trajC0(autoboost.res.x2,autoboost.res.x2.mean,type.x.axis ="scale")
# dev.off()

# pdf("autoboost.res.x2.custom.pdf")
# par(mfrow=c(1,2),mar=c(4,4,1,1))
# trajC0(autoboost.res.x2,autoboost.res.x2.custom,lasso.coef.path=mm.x2,type.graph="lasso")
# trajC0(autoboost.res.x2,autoboost.res.x2.custom)
# trajC0(autoboost.res.x2,autoboost.res.x2.custom,type.graph="bars")
# trajC0(autoboost.res.x2,autoboost.res.x2.custom,type.x.axis ="scale")
# dev.off()


#Other criteria
autoboost.res.x2.median <- summary(autoboost.res.x2)
plot(autoboost.res.x2.median)
trajC0(autoboost.res.x2,autoboost.res.x2.median)
trajC0(autoboost.res.x2,autoboost.res.x2.median,type.graph ="bars")
trajC0(autoboost.res.x2,autoboost.res.x2.median,type.x.axis ="scale")

autoboost.res.x2.mean2 <- summary(autoboost.res.x2,crit.func = mean,crit.int="median", alpha.conf.level = .99, trim = 0)
plot(autoboost.res.x2.mean2)
trajC0(autoboost.res.x2,autoboost.res.x2.mean2)

autoboost.res.x2.mean3 <- summary(autoboost.res.x2,crit.func = mean,crit.int="mean", alpha.conf.level = .99, trim = 0.15)
plot(autoboost.res.x2.mean3)
trajC0(autoboost.res.x2,autoboost.res.x2.mean3,type.x.axis = "scale")


#rm(autoboost.res.x2)

#Adaptative lasso, diabetes second order

autoboost.res.x2.adapt<-autoboost(diabetes$x2,diabetes$y,group_func_1,alasso_msgps_AICc,B=100, use.parallel=TRUE, ncores=ncores.value,verbose=TRUE)
#save(autoboost.res.x2.adapt,file="data/autoboost.res.x2.adapt.RData",compress = "xz")
fastboost.res.x2.adapt<-fastboost(diabetes$x2,diabetes$y,group_func_1,lasso_msgps_AICc,B=100, use.parallel=TRUE, ncores=ncores.value,verbose=TRUE)
#save(fastboost.res.x2.adapt,file="data/fastboost.res.x2.adapt.RData",compress = "xz")


autoboost.res.x2.adapt.mean <- summary(autoboost.res.x2.adapt)
autoboost.res.x2.adapt.mean$freq.dec
autoboost.res.x2.adapt.mean$freq.dec.lims
autoboost.res.x2.adapt.mean <- summary(autoboost.res.x2.adapt, index.lim=c(5,7,10))
plot(autoboost.res.x2.adapt.mean)
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.mean)
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.mean,type.x.axis ="scale")

### With lasso trajectories
m.x2<-lars::lars(diabetes$x2,diabetes$y)
#plot(m.x2,breaks=FALSE,ylim=c(-100,100)*5,xlim=c(0,0.1))
mm.x2<-lars::predict.lars(m.x2,type="coef",mode="lambda")

par(mfrow=c(2,2),mar=c(4,4,1,1))
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.mean,lasso.coef.path=mm.x2,type.graph="lasso")
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.mean)
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.mean,type.graph="bars")
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.mean,type.x.axis ="scale")

#Custom, force dec = TRUE
autoboost.res.x2.adapt.custom <- summary(autoboost.res.x2.adapt,custom.values.lim=c(.85,.9,.95),index.lim=c(3,5,7),force.dec=TRUE)
autoboost.res.x2.adapt.custom$freq.dec
#To help selecting the index.lim value
autoboost.res.x2.adapt.custom$freq.dec.lims
plot(autoboost.res.x2.adapt.custom)
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.custom)
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.custom,type.x.axis ="scale")


#Find the limit value using raw trajectories (to correctily assess the resampling effect) then use it with non-increasing constrained trajectories
par(mfrow=c(2,2),mar=c(4,4,1,1))
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.custom,lasso.coef.path=mm.x2,type.graph="lasso")
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.custom)
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.custom,type.graph="bars")
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.custom,type.x.axis ="scale")


#Custom, force dec = FALSE
autoboost.res.x2.adapt.custom <- summary(autoboost.res.x2.adapt,custom.values.lim=c(.85,.9,.95),index.lim=c(3,4,5),force.dec=FALSE)
autoboost.res.x2.adapt.custom$freq.dec
#To help selecting the index.lim value
autoboost.res.x2.adapt.custom$freq.dec.lims

par(mfrow=c(2,2),mar=c(4,4,1,1))
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.custom,lasso.coef.path=mm.x2,type.graph="lasso")
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.custom)
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.custom,type.graph="bars")
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.custom,type.x.axis ="scale")



#Custom, force dec = TRUE avec limites force dec = FALSE
autoboost.res.x2.adapt.auto <-summary(autoboost.res.x2.adapt)
par(mfrow=c(2,2),mar=c(4,4,1,1))
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.auto,lasso.coef.path=mm.x2,type.graph="lasso")
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.auto)
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.auto,type.graph="bars")
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.auto,type.x.axis ="scale")





# pdf("autoboost.res.x2.adapt.mean.pdf")
# par(mfrow=c(2,2),mar=c(4,4,1,1))
# trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.mean,lasso.coef.path=mm.x2,type.graph="lasso")
# trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.mean)
# trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.mean,type.graph="bars")
# trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.mean,type.x.axis ="scale")
# dev.off()

# pdf("autoboost.res.x2.adapt.custom.pdf")
# par(mfrow=c(2,2),mar=c(4,4,1,1))
# trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.custom,lasso.coef.path=mm.x2,type.graph="lasso")
# trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.custom)
# trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.custom,type.graph="bars")
# trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.custom,type.x.axis ="scale")
# dev.off()

#Other criteria
autoboost.res.x2.adapt.mean <- summary(autoboost.res.x2.adapt,crit.func = mean,crit.int="mean", alpha.conf.level = .99, trim = 0)
plot(autoboost.res.x2.adapt.mean)
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.mean)

autoboost.res.x2.adapt.mean2 <- summary(autoboost.res.x2.adapt,crit.func = mean,crit.int="median", alpha.conf.level = .99, trim = 0)
plot(autoboost.res.x2.adapt.mean2)
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.mean2)

autoboost.res.x2.adapt.mean3 <- summary(autoboost.res.x2.adapt,crit.func = mean,crit.int="mean", alpha.conf.level = .99, trim = 0.15)
plot(autoboost.res.x2.adapt.mean3)
trajC0(autoboost.res.x2.adapt,autoboost.res.x2.adapt.mean3,type.x.axis = "scale")

#rm(autoboost.res.x2.adapt)




