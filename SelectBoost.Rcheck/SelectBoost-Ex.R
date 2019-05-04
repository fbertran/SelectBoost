pkgname <- "SelectBoost"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "SelectBoost-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('SelectBoost')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("AICc_BIC_glmnetB")
### * AICc_BIC_glmnetB

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: AICc_BIC_glmnetB
### Title: AICc and BIC for glmnet logistic models
### Aliases: AICc_BIC_glmnetB rerr ridge_logistic BIC_glmnetB AICc_glmnetB

### ** Examples

set.seed(314)
xran=matrix(rnorm(150),30,5)
ybin=sample(0:1,30,replace=TRUE)
glmnet.fit <- glmnet.fit <- glmnet::glmnet(xran,ybin,family="binomial",standardize=FALSE)
set.seed(314)
rerr(1:10,10:1)

set.seed(314)
ridge_logistic(xran,ybin,lambda=.5,beta0=rnorm(5),beta=rnorm(5,1))

set.seed(314)
if(is.factor(ybin)){ynum=unclass(ybin)-1} else {ynum=ybin}
subSample <- 1:min(ncol(xran),100)
BIC_glmnetB(xran,ynum,glmnet.fit,alpha=1,subSample, reducer='median')

set.seed(314)
if(is.factor(ybin)){ynum=unclass(ybin)-1} else {ynum=ybin}
subSample <- 1:min(ncol(xran),100)
AICc_glmnetB(xran,ynum,glmnet.fit,alpha=1,subSample, reducer='median')




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("AICc_BIC_glmnetB", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("auto.analyze")
### * auto.analyze

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: auto.analyze
### Title: Find limits for selectboost analysis
### Aliases: auto.analyze auto.analyze.selectboost

### ** Examples

data(autoboost.res.x)
auto.analyze(autoboost.res.x)

data(autoboost.res.x2)
auto.analyze(autoboost.res.x2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("auto.analyze", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("autoboost")
### * autoboost

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: autoboost
### Title: Autoboost
### Aliases: autoboost

### ** Examples

set.seed(314)
xran=matrix(rnorm(75),15,5)
ybin=sample(0:1,15,replace=TRUE)
yran=rnorm(15)
set.seed(314)
#For quick test purposes, not meaningful, should be run with greater value of B
#and disabling parallel computing as well
res.autoboost <- autoboost(xran,yran,B=3,use.parallel=FALSE)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("autoboost", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("boost")
### * boost

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: boost
### Title: Boost step by step functions
### Aliases: boost boost.normalize boost.compcorrs boost.correlation_sign
###   boost.findgroups boost.Xpass boost.adjust boost.random boost.apply
###   boost.select

### ** Examples

set.seed(314)
xran=matrix(rnorm(200),20,10)
yran=rnorm(20)
xran_norm <- boost.normalize(xran)

xran_corr<- boost.compcorrs(xran_norm)

xran_corr_sign <- boost.correlation_sign(xran_corr)

xran_groups <- boost.findgroups(xran_corr, group=group_func_1, .3)
xran_groups_2 <- boost.findgroups(xran_corr, group=group_func_2, .3)

xran_Xpass <- boost.Xpass(nrow(xran_norm),ncol(xran_norm))

xran_adjust <- boost.adjust(xran_norm, xran_groups$groups, xran_corr_sign)

#Not meaningful, should be run with B>=100
xran_random <- boost.random(xran_norm, xran_Xpass, xran_adjust$vmf.params, B=5)

## Not run: 
##D xran_random <- boost.random(xran_norm, xran_Xpass, xran_adjust$vmf.params, B=100)
## End(Not run)

xran_apply <- boost.apply(xran_norm, xran_random, yran, lasso_msgps_AICc)

xran_select <- boost.select(xran_apply)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("boost", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fastboost")
### * fastboost

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fastboost
### Title: Fastboost
### Aliases: fastboost

### ** Examples

set.seed(314)
xran=matrix(rnorm(75),15,5)
ybin=sample(0:1,15,replace=TRUE)
yran=rnorm(15)
set.seed(314)
#For quick test purpose, not meaningful, should be run with greater value of B
#and disabling parallel computing as well
res.fastboost <- fastboost(xran,yran,B=3,use.parallel=FALSE)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fastboost", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("group_func_1")
### * group_func_1

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: group_func_1
### Title: Generate groups by thresholding.
### Aliases: group_func_1

### ** Examples

set.seed(314)
group_func_1(cor(matrix(rnorm(50),10,5)),.4)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("group_func_1", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("group_func_2")
### * group_func_2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: group_func_2
### Title: Generate groups using community analysis.
### Aliases: group_func_2

### ** Examples

set.seed(314)
group_func_2(cor(matrix(rnorm(100),10,10)),.5)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("group_func_2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("miscplot")
### * miscplot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: miscplot
### Title: Miscellaneous plot functions
### Aliases: miscplot plot.matrix

### ** Examples

set.seed(3141)
randmat=matrix(rnorm(360),60,60)
plot(randmat)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("miscplot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.selectboost")
### * plot.selectboost

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.selectboost
### Title: Plot selectboost object
### Aliases: plot.selectboost

### ** Examples

set.seed(314)
xran=matrix(rnorm(75),15,5)
ybin=sample(0:1,15,replace=TRUE)
yran=rnorm(15)
layout(matrix(1:4,2,2))

data(autoboost.res.x)
plot(autoboost.res.x)

data(autoboost.res.x2)
plot(autoboost.res.x2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.selectboost", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.summary.selectboost")
### * plot.summary.selectboost

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.summary.selectboost
### Title: Plot a summary of selectboost results
### Aliases: plot.summary.selectboost

### ** Examples

data(autoboost.res.x)
plot(summary(autoboost.res.x))

data(autoboost.res.x2)
plot(summary(autoboost.res.x2))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.summary.selectboost", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_selectboost_cascade")
### * plot_selectboost_cascade

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_selectboost_cascade
### Title: plot_Selectboost_cascade
### Aliases: plot_selectboost_cascade plot,network.confidence,ANY-method
###   plot,network.confidence,network.confidence-method

### ** Examples

data(net_confidences)
plot(net_confidence)
plot(net_confidence_.5)
plot(net_confidence_thr)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_selectboost_cascade", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("selectboost_cascade")
### * selectboost_cascade

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: selectboost_cascade
### Title: Selectboost_cascade
### Aliases: selectboost_cascade selectboost selectboost,micro_array-method
###   selectboost,micro_array,micro_array-method

### ** Examples

set.seed(314)
set.seed(314)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("selectboost_cascade", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simulation")
### * simulation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simulation
### Title: Miscellaneous simulation functions
### Aliases: simulation simulation_cor simulation_X simulation_DATA compsim
###   compsim.simuls

### ** Examples

N<-10
group<-c(rep(1:2,5))
cor_group<-c(.8,.4)
supp<-c(1,1,1,0,0,0,0,0,0,0)
minB<-1
maxB<-2
stn<-5
C<-simulation_cor(group,cor_group)

set.seed(314)
X<-simulation_X(10,C)
G<-abs(cor(X))
hist(G[lower.tri(G)])

set.seed(314)
DATA_exemple<-simulation_DATA(X,supp,1,2,stn)

set.seed(314)
result.boost = fastboost(DATA_exemple$X, DATA_exemple$Y, steps.seq = .7, c0lim = FALSE,
use.parallel = FALSE, B=10)
compsim(DATA_exemple, result.boost, level=.7)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simulation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.selectboost")
### * summary.selectboost

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.selectboost
### Title: Summarize a selectboost analysis
### Aliases: summary.selectboost

### ** Examples

data(autoboost.res.x)
summary(autoboost.res.x)
summary(autoboost.res.x, force.dec=FALSE)

data(autoboost.res.x.adapt)
summary(autoboost.res.x.adapt)

data(autoboost.res.x2)
summary(autoboost.res.x2)
summary(autoboost.res.x2, force.dec=FALSE)

data(autoboost.res.x2.adapt)
summary(autoboost.res.x2.adapt)

data(fastboost.res.x)
summary(fastboost.res.x)
summary(fastboost.res.x, force.dec=FALSE)

data(fastboost.res.x.adapt)
summary(fastboost.res.x.adapt)

data(fastboost.res.x2)
summary(fastboost.res.x2)
summary(fastboost.res.x2, force.dec=FALSE)

data(fastboost.res.x2.adapt)
summary(fastboost.res.x2.adapt)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.selectboost", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("trajC0")
### * trajC0

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: trajC0
### Title: Plot trajectories
### Aliases: trajC0 trajC0.selectboost

### ** Examples


data(autoboost.res.x)
data(diabetes, package="lars")

### With lasso trajectories
m.x<-lars::lars(diabetes$x,diabetes$y)
plot(m.x)
mm.x<-predict(m.x,type="coef",mode="lambda")
autoboost.res.x.mean = summary(autoboost.res.x)

par(mfrow=c(2,2),mar=c(4,4,1,1))
trajC0(autoboost.res.x,autoboost.res.x.mean,lasso.coef.path=mm.x,type.graph="lasso")
trajC0(autoboost.res.x,autoboost.res.x.mean)
trajC0(autoboost.res.x,autoboost.res.x.mean,type.graph="bars")
trajC0(autoboost.res.x,autoboost.res.x.mean,type.x.axis ="scale")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("trajC0", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("var_select")
### * var_select

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: var_select
### Title: Variable selection functions
### Aliases: var_select lasso_cv_glmnet_bin_min lasso_cv_glmnet_bin_1se
###   lasso_glmnet_bin_AICc lasso_glmnet_bin_BIC lasso_cv_lars_min
###   lasso_cv_lars_1se lasso_cv_glmnet_min lasso_cv_glmnet_min_weighted
###   lasso_cv_glmnet_1se lasso_cv_glmnet_1se_weighted lasso_msgps_Cp
###   lasso_msgps_AICc lasso_msgps_GCV lasso_msgps_BIC enetf_msgps_Cp
###   enetf_msgps_AICc enetf_msgps_GCV enetf_msgps_BIC lasso_cascade

### ** Examples

set.seed(314)
xran=matrix(rnorm(150),30,5)
ybin=sample(0:1,30,replace=TRUE)
yran=rnorm(30)
set.seed(314)
lasso_cv_glmnet_bin_min(xran,ybin)

set.seed(314)
lasso_cv_glmnet_bin_1se(xran,ybin)

set.seed(314)
lasso_glmnet_bin_AICc(xran,ybin)

set.seed(314)
lasso_glmnet_bin_BIC(xran,ybin)

set.seed(314)
lasso_cv_lars_min(xran,yran)

set.seed(314)
lasso_cv_lars_1se(xran,yran)

set.seed(314)
lasso_cv_glmnet_min(xran,yran)

set.seed(314)
lasso_cv_glmnet_min_weighted(xran,yran,c(1000,0,0,1,1))

set.seed(314)
lasso_cv_glmnet_1se(xran,yran)

set.seed(314)
lasso_cv_glmnet_1se_weighted(xran,yran,c(1000,0,0,1,1))

set.seed(314)
lasso_msgps_Cp(xran,yran)

set.seed(314)
lasso_msgps_AICc(xran,yran)

set.seed(314)
lasso_msgps_GCV(xran,yran)

set.seed(314)
lasso_msgps_BIC(xran,yran)

set.seed(314)
enetf_msgps_Cp(xran,yran)

set.seed(314)
enetf_msgps_AICc(xran,yran)

set.seed(314)
enetf_msgps_GCV(xran,yran)

set.seed(314)
enetf_msgps_BIC(xran,yran)

set.seed(314)
lasso_cascade(t(xran),yran,5,cv.fun=lars::cv.folds)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("var_select", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
