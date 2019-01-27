#Extending results from the Cascade package: reverse engineering with selectboost
#Code to reproduce the datasets saved with the package

#Reference for the Cascade modelling
# Vallat, L., Kemper, C. a., Jung, N., Maumy-Bertrand, M., Bertrand, F.,
# Meyer, N., Pocheville, A., Fisher, J. W., Gribben, J. G. et Bahram, S.
# (2013). Reverse-engineering the genetic circuitry of a cancer cell with predicted
# intervention in chronic lymphocytic leukemia. Proceedings of the National
# Academy of Sciences of the United States of America, 110(2), 459-64.

#Reference for the Cascade package
# Jung, N., Bertrand, F., Bahram, S., Vallat, L. et Maumy-Bertrand, M. (2014).
# Cascade : A R package to study, predict and simulate the diffusion of a signal
# through a temporal gene network. Bioinformatics. ISSN 13674803.

library(Cascade)
sav.graph=TRUE

#We change the array with the F matrices
T<-4
F<-array(0,c(T-1,T-1,T*(T-1)/2))

for(i in 1:(T*(T-1)/2)){diag(F[,,i])<-1}
F[,,2]<-F[,,2]*0.2
F[2,1,2]<-1
F[3,2,2]<-1
F[,,4]<-F[,,2]*0.3
F[3,1,4]<-1
F[,,5]<-F[,,2]

#We set the seed to make the results reproducible
set.seed(1)
Net<-Cascade::network_random(
  nb=100,
  time_label=rep(1:4,each=25),
  exp=1,
  init=1,
  regul=round(rexp(100,1))+1,
  min_expr=0.1,
  max_expr=2,
  casc.level=0.4
)
Net@F<-F

#We simulate gene expression according to the network Net
M <- Cascade::gene_expr_simulation(
  network=Net,
  time_label=rep(1:4,each=25),
  subject=5,
  level_pic=200)

#We infer the new network
Net_inf_C <- Cascade::inference(M,cv.subjects=TRUE)

Fab_inf_C <- Net_inf_C@F

set.seed(1)
#by default the crossvalidation is made subjectwise according to a leave one out scheme
net_confidence <- selectboost(M, Fab_inf_C)
#use cv.subject=FALSE to use default crossvalidation
net_confidence_cv <- selectboost(M, Fab_inf_C, cv.subject=FALSE)
net_confidence_.5 <- selectboost(M, Fab_inf_C, c0value = .5)
net_confidence_thr <- selectboost(M, Fab_inf_C, group = group_func_1)
plot(net_confidence)
plot(net_confidence_cv)
plot(net_confidence_.5)
plot(net_confidence_thr)


