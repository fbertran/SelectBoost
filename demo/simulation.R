# First example
library(SelectBoost)

set.seed(3141)
for(i in 1:200){
P<-10
N<-10
group<-c(1:9,1)
cor_group<-rep(0.95,9)
supp<-c(1,1,1,0,0,0,0,0,0,0)
minB<-1
maxB<-2
stn<-5
C<-simulation_cor(P,1,group,cor_group)
X<-simulation_X(10,C)
assign(paste("DATA_exemple1_nb_",i,sep=""),simulation_DATA(X,supp,1,2,stn))
}
cor(X)
G<-abs(cor(X))
hist(G[lower.tri(G)])

get(paste("DATA_exemple1_nb_",i,sep=""))


#Second example

set.seed(3141)
for(i in 1:200){
P<-50
N<-20
group<-rep(1,50)
cor_group<-rep(0.5,1)
supp<-c(1,1,1,1,1,rep(0,45))
minB<-1
maxB<-2
stn<-5
C<-simulation_cor(P,1,group,cor_group)
X<-simulation_X(N,C)
assign(paste("DATA_exemple2_nb_",i,sep=""),simulation_DATA(X,supp,1,2,stn))
}
head(cor(X))
G<-abs(cor(X))
hist(G[lower.tri(G)])


#Third example

require(Cascade)
data(micro_S)
data(micro_US)
micro_US<-as.micro_array(micro_US,c(60,90,240,390),6)
micro_S<-as.micro_array(micro_S,c(60,90,240,390),6)
S<-geneSelection(list(micro_S,micro_US),list("condition",c(1,2),1),-1)
data(micro_S)
Sel<-micro_S[S@name,]

set.seed(3141)
for(i in 1:200){
X<-t(as.matrix(Sel[sample(1:1300 ,100),]))
X2<-t(t(X)/sqrt(diag(t(X)%*%X)))
#SSS<-cor(X2)
#SS<-abs(SSS[row(SSS)>col(SSS)])
#mean(SS)

X<-X2
supp<-c(1,1,1,1,1,rep(0,95))
minB<-1
maxB<-2
stn<-5
assign(paste("DATA_exemple3_nb_",i,sep=""),simulation_DATA(X,supp,1,2,stn))
}


#Fourth example

require(Cascade)
data(micro_S)
data(micro_US)
micro_US<-as.micro_array(micro_US,c(60,90,240,390),6)
micro_S<-as.micro_array(micro_S,c(60,90,240,390),6)
S<-geneSelection(list(micro_S,micro_US),list("condition",c(1,2),1),101)
data(micro_S)
Sel<-micro_S[S@name,]
suppt<-rep(1:4,6)

set.seed(3141)
for(i in 1:101){
  X<-t(as.matrix(Sel[-i,suppt!=4]))
  X2<-t(t(X)/sqrt(diag(t(X)%*%X)))
  #SSS<-cor(X2)
  #SS<-abs(SSS[row(SSS)>col(SSS)])
  #mean(SS)

  X<-X2
  supp<-c(1,1,1,1,1,rep(0,95))
  minB<-1
  maxB<-2
  stn<-5
  DATA<-simulation_DATA(X,supp,1,2,stn)
  DATA$Y<-as.vector(t(Sel[i,suppt!=1]))
  assign(paste("DATA_exemple4_nb_",i,sep=""),DATA)
  rm(DATA)
}


#Fifth example

set.seed(3141)
for(i in 1:200){
  P<-500
  N<-25
  group<-rep(1,500)
  cor_group<-rep(0.5,1)
  supp<-c(1,1,1,1,1,rep(0,495))
  minB<-1
  maxB<-2
  stn<-5
  C<-simulation_cor(P,1,group,cor_group)
  X<-simulation_X(N,C)
  assign(paste("DATA_exemple5_nb_",i,sep=""),simulation_DATA(X,supp,1,2,stn))
}
head(cor(X))
G<-abs(cor(X))
hist(G[lower.tri(G)])
