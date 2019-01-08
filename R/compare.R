

compare<-function(DATA,result.boost,level=1){

	vp<-sum(result.boost[which(DATA$support==1)]>=level)
	totp<-sum(DATA$support==1)
	sens<-vp/totp

	spe<-0
	if(sum(result.boost>=level)!=0)	spe<-vp/sum(result.boost>=level)
	Fscore<-0
	Fscore2<-0
	Fscore<-0
	Fscore12<-0
	if(sens !=0){
		Fscore<-2*spe*sens/(spe+sens)
		Fscore12<-(1+0.5^2)*spe*sens/(spe/4+sens)
		Fscore2<-(1+2^2)*spe*sens/(spe*4+sens)
	}
	 zeroz<-1
	 if(sum(result.boost)==0){zeroz<-0}
	return(c(sens,spe,Fscore,Fscore12,Fscore2,zeroz))
}
