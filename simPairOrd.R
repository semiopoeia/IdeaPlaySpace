#sample size for respondents
n<-100
#number of pairs
np<-(n*(n-1))/2
#item sample size
i<-3
#number of item pairs
ip<-(i*(i-1))/2

#total item pairing across all respondent pairings
total_in<-ip*np
total_in

#simulate concordance/discordant item-respondent pairs
cordlev<-(-1:+1)

#i1
#give proportions for discordant, indeterminant (tied),concordant pairs respectively
pr1<-c(0,1,0)
npip1<-sample(cordlev,size=np,replace=T,prob=pr1)
#compute coefficients
gammanpip1<-sum(npip1)/np
gammanpip1
semipartgammanpip1<-sum(npip1)/total_in
semipartgammanpip1

#i2
pr2<-c(1,0,0)
npip2<-sample(cordlev,size=np,replace=T,prob=pr2)

#compute coefficients
gammanpip2<-sum(npip2)/np
gammanpip2
semipartgammanpip2<-sum(npip2)/total_in
semipartgammanpip2


#i3
pr3<-c(0,0,1)
npip3<-sample(cordlev,size=np,replace=T,prob=pr3)
#compute coefficients
gammanpip3<-sum(npip3)/np
gammanpip3
semipartgammanpip3<-sum(npip3)/total_in
semipartgammanpip3

multigamma<-sum(c(abs(sum(npip1)),abs(sum(npip2)),abs(sum(npip3))))/total_in
multigamma

