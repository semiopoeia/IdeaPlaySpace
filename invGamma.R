library(invgamma)
m<-0.1
(1/m)+1
shape
s<-rinvgamma(10000, shape=13, scale=4)
summary(s)
var(s)
hist(s)

M<-c(5.91,0.016,0.475,0.218,0.277,2.124,1.81,-0.652,-0.683,3.282,3.394,-1.072,0.319)
V<-c(3.617604,0.150544,0.050625,0.026569,0.020449,0.326041,0.015376,0.011025,0.012769,0.019881,0.026896,0.537289,2.480625)
wl<-noquote(paste("wl",1:13,"~N(",M,",",V,");",sep=""))

mv<-c(169.203,39.778,3.169,1.443,1.458,15.908,3.041,0.75,0.778,2.096,4.107,152.178,786.434)
b<-round(.1*mv,3)
we<-noquote(paste("we",1:13,"~IG(2.1,",b,")",sep=""))
(1/169.203)+1
1/(1.006-1)

x<-rnorm(1000,50,10)
y<-log(x+x)+log(x^2)
plot(x,y)

solve<-function(alpha,beta){
mean<-beta/(alpha-1)
median<-(alpha*beta)/((alpha^2)-1)
mode<-beta/(alpha+1)
variance<-beta^2/((alpha-1)^2*(alpha-2))
skew<-(4*sqrt(alpha-2))/(alpha-3)
kurtosis<-(6*(5*alpha-11))/((alpha-3)*(alpha-4))
moments<-c("mean","median","mode","variance","skew","excess kurtosis")
values<-c(mean,median,mode,variance,skew,kurtosis)
return(cbind(moments,values))
}

pascal<-function(varcount){
factorial(varcount)/(2*factorial(varcount-2))
}

solve(2,4.6)
pascal(6)

betamode<-function(alpha,beta){
return((alpha-1)/(alpha+beta-2))
}

logit2prob<-function(x){
return(exp(x)/(1+exp(x)))
}