poispdf<-function(mean,k){
(mean^k)*((exp(-mean))/(factorial(k)))
}
poispdf(.17,4)

binpdf<-function(p,k,n){
binc<-factorial(n)/(factorial(k)*factorial(n-k))
pp<-p^k*(1-p)^(n-k)
binc*pp
}

binpdf(.17,4,10)