gamma_ci<-function(coeff,se,c){
			xc<-qnorm(c+((1-0.95)/2))
			lb<-coeff-xc*se
			ub<-coeff+xc*se
			return(noquote(paste(c*100,"% interval is ", round(lb,4)," , ",round(ub,4))))
			}
gamma_ci(0.6631,0.099,0.90)