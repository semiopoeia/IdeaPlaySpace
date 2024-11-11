ats40<-read.table(
"C:/Users/PWS5/OneDrive - University of Pittsburgh/Desktop/SleepHUB/ATS40.csv",
header=T, sep=",")
taumat<-cor(na.omit(ats40[,-1]),method="kendall")

install.packages("qgraph", dep=T)
library("qgraph")

qgraph(taumat,minimum=0.5)
