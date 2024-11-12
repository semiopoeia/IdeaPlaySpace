ats40<-read.table("ATS40.csv",header=T, sep=",")
taumat<-cor(na.omit(ats40[,-1]),method="kendall")

install.packages("qgraph", dep=T)
library("qgraph")
#general network graph
qgraph(taumat,minimum=0.5)

#do Exploratory Graph Analysis (EGA) with TMFG to find "communities"
ifelse("EGAnet" %in% row.names(installed.packages())==TRUE,
       ifelse(packageVersion("EGAnet")=="2.0.6", 
       "The EGAnet package is installed, jump to step #3", 
       "The EGAnet package is installed, but you have a difference version. Please, install the EGAnet package from CRAN (see Step #2 below)."),
       "Please, install the EGAnet package (see Step #2 below)")
#install.packages("EGAnet",dep=T)
library(EGAnet)

net.tmfg<-EGA(
taumat,n=length(ats40[,1]),
model = "TMFG", algorithm = "walktrap",uni.method = "LE")
#information about solution (5 communities)
sum.net.tmfg<-summary(net.tmfg)
#plotting the network
plot.net.tmfg<- plot(net.tmfg,  
			vsize = 10, #Size of the nodes
                  label.size = 4, #Size of the labels
                  alpha = 0.3, #The level of transparency of the nodes, which might be a single value or a vector of values
                  edge.alpha = 0.1, #The level of transparency of the edges, which might be a single value or a vector of values
                  legend.names = c("C1", "C2", "C3","C4","C5"), # A vector with names for each dimension
                  color.palette = "rainbow")
#title plot
require(ggpubr)
plot.net.tmfg.titled <- annotate_figure(plot.net.tmfg, 
top =  text_grob("EGA TMFG \n TEFI = -25.979", color = "black", size = 10))  
plot.net.tmfg.titled



#figure total entropy fit index with TMFG and walktrap
tefi.net.tmfg<-tefi(net.tmfg$correlation,structure=net.tmfg$wc)
tefi.net.tmfg$VN.Entropy.Fit
