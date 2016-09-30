setwd("C:/Users/Saskia/Documents/Physik/FP/fp1-squid/Auswertung")
library("Hmisc")
library("plotrix")
source("Sinusfit.R")
data = read.csv("Kronkorken_36.5cm_HM1508-2.csv")
x <- data [[1]]
y <- data[[2]]
xmin = min(x)
xmax = max(x)
input_data = data.frame(x=x, y=y)
plot(x, y, xlab = "Time/s", ylab = "Signalspannung/V")
sinusfit(input_data)

parameter=sinusfit(input_data)
a = parameter[[1]]
b = parameter[[2]]
c = parameter[[3]]
d = parameter[[4]]


plot(function(x){a+b*sin(c*x+d)}, xmin, xmax, add = TRUE, col = 'red')



X=((c*x)%%(2*pi))/(2*pi)*360
Y=abs(y-a)

polar.plot(NA,NA,radial.lim=c(0,2),rp.type="s",cex=1,show.grid.labels=3)
polar.plot(Y,X,rp.type="s",cex=0.6,point.symbols=4,show.grid=FALSE,show.radial.grid=FALSE,add=TRUE,point.col="blue")

