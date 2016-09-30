setwd("C:/Users/Saskia/Documents/Physik/FP/fp1-squid/Auswertung")

source("Sinusfit.R")
data = read.csv("KK_36.0_HM1508-2.csv")
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
polar.plot(x, y)
