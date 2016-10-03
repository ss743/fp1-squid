setwd("C:/Users/Saskia/Documents/Physik/FP/fp1-squid/Auswertung")
library("Hmisc")
library("plotrix")
source("hochdreifit.R")
source("Einsdurch.R")

F = 9.3 * 10^-9
Si = 1.9
mu_0 = 4*pi*10^-7
U = 2.68
A = pi*0.002^2

# Leiterschleife
#Magnetfeld
a_LS <- c(-0.3485069, -0.1772445, -0.05893693, -0.0352345, 0.02223518)
s_a_LS <- c(0.0004740048, 0.0002890753, 0.0002707528, 0.0004135886, 0.0003707150)
B_LS = abs(F*a_LS/(2*Si))
s_B_LS = (s_a_LS/a_LS)*B_LS
R <- c(51.47, 100.8, 300.8, 510.6, 1000)


plot(R, B_LS, pch=4, ylim = c(0, 14*10^-10), , xlab="R/Ohm", ylab = "B_LS/T")
plotCI (R, B_LS, uiw=abs(s_B_LS) , err="y" , pch=4, cex=0.6 ,add=TRUE)



#Dipol

z_LS = sqrt(4.7^2 + 0.1^2)*10^-2
s_z_LS = 0.2*10^-2
p_LS = (2*pi*B_LS*z_LS^3)/mu_0

s_p_LS = p_LS * sqrt((s_B_LS/B_LS)^2 + 3*(s_z_LS/z_LS)^2)

input_data2=data.frame(x=R, y=B_LS)


#einsdurch(input_data2)

parameter2=einsdurch(input_data2)
a = parameter2[[1]]
b = parameter2[[2]]


plot(function(x){a+b/x},0,1000, add = TRUE, col = 'black')

#Theorie

p_theo = A*U/R
s_p_theo = 2*pi*0.002*0.0005/R
B_theo = (mu_0/(2*pi))*(p_theo/z_LS^3)
s_B_theo = B_theo * sqrt((s_p_theo/p_theo)^2 + 3*(s_z_LS/z_LS)^2)
points(R, B_theo, pch=4, col="red")
plotCI (R, B_theo, uiw=abs(s_B_theo) , err="y" , pch=4, cex=0.6 ,add=TRUE)

#einsdurch(input_data3)
input_data3=data.frame(x=R, y=B_theo)

parameter3=einsdurch(input_data3)
a = parameter3[[1]]
b = parameter3[[2]]






plot(function(x){a+b/x},0,1000, add = TRUE, col = 'red')
#random Shit - gleiche Reihenfolge wie.txt
#Magnetfeld

a_RS <- c(1.5468670, 0.1211425, 0.4497760, 1.0042261, -0.3500509)
s_a_RS <- c(0.0079674875, 0.0011208778, 0.0006547072, 0.0024883093, 0.001887523)
B_RS = abs(F*a_RS/(2*Si))
s_B_RS = (s_a_RS/a_RS)*B_RS

#Dipol
z_RS1 = sqrt(4.7^2 + 6.5^2)*10^-2
z_RS2 = sqrt(4.7^2 + 4.5^2)*10^-2
z_RS <- c (z_RS1, z_RS1, z_RS1, z_RS2, z_RS1)
s_z_RS = 0.2*10^-2

p_RS = (2*pi*B_RS*z_RS^3)/mu_0
s_p_RS = p_RS * sqrt((s_B_RS/B_RS)^2 + 3*(s_z_RS/z_RS)^2)

#Kronkorken

#Magnetfeld
a_KK <- c(10.6775694, -8.9101144, 3.6145876, 2.4405903, -1.4831742, -0.6929581)
s_a_KK <- c(0.0597263308, 0.0246627915, 0.0125814955, 0.0133117778, 0.0071175290, 0.005957833)
B_KK = abs(F*a_KK/(2*Si))
s_B_KK = (s_a_KK/a_KK)*B_KK

x <- c(33.5, 34.5, 35.0, 35.5, 36.0, 36.5)
s_x <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2)





#Dipol

z_KK = sqrt(4.7^2 + (x-28.5)^2)*10^-2
s_z_KK = 0.2*10^-2

p_KK = (2*pi*B_KK*z_KK^3)/mu_0
s_p_KK = p_KK * sqrt((s_B_KK/B_KK)^2 + 3*(s_z_KK/z_KK)^2)

plot(z_KK, B_KK, pch=4 ,xlab="z/cm", ylab="B_KK/T")
plotCI (z_KK, B_KK, uiw=abs(s_B_KK) , err="y" , pch=4, cex=0.6 ,add=TRUE)
plotCI (z_KK, B_KK, uiw=abs(s_z_KK) , err="x" , pch=4, cex=0.6 ,add=TRUE)

input_data1 = data.frame(x=z_KK, y=B_KK)
hochdreifit(input_data1)

parameter1=hochdreifit(input_data1)
a = parameter1[[1]]
b = parameter1[[2]]


plot(function(x){a+b/x^3},0.06,0.095, add = TRUE, col = 'red')
text(x= 0.085 ,y= 2*10^-8 ,  "B(z) = -(1.7+-0.5)*10^-8 + (1.5+-0.2)*10^-11/z^3", cex=0.7)