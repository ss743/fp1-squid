sinusfit <- function(data){
  
  sinus <- y ~ a+b*sin(c*x+d)
  x <- data [[1]]
  y <- data[[2]]
  a0=(max(y)+ min(y))/2
  b0=(max(y)-min(y))/2
  c0= 7/(2*pi)
  d0=5
  
  
  #plot(function(x){(besselJ(x*alpha0,m))^2},0,10,col="black",add=TRUE)
  
  fit = nls(sinus, data, start=list(a=a0, b=b0, c=c0, d=d0))
 
  return(summary(fit)$parameters)
}