sinusfit <- function(data){
  
  sinus <- y ~ a+b*sin(c*x+d)
  x <- data [[1]]
  y <- data[[2]]
  a0=(max(y)+ min(y))/2
  b0=(max(y)-min(y))/2
  c0= (2*pi)/7
  d0=5
  
  
  #plot(function(x){a0+b0*sin(c0*x+d0)},0,20,col="green",add=TRUE)
  
  fit = nls(sinus, data, start=list(a=a0, b=b0, c=c0, d=d0))
 
  return(summary(fit)$parameters)
}