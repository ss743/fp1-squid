einsdurch <- function(data){
  
  einsdurch <- y ~ a+b/(1*x)
  x <- data [[1]]
  y <- data[[2]]
  a0 = 0#2*10^-12
  b0 = 6.9*10^-8
  
  
  
  plot(function(x){a0+b0/x},0,1000,col="green",add=TRUE)
  
  fit = nls(einsdurch, data, start=list(a=a0, b=b0))
  
  return(summary(fit)$parameters)
}