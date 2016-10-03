hochdreifit <- function(data){
  
  hochdrei <- y ~ a+b/(1*x^3)
  x <- data [[1]]
  y <- data[[2]]
  a0 = 0
  b0 = 1*10^-9

  
  
  plot(function(x){a0+b0/x^3},0,20,col="green",add=TRUE)
  
  fit = nls(hochdrei, data, start=list(a=a0, b=b0))
  
  return(summary(fit)$parameters)
}