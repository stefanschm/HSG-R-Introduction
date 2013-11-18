ES <- function (quantfunc,params) {
  # This function returns the ES given a level alpha and a quantile
  # function as an argument
  #
  # Args: 
  # params: confidence level  and df for  ES
  # quantfunc: A quantile function which will be used for the ES 
  #                calculations
  #
  # Returns:
  #   Expected Shortfall
  
  
  if (identical(quantfunc,qnorm)){
    qnormint <-  function(alpha){
      integrate(function(a){quantfunc(a,params$mean,params$std)},0,1-alpha)$value*(1/(1-alpha))
    }
    nes <-   -as.numeric(lapply(params$alpha,qnormint))
  }    else{  
    qtint <-  function(alpha){
      integrate(function(a){quantfunc(a,params$df)},0,1-alpha)$value*(1/(1-alpha))
    }
    nes <-    -as.numeric(lapply(params$alpha,qtint))
  }
  
  return(nes)
}