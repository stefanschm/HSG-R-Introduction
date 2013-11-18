VaR <- function (quantilefunct,params) {
  # This function returns the VaR given a level alpha and a quantile
  # function as an argument
  #
  # Args: 
  # params: confidence level for VaR
  # quantilefunct: A quantile function which will be used for the VaR
  #                calculations
  #
  # Returns:
  #   Value At Risk 

  
  # check if qt or qnorm was the input argument
  if (identical(quantilefunct,qnorm)){
    nvar <-  quantilefunct(params$alpha)*params$std+params$mean
  }    else{
    nvar <-  quantilefunct(params$alpha,params$df)*params$std+params$mean
  }
  
  return(nvar) 
}