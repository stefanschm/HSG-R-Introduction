ReturnVARandES <- function (quantilefunct,params) {
  # This function returns the VaR and the ES given a level alpha and a quantile
  # function as an argument
  #
  # Args: 
  # params: confidence level for VaR, conf and df ES
  # quantilefunct: A quantile function which will be used for the VaR and ES 
  #                calculations
  #
  # Returns:
  #   Value At Risk and Expected Shortfall 
  
  # Source the calculation functions
  source("var.R")
  source("es.R")
  
  # calculate the values
  
  nVaR <- lapply(quantilefunct,VaR,params)
  
  nES <- lapply(quantilefunct,ES,params)
  
    
  # return the values
  return(list(VaR=nVaR,ES= nES)) 
}