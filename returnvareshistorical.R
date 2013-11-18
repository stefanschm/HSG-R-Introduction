ReturnVARandESHistorical <- function (inputdata,params) {
  # This function returns the historical VaR and the ES given a level alpha 
  #
  # Args: 
  # params: confidence level for VaR and ES
  # inputdata: Historical data to calculate the VaR and ES with
  #
  # Returns:
  #  Historical Value At Risk and Expected Shortfall
  
  # Source the calculation functions
  source("varhist.R")
  source("eshist.R")
  
  # calculate the values
  nhistVaR <- lapply(inputdata[1:2],VaRHist,params)
  nhistES <- lapply(inputdata[1:2],ESHist,params)
  
  # return the values
  return(list(VaR=nhistVaR, ES=nhistES)) 
}