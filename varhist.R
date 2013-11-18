VaRHist <- function (inputdata,params) {
  # This function returns the historical VaR given a level alpha and inputdata
  #
  # Args: 
  # alpha: confidence level for VaR
  # inputdata: Historical data to calculate the VaR with
  #
  # Returns:
  #  Historical Value At Risk 
  
  # length of inputdata
  N <- nrow(inputdata)
  
  # sort the input data according to the return
  sorteddata <- inputdata[order(inputdata[, 2]), 2]
  
  # calculate the historical VaR
  varhist <- -sorteddata[ceiling(N * (1 - params$alpha))]
  
  return(varhist) 
  
}