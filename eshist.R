ESHist <- function (inputdata,params) {
  # This function returns the historical ES given a level alpha and inputdata
  #
  # Args: 
  # params: confidence level and df for ES
  # inputdata: Historical data to calculate the ES with
  #
  # Returns:
  #  Historical Expected Shortfall
  # length of inputdata
  N <- nrow(inputdata)
  
  # sort the input data according to the return
  sorteddata <- inputdata[order(inputdata[, 2]), 2]
  
  # calculate the historical VaR as an intermediate step
  varhist <- -sorteddata[ceiling(N * (1 - params$alpha))]
  
  # calculate ES, discontinuous loss function (see McNeil/Frey/Embrecht QRM, p.45)
  eshist <- rep(NA, length(varhist))
  for (i in 1:length(varhist)) {
    eshist[i] <- 1/(1 - params$alpha[i]) * (1/N * sum(-subset(sorteddata, sorteddata < -varhist[i]))) - varhist[i] * 
      (length(subset(sorteddata, sorteddata < -varhist[i])) * 1/N - (1 - params$alpha[i]))
  }
  
  # for comparison, standard textbook approach to calculate ES
  esstandard <- rep(NA, length(varhist))
  for (i in 1:length(varhist)) {
    esstandard[i] <- -mean(sorteddata[1:ceiling(N * (1 - params$alpha[i]))])
  }
  
  return(eshist) 

}