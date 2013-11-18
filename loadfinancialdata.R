loadFinancialData <- function () {
  # This function loads the DAX and SP500 data
  #
  # Args: none
  #
  # Returns:
  #   Dataframes with DAX and SP500 data
  
  # read in the files
  daxst <- read.csv("daxst.csv", header = T)
  sp500st <- read.csv("sp500st.csv", header = T)
  
  # calculate mean and standard deviations
  means <- list(dax=mean(daxst$DAX),sp500=mean(sp500st$SP500))
  std <-list(dax=sd(daxst$DAX),sp500=sd(sp500st$SP500))
  
  # create list to return, include means and SDs
  findata <- list(dax = daxst, sp500 = sp500st,means=means,std = std )
  
  return(findata) 
  
}