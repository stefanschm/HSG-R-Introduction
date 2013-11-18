TransformResults <- function(VarES,histVaRES,params){
  # This function transforms the results to a matrix
  #
  # Args: 
  # VarES: Analytical VaR values
  # histVaRES: Historical VaR values
  # params: Parameters used for calculation
  #
  # Returns:
  #  A table with all the results 
  
  # tranform the results into a matrix
  results <- matrix(c(unlist(VarES),unlist(histVarES)),ncol=8,byrow=F)
  
  # name columns
  colnames(results) <- c("VaR Normal","VaR t","ES Normal","ES t","VaR Dax",
                         "VaR SP500","ES DAX","ES SP500")
  # name rows, alpha
  rownames(results) <- params$alpha
  
  # create a table
  results <- as.table(results)
  
  # round output
  results <- round(results,2)  
  
  return(results)
}


PlotResults <- function (results,quant) {
  # This function plots the results
  #
  # Args: 
  # results: results to be plotted
  # quant: line of the results matrix, the quantile
  #
  # Returns:
  #  A Plot 
  
  # create a barplot for a given quantile
  barplot(results[quant,])
  title(c("Results displayed for the ",quant," quantile"))
}

PrintResults <- function (results,quant) {
  # This function prints the results
  #
  # Args: 
  # results: results to be printed
  #
  # Returns:
  #  An output of the results 
  
  # create text output
  name <- colnames(results)
  
  for (i in 1:nrow(results)) {
    cat("The", name[i], "for the quantile", quant, "is: \n", results[quant, i], "\n")
  } 
  
}

TabulateResults <- function (results) {
  # This function tabulates the results
  #
  # Args: 
  # results: results to be tabulated
  #
  # Returns:
  #  A table of the results 
  
  # show the table
  results
}