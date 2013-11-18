# Final Group Assignment - Problemset 1 ----------------------------------------
# Stefan Schmaltz - 12603981
# Pascal Rakers - 00000000
# Version 0.1
# Date 14.11.2013
# This file calculates analytical VaR and ES and compares these with historical
# VaR and ES based on DAX and SP500 timeseries

# general
rm(list=ls())
setwd("G:/Dropbox/iPhone/HSG/Introduction to R/FinalAssignment/")

# source files
source("loadfinancialdata.R")
source("returnvares.R")
source("returnvareshistorical.R")
source("showresults.R")

# load the input data for the historical VaR and ES
findata  <- loadFinancialData()

# set parameters
quantilefunct <- c(qnorm,qt)  # first normal and then t-distributed
params <- list(alpha=c(0.9,0.95,0.975,0.99,0.995),df=4,mean=0, std=1)

# retrurn analytical VaR and ES
VarES <- ReturnVARandES(quantilefunct,params)

# returns historical VaR and ES
histVarES <- ReturnVARandESHistorical(findata,params)

# transform results to make them comparable
results <- TransformResults(VarES,histVarES,params)  # insert transformations here

# plot results
PlotResults(results,"0.9")

# print results
PrintResults(results,"0.9")

# tabulate results
TabulateResults(results)
