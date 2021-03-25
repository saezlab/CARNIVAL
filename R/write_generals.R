## This code writes all the variables.
##
## Enio Gjerga, 2020

createGenerals <- function(variables = variables, objectiveFunction = objectiveFunction){

  generals <- c(paste0("\t", 
                       variables$variables[variables$idxNodes]),
                paste0("\t", 
                       variables$variables[variables$idxB]),
                paste0("\t",
                       unique(strsplit(objectiveFunction, split = " ")[[1]][grep(
                         pattern = "absDiff", 
                         x = strsplit(objectiveFunction, split = " ")[[1]])])))
  
  return(generals)
  
}