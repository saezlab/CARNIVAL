## This code writes all the variables.
##
## Enio Gjerga, 2020

write_generals <- function(variables = variables, objectiveFunction = objectiveFunction){

  generals <- c(paste0("\t", 
                       variables[[1]]$variables[variables[[1]]$idxNodes]),
                paste0("\t", 
                       variables[[1]]$variables[variables[[1]]$idxB]),
                paste0("\t",
                       unique(strsplit(objectiveFunction, split = " ")[[1]][grep(
                         pattern = "absDiff", 
                         x = strsplit(oF, split = " ")[[1]])])))
  
  return(generals)
  
}