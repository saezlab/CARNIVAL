## This function returns the list of constraints associated with the 'Absolute 
## Difference' variables and which measure the mis-fit between inferred and 
## measured data.
## 
## Enio Gjerga, 2020


## ============================================ ##
## === Load write_constraints_objFunction.R === ##
## ============================================ ##

writeConstraintsObjectiveFunction <- function(variables = variables, 
                                              dataVector = dataVector){
  
  measurements <- dataVector$dataVectorSign
  
  idx2 <- which(measurements == 1)
  idx3 <- which(measurements == -1)
  
  cc1 <- rep("", length(measurements))
  cc2 <- rep("", length(measurements))
  
  cc1[idx2] <- paste0(variables$variables[idx2], " - absDiff", 
                      idx2, " <= 1")
  cc2[idx2] <- paste0(variables$variables[idx2], " + absDiff", idx2, " >= 1")
  
  cc1[idx3] <- paste0(variables$variables[idx3], " - absDiff", idx3, " <= -1")
  cc2[idx3] <- paste0(variables$variables[idx3], " + absDiff", idx3, " >= -1")
  
  constraints0 <- c(cc1, cc2)
  
  return(constraints0[-which(constraints0=="")])
  
} 