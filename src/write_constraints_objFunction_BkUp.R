write_constraints_objFunction <- function(variables=variables, dataMatrix=dataMatrix, conditionIDX=conditionIDX){
  
  measurements <- as.vector(t(dataMatrix$dataMatrixSign))
  
  idx2 <- which(measurements==1)
  idx3 <- which(measurements==-1)
  
  cc1 <- rep("", length(measurements))
  cc2 <- rep("", length(measurements))
  
  cc1[idx2] <- paste0(variables$variables[idx2], " - absDiff", idx2, "_", conditionIDX, " <= 1")
  cc2[idx2] <- paste0(variables$variables[idx2], " + absDiff", idx2, "_", conditionIDX, " >= 1")
  
  cc1[idx3] <- paste0(variables$variables[idx3], " - absDiff", idx3, "_", conditionIDX, " <= -1")
  cc2[idx3] <- paste0(variables$variables[idx3], " + absDiff", idx3, "_", conditionIDX, " >= -1")
  
  constraints0 <- c(cc1, cc2)
  
  return(constraints0[-which(constraints0=="")])
  
}