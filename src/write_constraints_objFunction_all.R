write_constraints_objFunction_all <- function(variables=variables, dataMatrix=dataMatrix) {
  
  constraints0 <- c()
  
  for (i in 1:nrow(dataMatrix$dataMatrix)) {
    
    dM <- dataMatrix
    dM$dataMatrix <- as.matrix(t(dataMatrix$dataMatrix[i, ]))
    dM$dataMatrixSign <- as.matrix(t(dataMatrix$dataMatrixSign[i, ]))
    
    var <- variables[[i]]
    
    constraints0 <- c(constraints0, write_constraints_objFunction(variables = var, dataMatrix = dM, conditionIDX = i))
    
  }
  
  return(constraints0)
  
}
