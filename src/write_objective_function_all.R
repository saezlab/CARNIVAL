write_objective_function_all <- function(dataMatrix = dataMatrix, variables = variables,scores=scores,weights=weights, alpha=alpha, beta=beta) {
  
  OF <- "Obj:\t"
  
  for (i in 1:nrow(dataMatrix$dataMatrix)) {
    
    dM <- dataMatrix
    dM$dataMatrix <- as.matrix(t(dataMatrix$dataMatrix[i, ]))
    dM$dataMatrixSign <- as.matrix(t(dataMatrix$dataMatrixSign[i, ]))
    
    var <- variables[[i]]
    
    if (i==1) {
      OF <- paste0(OF, write_objective_function(dataMatrix = dM, variables = var,scores=scores,weights=weights, alpha = alpha, beta = beta))
    } else {
      OF <- paste0(OF, " + ",  write_objective_function(dataMatrix = dM, variables = var, scores=scores,weights=weights,alpha = alpha, beta = beta))
    }
    
  }
  
  return(OF)
  
}