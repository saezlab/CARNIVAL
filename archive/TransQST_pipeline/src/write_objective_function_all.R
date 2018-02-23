write_objective_function_all <- function(dataMatrix = dataMatrix, variables = variables,alpha=alpha, beta=beta,scores=scores,nodeWeights=nodeWeights ) {
  
  OF <- "Obj:\t"
  
  for (i in 1:nrow(dataMatrix$dataMatrix)) {
    
    dM <- dataMatrix
    dM$dataMatrix <- as.matrix(t(dataMatrix$dataMatrix[i, ]))
    dM$dataMatrixSign <- as.matrix(t(dataMatrix$dataMatrixSign[i, ]))
    
    var <- variables[[i]]
    
    if (i==1) {
      OF <- paste0(OF, write_objective_function(dataMatrix = dM, variables = var,alpha = alpha, beta = beta,scores=scores,nodeWeights=nodeWeights))
    } else {
      OF <- paste0(OF, " + ",  write_objective_function(dataMatrix = dM, variables = var, alpha = alpha, beta = beta,scores=scores,weights=weights))
    }
    
  }
  
  return(OF)
  
}