write_objective_function_all <- function(dataMatrix = dataMatrix, variables = variables,alphaWeight=alphaWeight, betaWeight=betaWeight,scores=scores,nodeWeights=nodeWeights, measuremetntsWeights=measuremetntsWeights ) {
  
  OF <- "Obj:\t"
  
  if(!is.null(measuremetntsWeights)){
    
    if(nrow(measuremetntsWeights)!=nrow(dataMatrix$dataMatrix)){
      
      stop("Number of rows of the Measurements-Weights table should be the same as the number of conditions considered..")
      
    }
    
  }
  
  for (i in 1:nrow(dataMatrix$dataMatrix)) {
    
    dM <- dataMatrix
    dM$dataMatrix <- as.matrix(t(dataMatrix$dataMatrix[i, ]))
    dM$dataMatrixSign <- as.matrix(t(dataMatrix$dataMatrixSign[i, ]))
    
    if(!is.null(measuremetntsWeights)){
      
      mm <- as.matrix(measuremetntsWeights[i, ])
      
    } else {
      
      mm <- NULL
      
    }
    
    var <- variables[[i]]
    
    if (i==1) {
      OF <- paste0(OF, write_objective_function(dataMatrix = dM, variables = var,alphaWeight = alphaWeight, betaWeight = betaWeight,scores=scores,nodeWeights=nodeWeights,measuremetntsWeights = mm))
    } else {
      OF <- paste0(OF, " + ",  write_objective_function(dataMatrix = dM, variables = var, alphaWeight = alphaWeight, betaWeight = betaWeight,scores=scores,nodeWeights=nodeWeights,measuremetntsWeights = mm))
    }
    
  }
  
  return(OF)
  
}
