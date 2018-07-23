write_objective_function_all <- function(dataMatrix = dataMatrix, variables = variables,alphaWeight=alphaWeight, betaWeight=betaWeight,scores=scores,nodeWeights=nodeWeights, measurementsWeights=measurementsWeights ) {
  
  OF <- "Obj:\t"
  
  if(!is.null(measurementsWeights)){
    
    if(nrow(measurementsWeights)!=nrow(dataMatrix$dataMatrix)){
      
      stop("Number of rows of the Measurements-Weights table should be the same as the number of conditions considered..")
      
    }
    
  }
  
  for (i in 1:nrow(dataMatrix$dataMatrix)) {
    
    dM <- dataMatrix
    dM$dataMatrix <- as.matrix(t(dataMatrix$dataMatrix[i, ]))
    dM$dataMatrixSign <- as.matrix(t(dataMatrix$dataMatrixSign[i, ]))
    
    if(!is.null(measurementsWeights)){
      
      mm <- as.matrix(measurementsWeights[i, ])
      
    } else {
      
      mm <- NULL
      
    }
    # print(mm)
    
    var <- variables[[i]]
    if (i==1) {
      OF <- paste0(OF, write_objective_function(dataMatrix = dM, variables = var,alphaWeight = alphaWeight, betaWeight = betaWeight,scores=scores,nodeWeights=nodeWeights,measurementsWeights = mm,conditionIDX=i))
    } else {
      OF <- paste0(OF, " + ",  write_objective_function(dataMatrix = dM, variables = var, alphaWeight = alphaWeight, betaWeight = betaWeight,scores=scores,nodeWeights=nodeWeights,measurementsWeights = mm,conditionIDX=i))
    }
    
  }
  
  return(OF)
  
}
