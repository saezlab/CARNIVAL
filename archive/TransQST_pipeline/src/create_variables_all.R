create_variables_all <- function(pknList=pknList, dataMatrix=dataMatrix){
  
  res <- list()
  namesRes <- c()
  
  for(i in 1:nrow(dataMatrix$dataMatrix)){
    
    dM <- dataMatrix
    dM$dataMatrix <- as.matrix(t(dataMatrix$dataMatrix[i, ]))
    dM$dataMatrixSign <- as.matrix(t(dataMatrix$dataMatrixSign[i, ]))
    
    res[[length(res)+1]] <- create_variables(pknList = pknList, dataMatrix = dM, conditionIDX = i)
    
    namesRes <- c(namesRes, paste0("Condition_", i))
    
  }
  
  names(res) <- namesRes
  
  return(res)
  
}