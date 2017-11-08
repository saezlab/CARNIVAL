write_objective_function <- function(dataMatrix = dataMatrix, variables = variables, alpha=alpha, beta=beta){
  
  measured <- gsub(colnames(dataMatrix$dataMatrix)[dataMatrix$dsID], pattern = "DS:", replacement = "")
  
  idxMeasured <- c()
  for(i in 1:length(measured)){
    
    idxMeasured <- c(idxMeasured, which(variables$expNodesReduced==paste0("Species ", measured[i])))
    
  }
  
  measuredVar <- variables$variables[idxMeasured]
  
  objectiveFunctionVec <- paste0(" + ", alpha, " absDiff", gsub(measuredVar, pattern = "xb", replacement = ""))
  objectiveFunction <- paste(objectiveFunctionVec, collapse = "")
  
  objectiveFunction <- substring(text = objectiveFunction[1], first = 4, last = nchar(objectiveFunction))
  
  objectiveFunction <- paste0("", objectiveFunction)
  
  objectiveFunctionUpVec <- paste0(" + ", beta, " ", variables$variables[variables$idxNodesUp])
  objectiveFunctionUp <- paste(objectiveFunctionUpVec, collapse = "")
  
  objectiveFunctionDownVec <- paste0(" + ", beta, " ", variables$variables[variables$idxNodesDown])
  objectiveFunctionDown <- paste(objectiveFunctionDownVec, collapse = "")
  
  objectiveFunction <- paste0(objectiveFunction, objectiveFunctionUp)
  objectiveFunction <- paste0(objectiveFunction, objectiveFunctionDown)
  
  return(objectiveFunction)
  
}