write_objective_function <- function(dataMatrix = dataMatrix, variables = variables, alphaWeight=alphaWeight, betaWeight=betaWeight, scores = scores, nodeWeights = nodeWeights ){
  
  if(is.null(scores)){
    
    measured <- gsub(colnames(dataMatrix$dataMatrix)[dataMatrix$dsID], pattern = "DS:", replacement = "")
    measured <- c(measured, gsub(colnames(dataMatrix$dataMatrix)[dataMatrix$dnID], pattern = "DN:", replacement = ""))
    
    idxMeasured <- c()
    for(i in 1:length(measured)){
      
      idxMeasured <- c(idxMeasured, which(variables$expNodesReduced==paste0("Species ", measured[i])))
      
    }
    
    measuredVar <- variables$variables[idxMeasured]
    
    objectiveFunctionVec <- paste0(" + ", alphaWeight, " absDiff", gsub(measuredVar, pattern = "xb", replacement = ""))
    objectiveFunction <- paste(objectiveFunctionVec, collapse = "")
    
    objectiveFunction <- substring(text = objectiveFunction[1], first = 4, last = nchar(objectiveFunction))
    
    objectiveFunction <- paste0("", objectiveFunction)
    
    objectiveFunctionUpVec <- paste0(" + ", betaWeight, " ", variables$variables[variables$idxNodesUp])
    objectiveFunctionUp <- paste(objectiveFunctionUpVec, collapse = "")
    
    objectiveFunctionDownVec <- paste0(" + ", betaWeight, " ", variables$variables[variables$idxNodesDown])
    objectiveFunctionDown <- paste(objectiveFunctionDownVec, collapse = "")
    
    objectiveFunction <- paste0(objectiveFunction, objectiveFunctionUp)
    objectiveFunction <- paste0(objectiveFunction, objectiveFunctionDown)
    
    return(objectiveFunction)
    
  } else {
    
    measured <- gsub(colnames(dataMatrix$dataMatrix)[dataMatrix$dsID], pattern = "DS:", replacement = "")
    measured <- c(measured, gsub(colnames(dataMatrix$dataMatrix)[dataMatrix$dnID], pattern = "DN:", replacement = ""))
    
    idxMeasured <- c()
    for(i in 1:length(measured)){
      
      idxMeasured <- c(idxMeasured, which(variables$expNodesReduced==paste0("Species ", measured[i])))
      
    }
    
    measuredVar <- variables$variables[idxMeasured]
    
    objectiveFunctionVec <- paste0(" + ", alphaWeight, " absDiff", gsub(measuredVar, pattern = "xb", replacement = ""))
    objectiveFunction <- paste(objectiveFunctionVec, collapse = "")
    
    objectiveFunction <- substring(text = objectiveFunction[1], first = 4, last = nchar(objectiveFunction))
    
    objectiveFunction <- paste0("", objectiveFunction)
    
    # nodeWeights <- scores
    # absolutenodeWeights <- abs(nodeWeights)
    # for(i in 1:length(nodeWeights)){
    #   
    #   # nodeWeights[i] <- sign(scores[i])*(1-(absolutenodeWeights[i] - min(absolutenodeWeights, na.rm = TRUE))/(max(absolutenodeWeights, na.rm = TRUE) - min(absolutenodeWeights, na.rm = TRUE)))
    #   nodeWeights[i] <- 1 - 2*(absolutenodeWeights[i] - min(absolutenodeWeights, na.rm = TRUE))/(max(absolutenodeWeights, na.rm = TRUE) - min(absolutenodeWeights, na.rm = TRUE))
    #   
    # }
    # absolutenodeWeights <- abs(nodeWeights)
    
    weightedSpecies <- paste0("Species ", names(nodeWeights))
    
    ##
    idx1 <- which(variables$expNodesReduced%in%weightedSpecies[which(scores > 0)])

    objectiveFunctionUp1 <- ""
    
    if(length(idx1) > 0){
      
      for(i in 1:length(idx1)){
        
        ww <- nodeWeights[which(weightedSpecies==variables$expNodesReduced[idx1[i]])]
        
        if(sign(ww)==1){
          
          objectiveFunctionUpVec <- paste0(" + ", betaWeight*ww, " ", variables$variables[variables$idxNodesUp[idx1[i]]])
          objectiveFunctionUp1 <- paste(objectiveFunctionUp1, objectiveFunctionUpVec, sep = "")
          
        } else {
          
          objectiveFunctionUpVec <- paste0(" - ", abs(betaWeight*ww), " ", variables$variables[variables$idxNodesUp[idx1[i]]])
          objectiveFunctionUp1 <- paste(objectiveFunctionUp1, objectiveFunctionUpVec, sep = "")
          
        }
        
      }
      
    }
    
    ##
    idx2 <- which(variables$expNodesReduced%in%weightedSpecies[which(scores < 0)])
    
    objectiveFunctionDown1 <- ""
    
    if(length(idx2) > 0){
      
      for(i in 1:length(idx2)){
        
        ww <- nodeWeights[which(weightedSpecies==variables$expNodesReduced[idx2[i]])]
        
        if(sign(ww)==1){
          
          objectiveFunctionDownVec <- paste0(" + ", betaWeight*ww, " ", variables$variables[variables$idxNodesDown[idx2[i]]])
          objectiveFunctionDown1 <- paste(objectiveFunctionDown1, objectiveFunctionDownVec, sep = "")
          
        } else {
          
          objectiveFunctionDownVec <- paste0(" - ", abs(betaWeight*ww), " ", variables$variables[variables$idxNodesDown[idx2[i]]])
          objectiveFunctionDown1 <- paste(objectiveFunctionDown1, objectiveFunctionDownVec, sep = "")
          
        }
        
      }
      
    }
    
    ##
    
    objectiveFunctionUpVec <- paste0(" + ", betaWeight, " ", variables$variables[variables$idxNodesUp[setdiff(1:length(variables$idxNodesUp), idx1)]])
    objectiveFunctionUp2 <- paste(objectiveFunctionUpVec, collapse = "")

    objectiveFunctionDownVec <- paste0(" + ", betaWeight, " ", variables$variables[variables$idxNodesDown[setdiff(1:length(variables$idxNodesDown), idx2)]])
    objectiveFunctionDown2 <- paste(objectiveFunctionDownVec, collapse = "")
    
    objectiveFunction <- paste0(objectiveFunction, paste0(objectiveFunctionUp1, objectiveFunctionUp2))
    objectiveFunction <- paste0(objectiveFunction, paste0(objectiveFunctionDown1, objectiveFunctionDown2))
    
    write(objectiveFunction, file = "objFun.txt")
    
    return(objectiveFunction)
    
  }
  
}