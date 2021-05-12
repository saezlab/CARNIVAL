## This code writes the objective function of the ILP problem for all the 
## conditions.
##
## Enio Gjerga, 2020
createObjectiveFunction <- function(dataVector = dataVector, 
                                    variables = variables, 
                                    measurementsWeights = measurementsWeights, 
                                    alphaWeight = alphaWeight, 
                                    betaWeight = betaWeight, 
                                    scores = scores) {
  
  .Deprecated("createObjectiveFunction_v2")
  objectiveFunctionStart <- "Obj:\t "
  
  if(is.null(scores)){
    measured <- gsub(names(dataVector$dataVector)[dataVector$dsID], 
                     pattern = "DS:", replacement = "")
    
    idxMeasured <- c()
    for(i in seq_len(length(measured))){
      idxMeasured <- c(idxMeasured, which(variables$expNodesReduced ==
                                            paste0("Species ", measured[i])))
    }
    
    measuredVar <- variables$variables[idxMeasured]

    allWeights <- rep(x = 0, length(measuredVar))
    
    if(!is.null(measurementsWeights)){
      
      weightedSpecies <- names(measurementsWeights)
      for(i in seq_len(length(weightedSpecies))){
        
        allWeights[which(which(variables$expNodesReduced ==
                                 paste0("Species ", weightedSpecies[i])) ==
                           idxMeasured)] <- measurementsWeights[i]
        
      }
      
      objectiveFunctionVec <- paste0(" + ", allWeights, " absDiff", 
                                     gsub(measuredVar, pattern = "xb", 
                                          replacement = ""))
      
    } else {
      
      objectiveFunctionVec <- paste0(" + ", alphaWeight, " absDiff", 
                                     gsub(measuredVar, pattern = "xb", 
                                          replacement = ""))
      
    }
    
    objectiveFunction <- paste(objectiveFunctionVec, collapse = "")
    
    objectiveFunction <- substring(text = objectiveFunction[1], first = 4, 
                                   last = nchar(objectiveFunction))
    
    objectiveFunction <- paste0("", objectiveFunction)
    
    objectiveFunctionUpVec <- paste0(
      " + ", betaWeight, " ", variables$variables[variables$idxNodesUp])
    objectiveFunctionUp <- paste(objectiveFunctionUpVec, collapse = "")
    
    objectiveFunctionDownVec <- paste0(
      " + ", betaWeight, " ", variables$variables[variables$idxNodesDown])
    objectiveFunctionDown <- paste(objectiveFunctionDownVec, collapse = "")
    
    objectiveFunction <- paste0(objectiveFunction, objectiveFunctionUp)
    objectiveFunction <- paste0(objectiveFunction, objectiveFunctionDown)
    
    objectiveFunction <- paste0(objectiveFunctionStart, objectiveFunction)
    
    return(objectiveFunction)
    
  } else {
    
    measured <- gsub(names(dataVector$dataVector)[dataVector$dsID], 
                     pattern = "DS:", replacement = "")
    
    idxMeasured <- c()
    for(i in seq_len(length(measured))){
      
      idxMeasured <- c(idxMeasured, which(variables$expNodesReduced==
                                            paste0("Species ", measured[i])))
      
    }
    
    measuredVar <- variables$variables[idxMeasured]
    
    allWeights <- rep(x = 0, length(measuredVar))
    
    if(!is.null(measurementsWeights)){
      
      weightedSpecies <- names(measurementsWeights)
      
      for(i in seq_len(length(weightedSpecies))){
        
        allWeights[which(which(variables$expNodesReduced==
                                 paste0("Species ", weightedSpecies[i]))==
                           idxMeasured)] <- measurementsWeights[i]
        
      }
      
      objectiveFunctionVec <- paste0(" + ", allWeights, " absDiff", 
                                     gsub(measuredVar, pattern = "xb", 
                                          replacement = ""))
      
    } else {
      
      objectiveFunctionVec <- paste0(" + ", alphaWeight, " absDiff", 
                                     gsub(measuredVar, pattern = "xb", 
                                          replacement = ""))
      
    }
    objectiveFunction <- paste(objectiveFunctionVec, collapse = "")
    objectiveFunction <- substring(text = objectiveFunction[1], first = 4, 
                                   last = nchar(objectiveFunction))
    
    objectiveFunction <- paste0("", objectiveFunction)
    
    betaValPos <- rep(betaWeight, length(variables$idxNodesUp))
    betaValNeg <- rep(betaWeight, length(variables$idxNodesDown))
    
    #
    idxPos <- which(scores[1, ] >= 0)
    if(length(idxPos) > 0){
      
      speciesPos <- names(scores)[idxPos]
      
      for(ii in seq_len(length(speciesPos))){
        
        currPos <- speciesPos[ii]
        
        idx <- which(variables$exp[variables$idxNodesUp]==
                       paste0("SpeciesUP ", currPos))
        betaValPos[idx] <- betaWeight*(1-scores[1, idxPos[ii]])
        
        idx <- which(variables$exp[variables$idxNodesDown]==
                       paste0("SpeciesDown ", currPos))
        betaValNeg[idx] <- betaWeight*(1+scores[1, idxPos[ii]])
        
      }
      
    }
    
    #
    idxNeg <- which(scores[1, ] < 0)
    if(length(idxNeg) > 0){
      
      speciesNeg <- names(scores)[idxNeg]
      
      for(ii in seq_len(length(speciesNeg))){
        
        currNeg <- speciesNeg[ii]
        
        idx <- which(variables$exp[variables$idxNodesUp]==
                       paste0("SpeciesUP ", currNeg))
        betaValPos[idx] <- betaWeight*(1-scores[1, idxNeg[ii]])
        
        idx <- which(variables$exp[variables$idxNodesDown]==
                       paste0("SpeciesDown ", currNeg))
        betaValNeg[idx] <- betaWeight*(1+scores[1, idxNeg[ii]])
        
      }
      
    }
    
    objectiveFunctionUpVec <- paste0(
      " + ", betaValPos, " ", variables$variables[variables$idxNodesUp])
    objectiveFunctionUp <- paste(objectiveFunctionUpVec, collapse = "")
    
    objectiveFunctionDownVec <- paste0(
      " + ", betaValNeg, " ", variables$variables[variables$idxNodesDown])
    objectiveFunctionDown <- paste(objectiveFunctionDownVec, collapse = "")
    
    objectiveFunction <- paste0(objectiveFunction, objectiveFunctionUp)
    objectiveFunction <- paste0(objectiveFunction, objectiveFunctionDown)
  }
  
  objectiveFunction <- paste0(objectiveFunctionStart, objectiveFunction)
  
  return(objectiveFunction)
  
}

