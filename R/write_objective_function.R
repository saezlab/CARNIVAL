#'\code{write_objective_function}
#'
#'@param dataMatrix Contains the matrix which stores the information for each node in the PKN, i.e. acivity of the nodes which are measured,at each condition.
#'@param variables Contains the list of variables as used to formulate the ILP problem, explanations for each variable and a list of useful indeces.
#'@param alphaWeight The weightning factor of the measurement.
#'@param betaWeight The weightning factor of the network size.
#'@param scores The provided PROGENy scores.
#'@param measWeights A weightning factor for the measurements.
#'@param conditionIDX The index of the current condition being considered.
#'@return This code writes the objective function of the ILP problem for one specific condition.

write_objective_function <- function(dataMatrix = dataMatrix, variables = variables, alphaWeight=alphaWeight, betaWeight=betaWeight, scores = scores, nodeWeights = nodeWeights, measWeights = measWeights, conditionIDX = conditionIDX ){

  if(is.null(scores)){

    measured <- gsub(colnames(dataMatrix$dataMatrix)[dataMatrix$dsID], pattern = "DS:", replacement = "")
    # measured <- c(measured, gsub(colnames(dataMatrix$dataMatrix)[dataMatrix$dnID], pattern = "DN:", replacement = ""))

    idxMeasured <- c()
    for(i in 1:length(measured)){

      idxMeasured <- c(idxMeasured, which(variables$expNodesReduced==paste0("Species ", measured[i])))

    }

    measuredVar <- variables$variables[idxMeasured]

    allWeights <- rep(x = 0, length(measuredVar))

    if(!is.null(measWeights)){

      weightedSpecies <- rownames(measWeights)

      for(i in 1:length(weightedSpecies)){

        allWeights[which(which(variables$expNodesReduced==paste0("Species ", weightedSpecies[i]))==idxMeasured)] <- measWeights[i]

      }

      objectiveFunctionVec <- paste0(" + ", allWeights, "absDiff", gsub(measuredVar, pattern = "xb", replacement = ""))

    } else {

      objectiveFunctionVec <- paste0(" + ", alphaWeight, "absDiff", gsub(measuredVar, pattern = "xb", replacement = ""))

    }

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
    # measured <- c(measured, gsub(colnames(dataMatrix$dataMatrix)[dataMatrix$dnID], pattern = "DN:", replacement = ""))

    idxMeasured <- c()
    for(i in 1:length(measured)){

      idxMeasured <- c(idxMeasured, which(variables$expNodesReduced==paste0("Species ", measured[i])))

    }

    measuredVar <- variables$variables[idxMeasured]

    allWeights <- rep(x = 0, length(measuredVar))

    if(!is.null(measWeights)){

      weightedSpecies <- rownames(measWeights)

      for(i in 1:length(weightedSpecies)){

        allWeights[which(which(variables$expNodesReduced==paste0("Species ", weightedSpecies[i]))==idxMeasured)] <- measWeights[i]

      }

      objectiveFunctionVec <- paste0(" + ", allWeights, "absDiff", gsub(measuredVar, pattern = "xb", replacement = ""))

    } else {

      objectiveFunctionVec <- paste0(" + ", alphaWeight, "absDiff", gsub(measuredVar, pattern = "xb", replacement = ""))

    }

    objectiveFunction <- paste(objectiveFunctionVec, collapse = "")

    objectiveFunction <- substring(text = objectiveFunction[1], first = 4, last = nchar(objectiveFunction))

    objectiveFunction <- paste0("", objectiveFunction)

    betaValPos <- rep(betaWeight, length(variables$idxNodesUp))
    betaValNeg <- rep(betaWeight, length(variables$idxNodesDown))

    #
    idxPos <- which(scores[1, ] >= 0)
    if(length(idxPos) > 0){

      speciesPos <- colnames(scores)[idxPos]

      for(ii in 1:length(speciesPos)){

        currPos <- speciesPos[ii]

        idx <- which(variables$exp[variables$idxNodesUp]==paste0("SpeciesUP ", currPos, " in experiment ", conditionIDX))
        betaValPos[idx] <- betaWeight*(1-scores[1, idxPos[ii]])

        idx <- which(variables$exp[variables$idxNodesDown]==paste0("SpeciesDown ", currPos, " in experiment ", conditionIDX))
        betaValNeg[idx] <- betaWeight*(1+scores[1, idxPos[ii]])

      }

    }

    #
    idxNeg <- which(scores[1, ] < 0)
    if(length(idxNeg) > 0){

      speciesNeg <- colnames(scores)[idxNeg]

      for(ii in 1:length(speciesNeg)){

        currNeg <- speciesNeg[ii]

        idx <- which(variables$exp[variables$idxNodesUp]==paste0("SpeciesUP ", currNeg, " in experiment ", conditionIDX))
        betaValPos[idx] <- betaWeight*(1-scores[1, idxNeg[ii]])

        idx <- which(variables$exp[variables$idxNodesDown]==paste0("SpeciesDown ", currNeg, " in experiment ", conditionIDX))
        betaValNeg[idx] <- betaWeight*(1+scores[1, idxNeg[ii]])

      }

    }

    objectiveFunctionUpVec <- paste0(" + ", betaValPos, " ", variables$variables[variables$idxNodesUp])
    objectiveFunctionUp <- paste(objectiveFunctionUpVec, collapse = "")

    objectiveFunctionDownVec <- paste0(" + ", betaValNeg, " ", variables$variables[variables$idxNodesDown])
    objectiveFunctionDown <- paste(objectiveFunctionDownVec, collapse = "")

    objectiveFunction <- paste0(objectiveFunction, objectiveFunctionUp)
    objectiveFunction <- paste0(objectiveFunction, objectiveFunctionDown)

    return(objectiveFunction)

  }

}
