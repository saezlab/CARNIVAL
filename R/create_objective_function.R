## This code writes the objective function of the ILP problem 
##
## Enio Gjerga, Olga Ivanova 2020-2021

createObjectiveFunction_v2 <- function(variables, alphaWeight, betaWeight, weights) {
  objectiveFunction <- "Obj:\t "
  
  objectiveFunctionMeasurements <- paste(abs(as.numeric(variables$measurementsDf$value)), 
                                         variables$measurementsDf$measurementsVars, "+ ")
  
  if (is.null(weights)){
    objectiveFunctionNodesUp <- paste(betaWeight, variables$nodesDf$nodesUpVars, "+ ")
    objectiveFunctionNodesDown <- paste(betaWeight, variables$nodesDf$nodesDownVars, "+ ")
 
  } else { 
    weightsCalculated <- calculateWeightedValues(weights, betaWeight)
    nodesWeights <- merge(variables$nodesDf, weightsCalculated, by = "nodes", all.x = TRUE)

    nodesWeights$weightsUp[is.na(nodesWeights$weightsUp)] <- betaWeight
    nodesWeights$weightsDown[is.na(nodesWeights$weightsDown)] <- betaWeight
    
    objectiveFunctionNodesUp <- paste(nodesWeights$weightsUp, nodesWeights$nodesUpVars, "+ ")
    objectiveFunctionNodesDown <- paste(nodesWeights$weightsDown, nodesWeights$nodesDownVars, "+ ")
  }
  
  
  objectiveFunction <- paste(c(objectiveFunction, objectiveFunctionMeasurements, 
                               objectiveFunctionNodesUp, 
                               objectiveFunctionNodesDown), collapse = "")
  
  #removing the + sign at the end of the objective function (formed in paste func above)
  objectiveFunction <- substr(objectiveFunction, 1, nchar(objectiveFunction) - 3)
  
  return(objectiveFunction)
}

calculateWeightedValues <- function(weights, betaWeight) {
  weights <- as.data.frame(weights)
  weights$nodes <- rownames(weights)
  weights$weightsUp <- weights$weights
  weights$weightsDown <- weights$weights
  
  weights$weightsUp <- betaWeight * (1 - weights$weightsUp)  
  weights$weightsDown <- betaWeight * (1 + weights$weightsDown)  
  
  return(weights)
}