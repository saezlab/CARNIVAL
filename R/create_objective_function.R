## This code writes the objective function of the ILP problem for all the 
## conditions.
##
## Enio Gjerga, 2020

createObjectiveFunction_newIntRep <- function( variables = variables, 
                                               alphaWeight = alphaWeight, 
                                               betaWeight = betaWeight, 
                                               pathwaysScores = pathwaysScores) {
  objectiveFunction <- "Obj:\t "
  
  if(is.null(pathwaysScores)){
    
    objectiveFunctionMeasurements <- paste(variables$measurementsDf$value, 
                                           variables$measurementsDf$measurementsVars, "+ ")
    
    objectiveFunctionEdgesUp <- paste(betaWeight, variables$edgesDf$edgesUpVars, "+ ")
    objectiveFunctionEdgesDown <- paste(betaWeight, variables$edgesDf$edgesDownVars, "+ ")
    
    objectiveFunction <- paste(c(objectiveFunction, objectiveFunctionMeasurements, 
                                              objectiveFunctionEdgesUp, 
                                              objectiveFunctionEdgesDown), collapse = "")
    
    #removing the + sign at the end of the objective function (formed in paste func above)
    objectiveFunction <- substr(objectiveFunction, 1, nchar(objectiveFunction) - 3)
  } else {
    stop("Working with weights is not supported yet") 
  }
  
  return(objectiveFunction)
}
