## This code writes the objective function of the ILP problem 
##
## Enio Gjerga, Olga Ivanova 2020-2021

createObjectiveFunction_v2 <- function(variables, alphaWeight, betaWeight, pathwaysScores) {
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
