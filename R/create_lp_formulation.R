## Write a list of linear programming (LP) constraints into a file while will be
## read by interactive cplex solver to perform network optimisation.
##
## Enio Gjerga, 2020

createLpFormulation_newIntRep <- function( internalDataRepresentation, 
                                           dataPreprocessed,
                                           carnivalOptions ) {
  
  message("Writing constraints...")
  options(scipen=999)
  
  variables <- internalDataRepresentation
  objectiveFunction <- createObjectiveFunction_newIntRep ( variables = variables,
                                                           alphaWeight = carnivalOptions$alphaWeight,
                                                           betaWeight = carnivalOptions$betaWeight,
                                                           pathwaysScores = dataPreprocessed$pathwayWeights )
  
  message("Generating constraints for linear programming problem...")
  
  bounds <- createBoundaries_newIntRep(variables = variables)
  binaries <- createBinaries_newIntRep(variables = variables)
  generals <- createGenerals_newIntRep(variables = variables)

  c0 <- createConstraintsMeasuredNodes_newIntRep(variables)
  c1_2 <- createConstraints_1_2_newIntRep(variables)
  c3 <- createConstraints_3_newIntRep(variables)
  c4_5 <- createConstraints_4_5_newIntRep(variables)
  c6_7 <- createConstraints_6_7_newIntRep(variables)
  c8 <- createConstraints_8_newIntRep(variables, perturbations = dataPreprocessed$perturbations)
  c9 <- createLoopConstraints_newIntRep(variables)
  
  allConstraints <- c(c0, c1_2, c3, c4_5, c6_7, c8, c9)
  
  allConstraints <- concatenateConstraints(unlist(allConstraints))
  
  lpProblemFormed <- list("objectiveFunction" = objectiveFunction,
                          "allConstraints" = allConstraints,
                          "bounds" = bounds,
                          "binaries" = binaries,
                          "generals" = generals)
  
  return(lpProblemFormed)
}

createBoundary <- function(lowLimit, variable, upperLimit) {
  boundary <- paste0("\t", lowLimit, " <= ", variable, " <= ", upperLimit)
  return(boundary)
}

createBoundaries_newIntRep <- function(variables){
  distanceConstant <- 100 
  
  b1 <- createBoundary(-1, variables$nodesDf$nodesVars, 1)
  
  b2 <- createBoundary(0, variables$nodesDf$nodesUpVars, 1)
  b3 <- createBoundary(0, variables$nodesDf$nodesDownVars, 1)
  
  b4 <- createBoundary(0, variables$edgesDf$edgesUpVars, 1)
  b5 <- createBoundary(0, variables$edgesDf$edgesDownVars, 1)
  
  b6 <- createBoundary(0, variables$measurementsDf$measurementsVars, 2)
  b7 <- createBoundary(-1, variables$nodesDf$nodesActStateVars, 1)
  
  b8 <- createBoundary(0, variables$nodesDf$nodesDistanceVars, distanceConstant)
  
  return(c(b1, b2, b3, b4, b5, b6, b7, b8))
}

createBinaries_newIntRep <- function(variables = variables) {
  binaries <- paste(c(variables$nodesDf$nodesUpVars, variables$nodesDf$nodesDownVars,
                      variables$edgesDf$edgesUpVars, variables$edgesDf$edgesDownVars),
                    sep = "\t")
  return(binaries)
}


createGenerals_newIntRep <- function(variables = variables) {
  generals <- paste(c(variables$nodesDf$nodesVars, 
                      variables$nodesDf$nodesActStateVars,
                      variables$measurementsDf$absDifference), sep="\t")
}

defaultListConstraints <- function() {
  defaultListConstrains <- c("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9")
  return(defaultListConstrains)
}
