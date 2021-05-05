## Write a list of linear programming (LP) constraints into a file while will be
## read by interactive cplex solver to perform network optimisation.
##
## Enio Gjerga, Olga Ivanova 2020-2021
createLpFormulation_v2 <- function( internalDataRepresentation,
                                    dataPreprocessed,
                                    carnivalOptions ) {

  message(getTime(), " Generating formulation for LP problem")
  options(scipen=999)

  variables <- internalDataRepresentation
  objectiveFunction <- createObjectiveFunction_v2 (variables = variables,
                                                   alphaWeight = carnivalOptions$alphaWeight,
                                                   betaWeight = carnivalOptions$betaWeight,
                                                   weights = dataPreprocessed$weights)
  bounds <- createBoundaries_v2(variables)
  binaries <- createBinaries_v2(variables)
  generals <- createGenerals_v2(variables)

  c0 <- createConstraintsMeasurements_v2(variables)
  c1_2 <- createConstraints_1_2_v2(variables)
  c3 <- createConstraints_3_v2(variables)
  c4_5 <- createConstraints_4_5_v2(variables)
  c6_7 <- createConstraints_6_7_v2(variables)
  #TODO get rid of perturbations from param (not needed, this info is in nodesDf)
  # Keeping it here until the bug when one node is both perturbation and measurement fixed
  c8 <- createConstraints_8_v2(variables, dataPreprocessed$perturbations)
  c9 <- createLoopConstraints_v2(variables, dataPreprocessed$perturbations)

  allConstraints <- c(c0, c1_2, c3, c4_5, c6_7, c8, c9)
  allConstraints <- concatenateConstraints(unlist(allConstraints))

  lpProblemFormed <- list("objectiveFunction" = objectiveFunction,
                          "allConstraints" = allConstraints,
                          "bounds" = bounds,
                          "binaries" = binaries,
                          "generals" = generals)

  message(getTime(), " Done: generating formulation for LP problem.")

  return(lpProblemFormed)
}

createBoundary <- function(lowLimit, variable, upperLimit) {
  boundary <- paste0("\t", lowLimit, " <= ", variable, " <= ", upperLimit)
  return(boundary)
}

createBoundaries_v2 <- function(variables){
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

createBinaries_v2 <- function(variables) {
  binaries <- paste(c(variables$nodesDf$nodesUpVars, variables$nodesDf$nodesDownVars,
                      variables$edgesDf$edgesUpVars, variables$edgesDf$edgesDownVars),
                    sep = "\t")
  return(binaries)
}


createGenerals_v2 <- function(variables) {
  generals <- paste(c(variables$nodesDf$nodesVars,
                      variables$nodesDf$nodesActStateVars,
                      variables$measurementsDf$absDifference), sep="\t")
}

defaultListConstraints <- function() {
  defaultListConstrains <- c("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9")
  return(defaultListConstrains)
}
