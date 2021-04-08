## Write a list of linear programming (LP) constraints into a file while will be
## read by interactive cplex solver to perform network optimisation.
##
## Enio Gjerga, 2020

createLpFormulation <- function( internalDataRepresentation, 
                                 dataPreprocessed,
                                 carnivalOptions) {
  
  message("Writing constraints...")
  options(scipen=999)
  
  dataVector <- internalDataRepresentation[[1]]
  variables <- internalDataRepresentation[[2]]
  measurementsWeights <- abs(dataPreprocessed$measurements)
  
  objectiveFunction <- createObjectiveFunction( dataVector = dataVector,
                                                variables = variables,
                                                measurementsWeights = measurementsWeights,
                                                alphaWeight = carnivalOptions$alphaWeight,
                                                betaWeight = carnivalOptions$betaWeight,
                                                scores = dataPreprocessed$pathwayWeights )
  
  message("Generating constraints for linear programming problem...")
  
  bounds <- createBoundaries(variables = variables, 
                             objectiveFunction = objectiveFunction)
  
  binaries <- createBinaries(variables = variables)
  
  generals <- createGenerals(variables = variables, 
                             objectiveFunction = objectiveFunction)

  c0 <- createConstraintsObjectiveFunction(variables = variables,
                                           dataVector = dataVector)

  c1 <- createConstraints_1(variables = variables)
  c2 <- createConstraints_2(variables = variables)
  c3 <- createConstraints_3(variables = variables)
  c4 <- createConstraints_4(variables = variables)
  c5 <- createConstraints_5(variables = variables)

  c6 <- createConstraints_6(variables = variables, 
                            priorKnowledgeNetwork = dataPreprocessed$priorKnowledgeNetwork)
  c7 <- createConstraints_7(variables = variables,
                            priorKnowledgeNetwork = dataPreprocessed$priorKnowledgeNetwork)
  c8 <- createConstraints_8(variables = variables, 
                            perturbations = dataPreprocessed$perturbations,
                            priorKnowledgeNetwork = dataPreprocessed$priorKnowledgeNetwork)

  c9 <- createLoopConstraints(variables = variables, 
                              perturbations = dataPreprocessed$perturbations,
                              priorKnowledgeNetwork = dataPreprocessed$priorKnowledgeNetwork)

  allConstraints <- list(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9)
  
  allConstraints <- concatenateConstraints(unlist(allConstraints))
  
  lpProblemFormed <- list("objectiveFunction" = objectiveFunction,
                          "allConstraints" = allConstraints,
                          "bounds" = bounds,
                          "binaries" = binaries,
                          "generals" = generals)
  
  return(lpProblemFormed)
}

createLpFormulation_newIntRep <- function( internalDataRepresentation, 
                                            dataPreprocessed,
                                            carnivalOptions) {
  
  message("Writing constraints...")
  options(scipen=999)
  
  variables <- internalDataRepresentation
  objectiveFunction <- createObjectiveFunction_newIntRep ( variables = variables,
                                                          alphaWeight = carnivalOptions$alphaWeight,
                                                          betaWeight = carnivalOptions$betaWeight,
                                                          scores = dataPreprocessed$pathwayWeights )
  
  message("Generating constraints for linear programming problem...")
  
  bounds <- createBoundaries_newIntRep (variables = variables)
  binaries <- createBinaries_newIntRep(variables = variables)
  generals <- createGenerals_newIntRep(variables = variables)
  
  priorKnowledgeNetwork <- dataPreprocessed$priorKnowledgeNetwork)

  c0 <- createConstraintsMeasuredNodes_newIntRep(variables = variables)
  
  c1 <- createConstraints_1_newIntRep(variables, priorKnowledgeNetwork)
  c2 <- createConstraints_2_newIntRep(variables, priorKnowledgeNetwork)
  c3 <- createConstraints_3_newIntRep(variables = variables)
  
  c4 <- createConstraints_4_newIntRep(variables, priorKnowledgeNetwork)
  c5 <- createConstraints_5_newIntRep(variables, priorKnowledgeNetwork)
  
  c6 <- createConstraints_6_newIntRep(variables, priorKnowledgeNetwork)
  c7 <- createConstraints_7_newIntRep(variables, priorKnowledgeNetwork)
  
  c8 <- createConstraints_8_newIntRep(variables, 
                            perturbations = dataPreprocessed$perturbations,
                            priorKnowledgeNetwork)
  
  c9 <- createLoopConstraints(variables = variables, 
                              perturbations = dataPreprocessed$perturbations,
                              priorKnowledgeNetwork = dataPreprocessed$priorKnowledgeNetwork)
  
  allConstraints <- list(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9)
  
  allConstraints <- concatenateConstraints(unlist(allConstraints))
  
  lpProblemFormed <- list("objectiveFunction" = objectiveFunction,
                          "allConstraints" = allConstraints,
                          "bounds" = bounds,
                          "binaries" = binaries,
                          "generals" = generals)
  
  return(lpProblemFormed)
}



createConstraintFreeForm <- function(...) {
  constraint <- paste(...)  
  return(constraint)
}


createConstraint <- function(variable1, sign, variable2, inequality, rightPart) { 
  constraint <- paste(variable1, sign, variable2, inequality, rightPart, sep = " ")  
  return(constraint)
}

defaultListConstraints <- function() {
  defaultListConstrains <- c("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9")
  return(defaultListConstrains)
}
