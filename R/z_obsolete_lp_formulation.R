## Obsolete code kept here for backward compatibility for v.2.1. 
## Planned to be removed in v.3

createLpFormulation <- function( internalDataRepresentation, 
                                 dataPreprocessed,
                                 carnivalOptions ) {
  .Deprecated("createLpFormulation_v2")
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

createBinaries <- function(variables = variables){
  .Deprecated("createBinaries_v2")
  cc1 <- paste0("\t", variables$variables[variables$idxNodesUp])
  cc2 <- paste0("\t", variables$variables[variables$idxNodesDown])
  cc3 <- paste0("\t", variables$variables[variables$idxEdgesUp])
  cc4 <- paste0("\t", variables$variables[variables$idxEdgesDown])
  
  return(c(cc1, cc2, cc3, cc4))
  
}

createBoundaries <- function(variables = variables, 
                             objectiveFunction = objectiveFunction){
  .Deprecated("createBoundaries_v2")
  M <- 100
  cc1 <- paste0("\t", 
                "-1 <= ", 
                variables$variables[variables$idxNodes], 
                " <= 1")
  cc2 <- paste0("\t", 
                "0 <= ", 
                variables$variables[variables$idxNodesUp], 
                " <= 1")
  cc3 <- paste0("\t", 
                "0 <= ", 
                variables$variables[variables$idxNodesDown], 
                " <= 1")
  cc4 <- paste0("\t", 
                "0 <= ", 
                variables$variables[variables$idxEdgesUp], 
                " <= 1")
  cc5 <- paste0("\t", 
                "0 <= ",  
                variables$variables[variables$idxEdgesDown], 
                " <= 1")
  
  cc6 <- paste0("\t", "-1 <= ", variables$variables[variables$idxB], 
                " <= 1")
  cc7 <- paste0("\t", "0 <= ", variables$variables[variables$idxDist], 
                " <= ", M)
  cc8 <- paste0("\t", "0 <= ", unique(strsplit(objectiveFunction, split = " ")[[1]][grep(
    pattern = "absDiff", 
    x = strsplit(objectiveFunction, split = " ")[[1]])]), 
    " <= 2")
  cc9 <- paste0("\t", "0 <= ", variables$variables[variables$idxDist], 
                " <= ", M)
  
  return(c(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9))
  
}

createGenerals <- function(variables = variables, objectiveFunction = objectiveFunction){
  .Deprecated("createGenerals_v2")
  generals <- c(paste0("\t", 
                       variables$variables[variables$idxNodes]),
                paste0("\t", 
                       variables$variables[variables$idxB]),
                paste0("\t",
                       unique(strsplit(objectiveFunction, split = " ")[[1]][grep(
                         pattern = "absDiff", 
                         x = strsplit(objectiveFunction, split = " ")[[1]])])))
  
  return(generals)
  
}
