## Write a list of linear programming (LP) constraints into a file while will be
## read by interactive cplex solver to perform network optimisation.
##
## Enio Gjerga, 2020

writeLpFile <- function(perturbations, 
                        measurements, 
                        pathwayWeights, 
                        priorKnowledgeNetwork, 
                        carnivalOptions) {
  
  message("Writing constraints...")
  options(scipen=999)
  
  dataMatrix <- buildDataMatrix(measurements = measurements, 
                                priorKnowledgeNetwork = priorKnowledgeNetwork, 
                                perturbations = perturbations)
  
  variables <- create_variables_all(pknList = priorKnowledgeNetwork, 
                                    dataMatrix = dataMatrix)
  
  measurementsWeights <- abs(measurements)
  
  objectiveFunction <- write_objective_function_all(dataMatrix = dataMatrix,
                                     variables = variables,
                                     measurementsWeights = measurementsWeights,
                                     alphaWeight = carnivalOptions$alphaWeight,
                                     betaWeight = carnivalOptions$betaWeight, 
                                     scores = pathwayWeights)
  
  message("Generating constraints for linear programming problem...")
  
  bounds <- write_boundaries(variables = variables, oF = objectiveFunction)
  binaries <- write_binaries(variables = variables)
  generals <- write_generals(variables = variables, oF = objectiveFunction)
  
  #TODO line exists to fix the current implementation inside constraints. Will be removed when 
  #each of the constraint fixed (several experimental conditions removed)
  variablesTemp <- variables[[1]]

  c0 <- writeConstraintsObjFunction(variables = variablesTemp,
                                    dataMatrix = dataMatrix)

  c1 <- write_constraints_1(variables = variablesTemp)
  c2 <- write_constraints_2(variables = variablesTemp)
  c3 <- write_constraints_3(variables = variablesTemp)
  c4 <- write_constraints_4(variables = variablesTemp)
  c5 <- write_constraints_5(variables = variablesTemp)

  c6 <- write_constraints_6(variables = variables, dataMatrix = dataMatrix,
                            priorKnowledgeNetwork = priorKnowledgeNetwork)
  c7 <- write_constraints_7(variables = variables, dataMatrix = dataMatrix,
                            priorKnowledgeNetwork = priorKnowledgeNetwork)
  c8 <- write_constraints_8(variables = variables, perturbations = perturbations,
                            priorKnowledgeNetwork = priorKnowledgeNetwork)

  c9 <- write_loop_constraints(variables = variables, perturbations = perturbations,
                               priorKnowledgeNetwork = priorKnowledgeNetwork)
  
  #TODO
  #createConstraints(variables, variablesTemp, dataMatrix, perturbations, 
  #                  priorKnowledgeNetwork) 
  
  allConstraints <- c(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9)
  allConstraints <- concatenateConstraints(allConstraints)
  
  message("Creating LP file...")

  writeSolverFile(objectiveFunction = objectiveFunction,
                  allConstraints = allConstraints,
                  bounds = bounds,
                  binaries = binaries,
                  generals = generals,
                  carnivalOptions = carnivalOptions)

  message("Done: Creating LP file.")
  message("Saving parsed data")
  
  writeParsedData(variables = variables, 
                  priorKnowledgeNetwork = priorKnowledgeNetwork, 
                  perturbations = perturbations,
                  measurements = measurements, 
                  carnivalOptions = carnivalOptions)
  
  return(variables)
}

#TODO
prepareDataForLpFile <- function(measurements = measurements, 
                                 priorKnowledgeNetwork = priorKnowledgeNetwork, 
                                 perturbations = perturbations) {
  
  dataMatrix <- buildDataMatrix(measurements = measurements, 
                                priorKnowledgeNetwork = priorKnowledgeNetwork, 
                                perturbations = perturbations)
  
  variables <- create_variables_all(pknList = priorKnowledgeNetwork, 
                                    dataMatrix = dataMatrix)
  
  #return(c(dataMatrix, variables))
  return(variables)
}

#TODO
createConstraints <- function(variables, variablesTemp, dataMatrix, perturbations, 
                              priorKnowledgeNetwork) {
  
  constraintFunctions <- c( "0"=writeConstraintsObjFunction,
                            "1"=write_constraints_1, 
                            "2"=write_constraints_2, 
                            "3"=write_constraints_3, 
                            "4"=write_constraints_4, 
                            "5"=write_constraints_5, 
                            "6"=write_constraints_6,
                            "7"=write_constraints_7,
                            "8"=write_constraints_8,
                            "9"=write_loop_constraints )
  
  constraintFunctionsParams <- c("0" = c(variablesTemp, dataMatrix), 
                                 "1" = c(variablesTemp),
                                 "2" = c(variablesTemp),
                                 "3" = c(variablesTemp), 
                                 "4" = c(variablesTemp), 
                                 "5" = c(variablesTemp), 
                                 "6" = c(variables, dataMatrix, priorKnowledgeNetwork),
                                 "7" = c(variables, dataMatrix, priorKnowledgeNetwork),
                                 "8" = c(variables, perturbations, priorKnowledgeNetwork), 
                                 "9" = c(variables, perturbations, priorKnowledgeNetwork))
  
  
  constraints <- lapply(seq(0:length(constraintFunctions)), function(x) {
                            new_constraint <- callConstraintFunctions(constraintFunction = constraintFunctions[i], 
                                                                      constraintFunctionsParams[i])
                            return(new_constraint)
                  })
}

#TODO 
callConstraintsFunctions <- function(constraintFunction, ...) {
  constraintFormed <- constraintFunction(...)
  return(constraintFormed)
}


#TODO 
writeLpFileWithConstraints <- function(...){
  
}
