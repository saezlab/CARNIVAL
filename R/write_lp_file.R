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
  
  internalDataRepresentation <- createInternalDataRepresentation( measurements = measurements, 
                                    priorKnowledgeNetwork = priorKnowledgeNetwork, 
                                    perturbations = perturbations )
  
  dataVector <- internalDataRepresentation[[1]]
  variables <- internalDataRepresentation[[2]]
  
  measurementsWeights <- abs(measurements)
  
  objectiveFunction <- createObjectiveFunction(dataVector = dataVector,
                                              variables = variables,
                                              measurementsWeights = measurementsWeights,
                                              alphaWeight = carnivalOptions$alphaWeight,
                                              betaWeight = carnivalOptions$betaWeight, 
                                              scores = pathwayWeights)
  
  message("Generating constraints for linear programming problem...")
  
  bounds <- createBoundaries(variables = variables, 
                             objectiveFunction = objectiveFunction)
  
  binaries <- createBinaries(variables = variables)
  
  generals <- createGenerals(variables = variables, 
                             objectiveFunction = objectiveFunction)

  c0 <- writeConstraintsObjectiveFunction(variables = variables,
                                          dataVector = dataVector)

  c1 <- createConstraints_1(variables = variables)
  c2 <- createConstraints_2(variables = variables)
  c3 <- createConstraints_3(variables = variables)
  c4 <- createConstraints_4(variables = variables)
  c5 <- createConstraints_5(variables = variables)

  c6 <- createConstraints_6(variables = variables, 
                            priorKnowledgeNetwork = priorKnowledgeNetwork)
  c7 <- createConstraints_7(variables = variables,
                            priorKnowledgeNetwork = priorKnowledgeNetwork)
  c8 <- createConstraints_8(variables = variables, perturbations = perturbations,
                            priorKnowledgeNetwork = priorKnowledgeNetwork)

  c9 <- write_loop_constraints(variables = variables, perturbations = perturbations,
                               priorKnowledgeNetwork = priorKnowledgeNetwork)
  
  #TODO
  #createConstraints(variables, variablesTemp, dataVector, perturbations, 
  #                  priorKnowledgeNetwork)
  
  allConstraints <- list(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9)
  
  allConstraints <- concatenateConstraints(unlist(allConstraints))
  
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
  
  dataVector <- builddataVector(measurements = measurements, 
                                priorKnowledgeNetwork = priorKnowledgeNetwork, 
                                perturbations = perturbations)
  
  variables <- create_variables_all(pknList = priorKnowledgeNetwork, 
                                    dataVector = dataVector)
  
  #return(c(dataVector, variables))
  return(variables)
}

createInternalDataRepresentation <- function( measurements = measurements, 
                                              priorKnowledgeNetwork = priorKnowledgeNetwork, 
                                              perturbations = perturbations ) {
  
  dataVector<- buildDataVector(measurements = measurements, 
                               priorKnowledgeNetwork = priorKnowledgeNetwork, 
                               perturbations = perturbations)
  
  variables <- createVariables(priorKnowledgeNetwork = priorKnowledgeNetwork, 
                               dataVector = dataVector)
  
  return(list("dataVector" = dataVector, "variables" = variables))
}



createConstraint <- function(variable1, sign, variable2, inequality, rightPart) { 
  constraint <- paste(variable1, sign, variable2, inequality, rightPart, sep = " ")  
  return(constraint)
}

#TODO
createConstraints <- function(variables, variablesTemp, dataVector, perturbations, 
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
  
  constraintFunctionsParams <- c("0" = c(variablesTemp, dataVector), 
                                 "1" = c(variablesTemp),
                                 "2" = c(variablesTemp),
                                 "3" = c(variablesTemp), 
                                 "4" = c(variablesTemp), 
                                 "5" = c(variablesTemp), 
                                 "6" = c(variables, dataVector, priorKnowledgeNetwork),
                                 "7" = c(variables, dataVector, priorKnowledgeNetwork),
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

testFunc <- function(){
  return(list("1" = "123", "2" = "234"))
}
