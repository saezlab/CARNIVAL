## Write a list of linear programming (LP) constraints into a file while will be
## read by interactive cplex solver to perform network optimisation.
##
## Enio Gjerga, 2020

writeLpFile <- function(perturbations, 
                        measurements, 
                        pathwayWeights, 
                        priorKnowledgeNetwork, 
                        carnivalOptions) {
  
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
  c0 <- write_constraints_objFunction_all(variables = variables,
                                          dataMatrix = dataMatrix)
  
  c1 <- write_constraints_1_all(variables = variables)
  c2 <- write_constraints_2_all(variables = variables)
  c3 <- write_constraints_3_all(variables = variables)
  c4 <- write_constraints_4_all(variables = variables)
  c5 <- write_constraints_5_all(variables = variables)
  c6 <- write_constraints_6(variables = variables, dataMatrix = dataMatrix,
                           inputs = perturbations, pknList = priorKnowledgeNetwork)
  c7 <- write_constraints_7(variables = variables, dataMatrix = dataMatrix,
                           inputs = perturbations, pknList = priorKnowledgeNetwork)
  c8 <- write_constraints_8(variables = variables, inputs = perturbations,
                            pknList = priorKnowledgeNetwork)
  
  c9 <- write_loop_constraints(variables = variables, pknList = priorKnowledgeNetwork,
                             inputs = perturbations)
  allConstraints <- all_constraints_wLoop(c0 = c0, c1 = c1, c2 = c2, c3 = c3, c4 = c4,
                               c5 = c5, c6 = c6, c7 = c7, c8 = c8, c9 = c9)
  #allConstraints <- all_constraints_wLoop(c8 = c8)
  
  
  message("Creating LP file...")
  
  writeSolverFile(objectiveFunction = objectiveFunction, 
                  allConstraints = allConstraints,
                  bounds = bounds, 
                  binaries = binaries,
                  generals = generals, 
                  carnivalOptions = carnivalOptions)
  
  message("Done: Creating LP file.")
  
  return(variables)
}
