## Write a list of linear programming (LP) constraints into a file while will be
## read by interactive cplex solver to perform network optimisation.
##
## Enio Gjerga, 2020

writeLPFile <- function(perturbations, 
                        measurements, 
                        measurementsSign, 
                        measurementsWeights, 
                        pathwayWeights, 
                        priorKnowledgeNetwork, 
                        repIndex, 
                        condition,
                        carnivalOptions) {
  
  options(scipen=999)
  
  dataMatrix <- buildDataMatrix(data = measurements, 
                                pknList = priorKnowledgeNetwork, 
                                inputs = perturbations)
  
  variables <- create_variables_all(pknList = priorKnowledgeNetwork, 
                                    dataMatrix = dataMatrix)
  
  oF <- write_objective_function_all(dataMatrix = dataMatrix,
                                     variables = variables,
                                     alphaWeight = carnivalOptions$alphaWeight,
                                     betaWeight = carnivalOptions$betaWeight, 
                                     scores = pathwayWeights,
                                     measWeights = measurementsWeights)
  
  message("Generating constraints for linear programming problem...")
  bounds <- write_boundaries(variables = variables, oF=oF)
  binaries <- write_binaries(variables = variables)
  generals <- write_generals(variables = variables, oF = oF)
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
  allC <- all_constraints_wLoop(c0 = c0, c1 = c1, c2 = c2, c3 = c3, c4 = c4,
                                c5 = c5, c6 = c6, c7 = c7, c8 = c8, c9 = c9)
  
  message("Creating LP file...")
  
  writeSolverFiles(condition=condition, repIndex=repIndex, oF=oF,
                   allC=allC, bounds=bounds, binaries=binaries,
                   generals=generals, 
                   mipGAP=carnivalOptions$mipGap,
                   poolrelGAP=carnivalOptions$poolrelGap, 
                   poolReplace=carnivalOptions$poolReplace,
                   limitPop=carnivalOptions$limitPop, 
                   poolCap=carnivalOptions$poolCap,
                   poolIntensity=carnivalOptions$poolIntensity, 
                   timelimit=carnivalOptions$timelimit,
                   threads=carnivalOptions$threads)
  
  message("Done: Creating LP file.")
  
  return(variables)
}
