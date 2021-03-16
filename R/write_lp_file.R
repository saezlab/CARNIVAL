## Write a list of linear programming (LP) constraints into a file while will be
## read by interactive cplex solver to perform network optimisation.
##
## Enio Gjerga, 2020



# defaultConstraintsFunctions <- list(
#   
#   write_constraints_1 =   data.frame(func = write_constraints_1, 
#                              param = "variables"),
#   
#   write_constraints_2 =    data.frame(func = "write_constraints_2", 
#                              param = "variables"), 
#   
#   write_constraints_3 =    data.frame(func = "write_constraints_3", 
#                                       param = c("variables", "dataMatrix"))
#                              
# )
# 
# addConstraint <- function(func, ...) {
#   results <- func(...)
#   return(results)
# }
# 
# writeConstraints <- function(constraintsFunctions = defaultConstraintsFunctions) {
#   
#   lapply(constraintsFunctions, function(x){
#     print(x["func"])
#     functionTocall <- eval(parse(text = x['func']))
#     functionToCall(x['param']) 
#   })
# 
#   invisible(
#     lapply(names(constraintsFunctions), function(x) {
#       value = unlist(constraintsFunctions[x])
#       checkValue = carnivalOptionsErrorChecks[[x]]
#       
#       # if there are several checks, apply all
#       if (is.data.frame(checkValue)) {
#         apply(checkValue, 1, checkGenericFunction, value)
#       } else {
#         checkGenericFunction(checkValue, value)
#       }
#     }))
# 
# }



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
  
  variablesTest <- variables[[1]]
  
  c0 <- writeConstraintsObjFunction(variables = variablesTest,
                                    dataMatrix = dataMatrix)
  
  c1 <- write_constraints_1(variables = variablesTest)
  c2 <- write_constraints_2(variables = variablesTest)
  c3 <- write_constraints_3(variables = variablesTest)
  c4 <- write_constraints_4(variables = variablesTest)
  c5 <- write_constraints_5(variables = variablesTest)
  c6 <- write_constraints_6(variables = variables, dataMatrix = dataMatrix,
                            priorKnowledgeNetwork = priorKnowledgeNetwork)
  c7 <- write_constraints_7(variables = variables, dataMatrix = dataMatrix,
                            priorKnowledgeNetwork = priorKnowledgeNetwork)
  c8 <- write_constraints_8(variables = variables, perturbations = perturbations,
                            priorKnowledgeNetwork = priorKnowledgeNetwork)
  
  c9 <- write_loop_constraints(variables = variables, 
                               priorKnowledgeNetwork = priorKnowledgeNetwork,
                               perturbations = perturbations)
  
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
  
  return(variables)
}

#TODO 
writeLpFileWithConstraints <- function(...){
  
}
