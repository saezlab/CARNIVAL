## Error message in case of errors in the inputs
##
## Enio Gjerga, Olga Ivanova, Attila Gabor, 2020-2021

#' Check input data for CARNIVAL
#' 
#' checks the format of the main data inputs for CARNIVL. Checks the data format
#' and coverage of nodes in the PKN and data. All nodes in input_data and 
#' measured data must appear in the PKN 
#' 

#TODO for all check up functions, write a wrapper (similar to carnival options checks)
checkData <- function( perturbations,
                        measurements,
                        priorKnowledgeNetwork, 
                        pathwayWeights = NULL ) {

  checkPriorKnowledgeNetwork(priorKnowledgeNetwork = priorKnowledgeNetwork)
  priorKnowledgeNetworkProcessed <- preprocessPriorKnowledgeNetwork(priorKnowledgeNetwork = priorKnowledgeNetwork)
  
  measurementsProcessed = checkMeasurements(measurements = measurements, priorKnowledgeNetwork = priorKnowledgeNetwork)
  perturbationsProcessed = checkPerturbations(perturbations = perturbations, priorKnowledgeNetwork = priorKnowledgeNetwork)
  weightsProcessed = checkWeights(weights = pathwayWeights, priorKnowledgeNetwork = priorKnowledgeNetwork)

  #TODO multiple experimental conditions are not going to be supported currently
  #if(nrow(measurements) == 1){
  experimentalConditions = "NULL"
  #} else {
      #experimental_conditions = seq_len(nrow(measObj))
  #}
  
  results <- list("priorKnowledgeNetwork" = priorKnowledgeNetworkProcessed, 
                  "measurements" = measurementsProcessed, 
                  "perturbations" = perturbationsProcessed, 
                  "weights" = weightsProcessed, 
                  #TODO multiple experimental conditions are not going to be supported currently
                  "experimental_conditions" = experimentalConditions)
  
  return(results)
  
} 

checkSolverInputs <- function(options){
  if (options$solver == supportedSolvers$cplex) {
    checkCplexCarnivalOptions(options)  
  } else {
    stop("Other solvers are not supported yet in the updated API")
  }
}


