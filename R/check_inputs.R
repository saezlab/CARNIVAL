## Check input data and options for CARNIVAL. 
##
## Enio Gjerga, Olga Ivanova, Attila Gabor, 2020-2021

checkData <- function( measurements = measurements,
                       priorKnowledgeNetwork = priorKnowledgeNetwork, 
                       perturbations = perturbations,
                       pathwayWeights = NULL ) {

  checkPriorKnowledgeNetwork(priorKnowledgeNetwork)
  
  priorKnowledgeNetworkProcessed <- preprocessPriorKnowledgeNetwork(priorKnowledgeNetwork)
  nodesPriorKnowledgeNetwork <- getPriorKnowledgeNetworkNodes(priorKnowledgeNetworkProcessed)
  
  if (is.null(perturbations)) {
    priorKnowledgeNetworkProcessed <- addPerturbationNodes(priorKnowledgeNetworkProcessed)
    perturbationsProcessed <- NULL
  } else {
    perturbationsProcessed <- checkPerturbations(perturbations = perturbations, 
                                                 nodesPriorKnowledgeNetwork = nodesPriorKnowledgeNetwork)
  }
  
  measurementsProcessed <- checkMeasurements(measurements = measurements, 
                                             nodesPriorKnowledgeNetwork = nodesPriorKnowledgeNetwork)
  
  weightsProcessed <- NULL
  if ( !is.null(pathwayWeights) ) {
    weightsProcessed = checkWeights(weights = pathwayWeights, 
                                    nodesPriorKnowledgeNetwork = nodesPriorKnowledgeNetwork)
  } 
  
  results <- list("priorKnowledgeNetwork" = priorKnowledgeNetworkProcessed, 
                  "measurements" = measurementsProcessed, 
                  "perturbations" = perturbationsProcessed, 
                  "weights" = weightsProcessed)
  
  return(results)
  
} 

getPriorKnowledgeNetworkNodes <- function(priorKnowledgeNetwork = priorKnowledgeNetwork) {
  allNodes <- c(priorKnowledgeNetwork$Node1, priorKnowledgeNetwork$Node2)
  return( unique(allNodes) )
}

checkSolverInputs <- function(options){
  if (options$solver == supportedSolvers$cplex) {
    checkCplexCarnivalOptions(options)  
  } else if (options$solver == supportedSolvers$lpSolve) {
    message("No checks for inputs for lpSolve needed.")
  } else if (options$solver == supportedSolvers$cbc){
    message("No checks for inputs for cbc needed.")
  } else {
    stop("Other solvers (except lpSolve and cplex) are not supported (yet) in the updated API.")
  }
}


