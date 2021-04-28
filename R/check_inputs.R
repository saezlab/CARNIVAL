## Check input data and options for CARNIVAL. 
##
## Enio Gjerga, Olga Ivanova, Attila Gabor, 2020-2021

checkData <- function( perturbations,
                       measurements,
                       priorKnowledgeNetwork,
                       weights = NULL ) {
  
  checkPriorKnowledgeNetwork(priorKnowledgeNetwork)
  
  priorKnowledgeNetworkProcessed <- preprocessPriorKnowledgeNetwork(priorKnowledgeNetwork)
  nodesPriorKnowledgeNetwork <- getPriorKnowledgeNetworkNodes(priorKnowledgeNetworkProcessed)
  
  if (is.null(perturbations)) {
    priorKnowledgeNetworkProcessed <- addPerturbationNodes(priorKnowledgeNetworkProcessed)
    message("Perturbations are not given, all parents nodes are added as potential perturbations.")
    perturbationsProcessed <- NULL
  } else {
    perturbationsProcessed <- checkPerturbations(perturbations, 
                                                 nodesPriorKnowledgeNetwork)
  }
  
  measurementsProcessed <- checkMeasurements(measurements, 
                                             nodesPriorKnowledgeNetwork)
  
  weightsProcessed <- NULL
  if ( !is.null(weights) ) {
    weightsProcessed = checkWeights(weights, nodesPriorKnowledgeNetwork)
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
    #TODO
    #checkLpSolveCarnivalOptions(options)
    message("No checks for inputs for lpSolve needed.")
  } else if (options$solver == supportedSolvers$cbc){
    #TODO 
    #checkCbcCarnivalOptions(options)
    message("No checks for inputs for cbc needed.")
  } else {
    stop("Other solvers (except lpSolve and cplex) are not supported (yet) in the updated API.")
  }
}


