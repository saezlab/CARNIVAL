## Check input data and options for CARNIVAL. 
##
## Enio Gjerga, Olga Ivanova, Attila Gabor, 2020-2021

#TODO for all check up functions, write a wrapper (similar to carnival options checks)
checkData <- function( perturbations = perturbations,
                       measurements = measurements,
                       priorKnowledgeNetwork = priorKnowledgeNetwork, 
                       pathwayWeights = NULL ) {

  checkPriorKnowledgeNetwork(priorKnowledgeNetwork = priorKnowledgeNetwork)
  priorKnowledgeNetworkProcessed <- preprocessPriorKnowledgeNetwork(priorKnowledgeNetwork = priorKnowledgeNetwork)
  
  nodesPriorKnowledgeNetwork <- getPriorKnowledgeNetworkNodes(priorKnowledgeNetwork = priorKnowledgeNetworkProcessed)
  
  measurementsProcessed <- checkMeasurements(measurements = measurements, 
                                            nodesPriorKnowledgeNetwork = nodesPriorKnowledgeNetwork)
  
  perturbationsProcessed <- checkPerturbations(perturbations = perturbations, 
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
  } else {
    stop("Other solvers are not supported (yet) in the updated API")
  }
}


