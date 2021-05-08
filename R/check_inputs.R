#' Checks the input data for correctness. 
#'
#' @param perturbations 
#' @param measurements 
#' @param priorKnowledgeNetwork 
#' @param weights 
#'
#' @return
#' @author Enio Gjerga, Olga Ivanova, Attila Gabor, 2020-2021
#'
#' @keywords internal
checkData <- function(perturbations = NULL,
                      measurements,
                      priorKnowledgeNetwork,
                      weights = NULL) {
  
  checkPriorKnowledgeNetwork(priorKnowledgeNetwork)
  
  priorKnowledgeNetworkProcessed <- preprocessPriorKnowledgeNetwork(priorKnowledgeNetwork)
  nodesPriorKnowledgeNetwork <- getPriorKnowledgeNetworkNodes(priorKnowledgeNetworkProcessed)
  
  if (is.null(perturbations)) {
    priorKnowledgeNetworkProcessed <- addPerturbationNodes(priorKnowledgeNetworkProcessed)
    message("Perturbations are not provided, all parents nodes are added as potential perturbations.")
    perturbationsProcessed <- c("Perturbation" = "NaN")
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

