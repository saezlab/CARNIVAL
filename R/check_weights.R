## Returning error message in case of errors in the weights
##
## Enio Gjerga, Olga Ivanova 2020-2021

checkWeights <- function(weights = weights, 
                         nodesPriorKnowledgeNetwork = nodesPriorKnowledgeNetwork){
  
  nullObjectError <- "Please provide a valid weights object."
  noWeightsInNetworkError <- "None of your weights nodes are in prior knowledge networks (PKN). 
                              Check node identifiers in both weights and PKN objects. 
                              You can use Carnival without weights (set to NULL by default)."
  incorrectValuesError <- "Error in provided weights values: should be between -1 and 1."
  
  stopifnot(nullObjectError = !is.null(weights))
   
  colnames(weights) <- correctNodeIdentifies(colnames(weights))
  
  weightsNotInNetwork <- weights[!names(weights) %in% nodesPriorKnowledgeNetwork]
  weightsProcessed <- weights[names(weights) %in% nodesPriorKnowledgeNetwork] 
  
  stopifnot(noMeasurementsInNetworkError = length(weightsNotInNetwork) != length(weightsProcessed))
  stopifnot(incorrectValuesError = any(weights < 1 | weights >-1 ))
  
  warning("These weight nodes are not in prior knowledge network and will be ignored: ", 
          names(weightsNotInNetwork)) 
  
  return(weightsProcessed)
}
