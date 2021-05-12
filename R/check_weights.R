## Returning error message in case of errors in the weights
##
## Enio Gjerga, Olga Ivanova 2020-2021
checkWeights <- function(weights, nodesPriorKnowledgeNetwork){
  
  nullObjectError <- "Please provide a valid weights object: a vector with identifiers and their values."
  incorrectOrderError <- "Or did you use carnivalOptions in place of weights object?"
  noWeightsInNetworkError <- "None of your weights nodes are in prior knowledge networks (PKN). 
                              Check node identifiers in both weights and PKN objects. 
                              You can use Carnival without weights (set to NULL by default)."
  incorrectValuesError <- "Error in provided weights values: should be between -1 and 1."
  
  if (is.null(weights) || !is.numeric(weights)) {
    stop(paste(nullObjectError, incorrectOrderError))
  } 
  
  names(weights) <- correctIdentifiers(names(weights))
  
  weightsNotInNetwork <- weights[!names(weights) %in% nodesPriorKnowledgeNetwork]
  weightsProcessed <- weights[names(weights) %in% nodesPriorKnowledgeNetwork] 
  
  if (length(weightsNotInNetwork) == length(weightsProcessed)) {
    stop(noWeightsInNetworkError)
  }
  
  if (!any(weights < 1 | weights > -1 )) {
    stop(incorrectValuesError)
  }
  
  if (length(weightsNotInNetwork) > 0) {
    warning("These nodes are not in prior knowledge network and will be ignored: ", 
          names(weightsNotInNetwork))    
  } 
  
  return(weightsProcessed)
}
