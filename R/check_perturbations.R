## Returning error message in case of errors in the inputs
##
## Enio Gjerga, 2020

checkPerturbations <- function(perturbations = perturbations, 
                               nodesPriorKnowledgeNetwork = nodesPriorKnowledgeNetwork) {

  nullObjectError <- "No perturbations are provided. Please, use runInversedCarnival if you have no perturbation data."
  
  #TODO leftover from invCARNIVAL, use it later. 
  #MappedPertNode <- AddPerturbationNode(network = as.matrix(netObj))
  
  noPerturbationsInNetworkError <- "None of your perturbations are in prior knowledge networks (PKN). 
                                   Check node identifiers in both perturbations and PKN objects."
  
  stopifnot(nullObjectError = !is.null(measurements))
  
  colnames(perturbations) <- correctNodeIdentifies(colnames(perturbations))
  
  perturbationsNotInNetwork <- perturbations[!names(perturbations) %in% nodesPriorKnowledgeNetwork]
  perturbationsProcessed <- perturbations[names(perturbations) %in% nodesPriorKnowledgeNetwork] 
  
  stopifnot(noPerturbationsInNetworkError = length(perturbationsNotInNetwork) != length(perturbationsProcessed))
  warning("These nodes are not in prior knowledge network and will be ignored: ", 
          names(perturbationsNotInNetwork)) 
  
  return(perturbationsProcessed)
}