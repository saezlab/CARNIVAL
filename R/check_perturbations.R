## Returning error message in case of errors in the inputs
##
## Enio Gjerga, 2020

checkPerturbations <- function(perturbations
                               nodesPriorKnowledgeNetwork) {

  nullObjectError <- "No perturbations are provided. Please, use runInversedCarnival if you have no perturbation data."
  
  noPerturbationsInNetworkError <- "None of your perturbations are in prior knowledge networks (PKN). 
                                   Check node identifiers in both perturbations and PKN objects."
  
  stopifnot(nullObjectError = !is.null(perturbations))
  
  names(perturbations) <- correctIdentifiers(names(perturbations))
  
  perturbationsNotInNetwork <- perturbations[!names(perturbations) %in% nodesPriorKnowledgeNetwork]
  perturbationsProcessed <- perturbations[names(perturbations) %in% nodesPriorKnowledgeNetwork] 
  
  stopifnot(noPerturbationsInNetworkError = length(perturbationsNotInNetwork) != length(perturbationsProcessed))
  
  if ( length(perturbationsNotInNetwork) > 0 ) {
    warning("These nodes are not in prior knowledge network and will be ignored: ", 
            names(perturbationsNotInNetwork))   
  }

  return(perturbationsProcessed)
}

