## Returning error message in case of errors in the perturbations. 
##
## Enio Gjerga, Olga Ivanova 2020-2021
checkPerturbations <- function(perturbations, nodesPriorKnowledgeNetwork) {

  nullObjectError <- "Please provide a valid perturbations object: a vector with identifiers and their values. Please, 
                      Use runInverseCarnival() if you have no perturbation data."
  wrongOrderError <- "Or did you use unnamed carnivalOptions parameter in place of measurements?"
  noPerturbationsInNetworkError <- "None of your perturbations are in prior knowledge networks (PKN). 
                                   Check node identifiers in both perturbations and PKN objects."
  
  if (is.null(perturbations) || !is.numeric(perturbations)) {
    stop(paste(nullObjectError, wrongOrderError))
  }
  
  names(perturbations) <- correctIdentifiers(names(perturbations))
  
  perturbationsNotInNetwork <- perturbations[!names(perturbations) %in% nodesPriorKnowledgeNetwork]
  perturbationsProcessed <- perturbations[names(perturbations) %in% nodesPriorKnowledgeNetwork] 
  
  if (length(perturbationsNotInNetwork) == length(perturbationsProcessed)) {
    stop(noPerturbationsInNetworkError)
  }
  
  if ( length(perturbationsNotInNetwork) > 0 ) {
    warning("These nodes are not in prior knowledge network and will be ignored: ", 
            names(perturbationsNotInNetwork))   
  }

  return(perturbationsProcessed)
}

