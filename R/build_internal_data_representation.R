## This function returns the data matrix containing the data for running CARNIVAL 
## and a set of identifiers for Targets, Measured and Un-measured nodes.
##
## Enio Gjerga, 2020

buildDataVector <- function(measurements = measurements, 
                            priorKnowledgeNetwork = priorKnowledgeNetwork, 
                            perturbations = perturbations) {
    
  colnames(priorKnowledgeNetwork) <- c("X1", "X2", "X3")
  
  allSpecies <- unique( c(as.character(priorKnowledgeNetwork$X1), 
                          as.character(priorKnowledgeNetwork$X3)) )
  
  if( !is.null(perturbations) ){
    #ignore nodes in perturbations that cannot be found in priorKnowledgeNetwork
    ts <- intersect(names(perturbations), allSpecies)
  } else {
    ts <- allSpecies[allSpecies == "Perturbation"]
  }
  
  speciesMeasured <- intersect(names(measurements), allSpecies)
  speciesMeasuredNotInPkn <- setdiff(names(measurements), allSpecies)
  speciesUnmeasured <- setdiff(allSpecies, speciesMeasured)
  
  dataVector <- rep(0, length(allSpecies))
  
  dnNames <- paste0("DN:", speciesUnmeasured)
  dsNames <- paste0("DS:", speciesMeasured)
  names(dataVector) <- c(dnNames, dsNames)
  
  indices <- seq(from = length(speciesUnmeasured) + 1, to = length(allSpecies), by = 1)
  
  #save only the measurements that are present in prior knowledge network
  dataVector[indices] <- measurements[names(measurements) %in% speciesMeasured]
  
  dataVectorSign <- sign(dataVector)
  
  dnID <- seq_len(length(speciesUnmeasured))
  dsID <- seq(from = length(speciesUnmeasured) + 1, to = length(allSpecies), by = 1)
  tsID <- which(is.element(el = c(speciesUnmeasured, speciesMeasured), set = ts))
  
  result <- list(dataVector = dataVector, 
                 dataVectorSign = dataVectorSign, 
                 dnID = dnID, 
                 dsID = dsID, 
                 tsID = tsID, 
                 species = c(speciesUnmeasured, speciesMeasured))
  
  return(result)

}
