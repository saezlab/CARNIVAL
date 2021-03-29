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
  
  #species with measurements
  ds <- intersect(names(measurements), allSpecies)
  #species without measurements 
  dn <- setdiff(allSpecies, ds)
  
  dataVector <- rep(0, length(allSpecies))
  
  dnNames <- paste0("DN:", dn)
  dsNames <- paste0("DS:", ds)
  names(dataVector) <- c(dnNames, dsNames)
  
  if(length(which(is.element(el = names(measurements), 
                             set = setdiff(names(measurements), ds)))) > 0) {
    
    dataVector[seq(from = length(dn) + 1, to = length(allSpecies), by = 1)] <- 
      measurements[-which(is.element(el = names(measurements), 
                                     set = setdiff(names(measurements), ds)))]
    
  } else {
    dataVector[seq(from = length(dn) + 1, 
                   to = length(allSpecies), by = 1)] <- measurements
  }
  
  dataVectorSign <- sign(dataVector)
  
  dnID <- seq_len(length(dn))
  dsID <- seq(from = length(dn) + 1, to = length(allSpecies), by = 1)
  tsID <- which(is.element(el = c(dn, ds), set = ts))
  
  result <- list(dataVector = dataVector, 
                 dataVectorSign = dataVectorSign, 
                 dnID = dnID, dsID = dsID, 
                 tsID = tsID, 
                 species = c(dn, ds))
  
  return(result)
  
}
