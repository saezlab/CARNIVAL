## This function returns the data matrix containing the data for running CARNIVAL 
## and a set of identifiers for Targets, Measured and Un-measured nodes.
##
## Enio Gjerga, 2020

#TODO the current code below does not produce the expected lp file. Likely, 
# it is due to vectors being passed and some weird stuff happening around vectors

# buildDataMatrix <- function(measurements = measurements, 
#                             priorKnowledgeNetwork = priorKnowledgeNetwork, 
#                             perturbations = perturbations) {
#   
#   colnames(priorKnowledgeNetwork) <- c("X1", "X2", "X3")
#   allSpecies <- unique(c(as.character(priorKnowledgeNetwork$X1), 
#                          as.character(priorKnowledgeNetwork$X3)))
#   
#   #TODO optional param for inverse carnival
#   perturbationsTs <- c()
#   if(length(perturbations) > 0){
#     perturbationsTs <- intersect(names(perturbations), allSpecies)
#   }
# 
#   ds <- intersect(names(measurements), allSpecies)
#   dn <- setdiff(allSpecies, ds)
#   
# 
#   #TODO should not be data matrix! because we now set all data inputs to vectors (except PKN)
#   dataMatrix <- matrix(0, nrow = 1, ncol = length(allSpecies))
# 
#   dnNames <- paste0("DN:", dn)
#   dsNames <- paste0("DS:", ds)
# 
#   colnames(dataMatrix) <- c(dnNames, dsNames)
#   
#   if(length(which(is.element(el = names(measurements), 
#                              set = setdiff(names(measurements), ds)))) > 0){
#   
#     dataMatrix[, seq(from = length(dn) + 1, to = length(allSpecies), by = 1)] <- 
#       as.matrix(measurements[, -which(is.element(el = names(measurements), 
#                                          set = setdiff(names(measurements), ds)))])
#   }
#   else{
# 
#     dataMatrix[, seq(from = length(dn) + 1, 
#                      to = length(allSpecies), by = 1)] <- as.matrix(measurements)
#   }
#   dataMatrixSign <- sign(dataMatrix)
# 
#   dnID <- seq_len(length(dn))
#   dsID <- seq(from = length(dn) + 1, to = length(allSpecies), by = 1)
#   
#   perturbationsTsID <- which(is.element(el = c(dn, ds), set = perturbationsTs))
# 
#   res <- list(dataMatrix = dataMatrix, 
#               dataMatrixSign = dataMatrixSign, 
#               dnID = dnID, 
#               dsID = dsID, tsID = perturbationsTsID, 
#               species = c(dn, ds))
# 
#   return(res)
# 
# }

buildDataMatrix <- function(measurements = measurements, 
                            priorKnowledgeNetwork = priorKnowledgeNetwork, 
                            perturbations = perturbations) {
  
  data <- t(as.data.frame(measurements))
  inputs <- t(as.data.frame(perturbations))
  pknList <- priorKnowledgeNetwork 
    
  colnames(pknList) <- c("X1", "X2", "X3")
  allSpecies <- unique(c(as.character(pknList$X1), as.character(pknList$X3)))
  
  if(ncol(inputs) > 0){
    ts <- intersect(colnames(inputs), allSpecies)
  }
  
  ds <- intersect(colnames(data), allSpecies)
  dn <- setdiff(allSpecies, ds)
  
  dataMatrix <- matrix(0, nrow = nrow(data), ncol = length(allSpecies))
  
  dnNames <- paste0("DN:", dn)
  dsNames <- paste0("DS:", ds)
  
  colnames(dataMatrix) <- c(dnNames, dsNames)
  
  if(length(which(is.element(el = colnames(data), 
                             set = setdiff(colnames(data), ds)))) > 0){
    
    dataMatrix[, seq(from = length(dn)+1, to = length(allSpecies), by = 1)] <- 
      as.matrix(data[, -which(is.element(el = colnames(data), 
                                         set = setdiff(colnames(data), ds)))])
    
  } else {
    dataMatrix[, seq(from = length(dn) + 1, 
                     to = length(allSpecies), by = 1)] <- as.matrix(data)
    
  }
  
  dataMatrixSign <- sign(dataMatrix)
  
  dnID <- seq_len(length(dn))
  dsID <- seq(from = length(dn) + 1, to = length(allSpecies), by = 1)
  tsID <- which(is.element(el = c(dn, ds), set = ts))
  
  res <- list(dataMatrix = dataMatrix, dataMatrixSign = dataMatrixSign, 
              dnID = dnID, dsID = dsID, tsID = tsID, 
              species = c(dn, ds))
  
  return(res)
  
}
