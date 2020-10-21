## This function returns the data matrix containing the data for running CARNIVAL 
## and a set of identifiers for Targets, Measured and Un-measured nodes.
##
## Enio Gjerga, 2020

buildDataMatrix <- function(data = data, pknList = pknList, inputs = inputs) {

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

  }
  else{

    dataMatrix[, seq(from = length(dn)+1, 
                     to = length(allSpecies), by = 1)] <- as.matrix(data)

  }

  dataMatrixSign <- sign(dataMatrix)

  dnID <- seq_len(length(dn))
  dsID <- seq(from = length(dn)+1, to = length(allSpecies), by = 1)
  tsID <- which(is.element(el = c(dn, ds), set = ts))

  res <- list(dataMatrix=dataMatrix, dataMatrixSign=dataMatrixSign, dnID=dnID, 
              dsID=dsID, tsID=tsID, species=c(dn, ds))

  return(res)

}
