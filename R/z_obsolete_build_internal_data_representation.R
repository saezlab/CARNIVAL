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


createVariables <- function(priorKnowledgeNetwork = priorKnowledgeNetwork, 
                            dataVector = dataVector){
  
  colnames(priorKnowledgeNetwork) <- c("X1", "X2", "X3")
  
  dataLength <- length(dataVector$dataVector)
  nrowsPkn <- nrow(priorKnowledgeNetwork)
  
  nodes <- paste0("xb", seq_len( dataLength ))
  
  nodesUp <- paste0("xb", seq(from = dataLength + 1, 
                              to = 2 * dataLength, 
                              by = 1))
  
  nodesDown <- paste0("xb", seq(from = 2 * dataLength + 1, 
                                to = 3 * dataLength, by = 1))
  
  expNodes <- paste0("Species ", dataVector$species)
  expNodesReduced = paste0("Species ", dataVector$species)
  expNodesUp <- paste0("SpeciesUP ", dataVector$species)
  expNodesDown <- paste0("SpeciesDown ", dataVector$species)
  
  expNodesReducedUpSource <- as.character(priorKnowledgeNetwork$X1)
  expNodesReducedDownSource <- as.character(priorKnowledgeNetwork$X1)
  
  expNodesReducedUpTarget <- as.character(priorKnowledgeNetwork$X3)
  expNodesReducedDownTarget <- as.character(priorKnowledgeNetwork$X3)
  
  idxExperimentNodes <- length(expNodes)
  
  nodesALL <- c(nodes, nodesUp, nodesDown)
  expNodesALL <- c(expNodes, expNodesUp, expNodesDown)
  
  idxNodes <- seq_len(dataLength)
  idxNodesUp <- seq(from = dataLength + 1,
                    to = 2 * dataLength, by = 1)
  idxNodesDown <- seq(from = 2 * dataLength + 1, 
                      to = 3 * dataLength, by = 1)
  
  # edges
  edgesUp <- paste0("xb", seq(from = length(nodesALL) + 1, 
                              to = length(nodesALL) +
                                nrowsPkn *
                                dataLength, by = 1))
  
  edgesDown <- paste0("xb", seq(from = length(nodesALL) +
                                  nrowsPkn *
                                  dataLength + 1, 
                                to = length(nodesALL) +
                                  2 * nrowsPkn *
                                  dataLength, by = 1))
  
  expEdgesUp <- paste0("ReactionUp ", as.character(priorKnowledgeNetwork$X1), "=", 
                       as.character(priorKnowledgeNetwork$X3))
  
  expEdgesDown <- paste0("ReactionDown ", as.character(priorKnowledgeNetwork$X1), "=", 
                         as.character(priorKnowledgeNetwork$X3))
  
  expEdgesReducedSource <- paste0("ReactionSource ", 
                                  as.character(priorKnowledgeNetwork$X1))
  
  expEdgesReducedTarget <- paste0("ReactionTarget ", 
                                  as.character(priorKnowledgeNetwork$X3))
  
  expNodesReducedUp <- priorKnowledgeNetwork$X1
  expNodesReducedDown <- priorKnowledgeNetwork$X3
  
  idxExperimentEdges <- length(expEdgesUp)
  
  edgesALL <- c(edgesUp, edgesDown)
  expEdgesALL <- c(expEdgesUp, expEdgesDown)
  
  idxEdgesUp <- seq(from = length(nodesALL) + 1, 
                    to = length(nodesALL) + nrowsPkn, 
                    by = 1)
  
  idxEdgesDown <- seq(from = length(nodesALL) + nrowsPkn + 1, 
                      to = length(nodesALL) + 2 * nrowsPkn, 
                      by = 1)
  
  signs <- priorKnowledgeNetwork$X2
  reactionSource <- as.character(priorKnowledgeNetwork$X1)
  reactionTarget <- as.character(priorKnowledgeNetwork$X3)
  
  #Introducing distance variables
  dist <- paste0("dist_", sapply(strsplit(expNodesALL[idxNodes], 
                                          split = " "), "[[", 2))
  
  distExp <- paste0("Distance ", sapply(strsplit(expNodesALL[idxNodes],
                                                 split = " "), "[[", 2))
  
  #Introducing B variables
  varB <- paste0("B_", sapply(strsplit(expNodes, split = " "),
                              function(x) x[2]))
  
  expVarB <- paste0("B variable for ", sapply(strsplit(expNodes, split = " "),
                                              function(x) x[2]))
  
  
  # Matching table for u variables
  uTable <- matrix(data = , nrow = length(idxEdgesUp), ncol = 2)
  uTable[, 1] <- c(nodesALL, edgesALL, varB, dist)[idxEdgesUp]
  uTable[, 2] <- c(nodesALL, edgesALL, varB, dist)[idxEdgesDown]
  
  # output
  res <- list(variables=c(nodesALL, edgesALL, varB, dist), 
              exp=c(expNodesALL, expEdgesALL, expVarB, distExp), 
              idxNodes=idxNodes, idxNodesUp=idxNodesUp,
              idxNodesDown=idxNodesDown, idxEdgesUp=idxEdgesUp, 
              idxEdgesDown=idxEdgesDown, signs=signs,
              reactionSource=reactionSource, reactionTarget=reactionTarget, 
              expNodesReduced=expNodesReduced,
              expNodesReducedUpSource=expNodesReducedUpSource, 
              expNodesReducedDownSource=expNodesReducedDownSource,
              expNodesReducedDownTarget=expNodesReducedDownTarget, 
              expNodesReducedUpTarget=expNodesReducedUpTarget,
              expEdgesReducedSource=expEdgesReducedSource, 
              expEdgesReducedTarget=expEdgesReducedTarget,
              idxExperimentNodes=idxExperimentNodes, 
              idxExperimentEdges=idxExperimentEdges,
              expNodesReducedUp=expNodesReducedUp, 
              expNodesReducedDown=expNodesReducedDown, 
              
              idxB = seq(from = length(c(nodesALL, edgesALL)) + 1, 
                         to = length(c(nodesALL, edgesALL)) + length(varB), 
                         by = 1),
              
              idxDist = seq(from = length(c(nodesALL, edgesALL)) +
                              length(varB) + 1, 
                            to = length(c(nodesALL, edgesALL)) +
                              length(varB) + length(dist), 
                            by = 1), 
              
              uTable = uTable)
  
  return(res)
}