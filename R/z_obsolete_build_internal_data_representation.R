## Obsolete code kept here for backward compatibility for v.2.1. Planned to be removed in v.3
## Functions to build internal data representation used in CARNIVAL v.1.2
## The code was refactored but has the same logic as in v.1.2. 
#TODO 
## Changes: ...

## Enio Gjerga, Olga Ivanova 2020-2021

## Return the data matrix containing the data for running CARNIVAL 
## and a set of identifiers for targets, measured and unmeasured nodes.
buildDataVector <- function(measurements, priorKnowledgeNetwork, perturbations) {
  .Deprecated("createVariablesForIlpProblem")  
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

## Returns the identifiers of all the variables used in the ILP 
## formulation together with an explanation about each of them. 
## Full explanations:
# variables - list of all variables for ILP problem 
# exp - verbose explanation of each variable 
# idxNodes - indices of variables corresponding to nodes in full variables list
# idxNodesUp/idxNodesDown - indices of variables corresponding to nodes up/down-regulated in full variables list
# idxEdgesUp/idxEdgesDown - indices of variables corresponding to edges up/down-regulated in full variables list
# signs - signs of the edges provided in prior knowledge network
# reactionSource/reactionTarget - nodes names from prior knowledge network with an outgoing/incoming edge
# expNodesReduced - full list of nodes names in prior knowledge network
# expNodesReducedUpSource/expNodesReducedDownSource - source nodes names from prior knowledge network 
# expNodesReducedUpTarget/expNodesReducedDownTarget - target nodes names from prior knowledge network 
# expEdgesReducedSource/expEdgesReducedTarget - list of sources/targets from $exp with verbose explnation
# expNodesReducedUp/expNodesReducedDown - all nodes names 
# idxB - indices of variables corresponding to B variables (see constraint 8 in Liu et al.) in full variables list
# idxDist - indices of variables corresponding to distance variables in full variables list
# uTable - prior knowledge network nodes translated to variables. Column 1 - source nodes; column 2 - target nodes

# "Reduced" in variable names corresponded to measurements information combined with 
# prior knowledge network, where nodes that are not present in prior knowledge network were removed
# from the measurements data. Basically, it is always the same list of nodes as in PKN.
# See buildDataVector(...) dataVector for information. 
createVariables <- function(priorKnowledgeNetwork, dataVector, postfix="_1"){
  .Deprecated("createVariablesForIlpProblem")  
  colnames(priorKnowledgeNetwork) <- c("X1", "X2", "X3")
  
  dataLength <- length(dataVector$dataVector)
  nrowsPkn <- nrow(priorKnowledgeNetwork)
  
  nodes <- paste0("xb", seq_len( dataLength ), postfix)
  
  nodesUp <- paste0("xb", seq(from = dataLength + 1, 
                              to = 2 * dataLength, 
                              by = 1), 
                    postfix)
  
  nodesDown <- paste0("xb", seq(from = 2 * dataLength + 1, 
                                to = 3 * dataLength, by = 1),
                      postfix)
  
  expNodes <- paste0("Species ", dataVector$species)
  expNodesReduced = paste0("Species ", dataVector$species)
  expNodesUp <- paste0("SpeciesUP ", dataVector$species)
  expNodesDown <- paste0("SpeciesDown ", dataVector$species)
  
  expNodesReducedUpSource <- as.character(priorKnowledgeNetwork$X1)
  expNodesReducedDownSource <- as.character(priorKnowledgeNetwork$X1)
  
  expNodesReducedUpTarget <- as.character(priorKnowledgeNetwork$X3)
  expNodesReducedDownTarget <- as.character(priorKnowledgeNetwork$X3)
  
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
                                nrowsPkn, by = 1), 
                    postfix)
  
  edgesDown <- paste0("xb", seq(from = length(nodesALL) +
                                  nrowsPkn + 1, 
                                to = length(nodesALL) +
                                  2 * nrowsPkn, by = 1),
                      postfix)
  
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
                              function(x) x[2]), postfix)
  
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
