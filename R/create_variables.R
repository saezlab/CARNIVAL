## This function returns the identifiers of all the variables used in the ILP 
## formulation together with an explanation about the meaning of each of them. 
## Also it returns a list of useful identifiers.
##
## Enio Gjerga, 2020

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

createVariablesForIlpProblem <- function(dataProcessed) {
  perturbations <- dataProcessed$perturbations
  measurements <- dataProcessed$measurements
  priorKnowledgeNetwork <- dataProcessed$priorKnowledgeNetwork
  
  nodesDf <- createNodesVariables(priorKnowledgeNetwork)
  edgesDf <- createEdgesVariables(priorKnowledgeNetwork) 
  measurementsDf <- createMeasurementsVariables(measurements, nodesDf, priorKnowledgeNetwork)
 
  return(list("nodesDf" = nodesDf, "edgesDf" = edgesDf, 
              "measurementsDf" = measurementsDf))
}

createNodesVariables <- function(priorKnowledgeNetwork, backwardCompatibility=F,
                                 prefixes=c("nodes" = "n", "nodesUp" = "nU", 
                                            "nodesDown" = "nD", 
                                            "nodesActivationState" = "nAc",
                                            "nodesDistance" = "nDs")) {
  nodes <- unique(c(priorKnowledgeNetwork$Node1, priorKnowledgeNetwork$Node2))

  if (backwardCompatibility) {
    #this will generate the variable names that were previously used in the ILP file (CARNIVAL v1)
    nodesPrefix <- "xb"
    idxNodes <- seq(from = 1, to = 3 * length(nodes), by = 1)
    nodesVars <- paste0(nodesPrefix, idxNodes[1 : length(nodes)], "_1")
    nodesUpVars <- paste0(nodesPrefix, idxNodes[(length(nodes) + 1) : ( 2 * length(nodes))], "_1")
    nodesDownVars <- paste0(nodesPrefix, idxNodes[(2 * length(nodes) + 1) : (3 * length(nodes))], "_1")
    
    nodesActStateVars <- paste0(nodesActivationStatePrefix, "_", nodes)
    nodesDistanceVars <- paste0(nodesDistancePrefix, "_", nodes)
  } else {
    idxNodes <- seq(from = 1, to = length(nodes), by = 1)
    nodesVars <- paste0(prefixes['nodes'], idxNodes)
    nodesUpVars <- paste0(prefixes['nodesUp'], idxNodes)
    nodesDownVars <- paste0(prefixes['nodesDown'], idxNodes)
    
    nodesActStateVars <- paste0(prefixes['nodesActivationState'], idxNodes)
    nodesDistanceVars <- paste0(prefixes['nodesDistance'], idxNodes)
  }
 
  nodesDf <- cbind(nodes, nodesVars, nodesUpVars, nodesDownVars, 
                   nodesActStateVars, nodesDistanceVars)
  nodesDf <- as.data.frame(nodesDf)
  
  return(nodesDf)
}

createEdgesVariables <- function(priorKnowledgeNetwork, prefixes = c("edgeUp" = "eU", 
                                                                     "edgeDown" = "eD"),
                                 backwardCompatibility = F,
                                 startingIdx = 1) {
  
  if(backwardCompatibility) {
    #this will generate the variable names that were previously used in the ILP file (CARNIVAL v1)
    idxEdges <- seq(from = startingIdx, to = startingIdx + 2 * length(priorKnowledgeNetwork$Node1))
    edgesUpVars <- paste0(edgesPrefixes, idxEdges[1 : length(priorKnowledgeNetwork$Node1)], "_1")
    edgesDownVars <- paste0(edgesPrefixes, idxEdges[(length(priorKnowledgeNetwork$Node1) + 1) : 
                                                      (2 * length(priorKnowledgeNetwork$Node1))], "_1")
  } else {
    idxEdges <- seq(from = 1, to = length(priorKnowledgeNetwork$Node1), by = 1)
    edgesUpVars <- paste0(prefixes["edgeUp"], idxEdges)
    edgesDownVars <- paste0(prefixes["edgeDown"], idxEdges)  
  }

  edgesDf <- cbind(priorKnowledgeNetwork, edgesUpVars, edgesDownVars)
  return(edgesDf)
}

#TODO no backward compatibility (yet)
createMeasurementsVariables <- function(measurements, 
                                        nodesDf, 
                                        priorKnowledgeNetwork,
                                        prefixes = c("aD")) {
  
  nodes <- c(priorKnowledgeNetwork$Node1, priorKnowledgeNetwork$Node2)
  measurements <- measurements[names(measurements) %in% nodes]
  
  idxNodes <- seq(from = 1, to = length(measurements), by = 1)
  #measurementsAbsDifferencePrefix <- "aD"
  measurementsAbsDifferencePrefix <- "absDiff"
  
  measurementsVars <- paste0(measurementsAbsDifferencePrefix, idxNodes)
  measurementsVars <- cbind("nodes" = names(measurements), 
                            "value" = measurements, 
                            measurementsVars)
  
  measurementsVars <- as.data.frame(measurementsVars)
  
  #add nodes variables for convenience in constraint generation
  nodesVars <- nodesDf[nodesDf$nodes %in% measurementsVars$nodes, 
                       c('nodes', 'nodesVars')]
  
  measurementsVars <- merge(measurementsVars, nodesVars)
  
  return(measurementsVars)
}

