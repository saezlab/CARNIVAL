## This function returns the identifiers of all the variables used in the ILP 
## formulation.
##
## Olga Ivanova 2021

createVariablesForIlpProblem <- function(dataProcessed) {
  message(getTime(), " Generating variables for lp problem")
  
  perturbations <- dataProcessed$perturbations
  measurements <- dataProcessed$measurements
  priorKnowledgeNetwork <- dataProcessed$priorKnowledgeNetwork
  
  nodesDf <- createNodesVariables(priorKnowledgeNetwork, perturbations, measurements)
  edgesDf <- createEdgesVariables(priorKnowledgeNetwork) 
  measurementsDf <- createMeasurementsVariables(measurements, nodesDf, priorKnowledgeNetwork)
 
  message(getTime(), " Done: generating variables for lp problem")
  return(list("nodesDf" = nodesDf, "edgesDf" = edgesDf, 
              "measurementsDf" = measurementsDf))
}

createNodesVariables <- function(priorKnowledgeNetwork, 
                                 perturbations, measurements, 
                                 backwardCompatibility=F,
                                 prefixes=c("nodes" = "n", "nodesUp" = "nU", 
                                            "nodesDown" = "nD", 
                                            "nodesActivationState" = "nAc",
                                            "nodesDistance" = "nDs")) {
  nodes <- unique(c(priorKnowledgeNetwork$Node1, priorKnowledgeNetwork$Node2))
  
  if (backwardCompatibility) {
    #this will generate the variable names that were previously used in the ILP file (CARNIVAL v1)
    nodesPrefix <- "xb"
    nodesActivationStatePrefix <- "B"
    nodesDistancePrefix <- "dist"
      
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
  
  nodesDf$nodesType <- ""
  nodesDf[nodesDf$nodes %in% names(perturbations), "nodesType"] <- "P"
  nodesDf[nodesDf$nodes %in% names(measurements), "nodesType"] <- "M"
  
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

