## Extract and export the optimisation results from the solution matrix.
##
## Enio Gjerga, Olga Ivanova 2020-2021
exportIlpSolutionFromSolutionMatrix <- function(solutionMatrix, variables) {
  
  nSolutions <- ncol(solutionMatrix)
  
  allSolutions <- list()
  allAttributes <- list()
  weightedSolution <- c()
  collapsedAttributes <- data.frame("nodes" = variables$nodesDf$nodes, 
                                    "activityUp" = rep(0, length(variables$nodesDf$nodes)), 
                                    "activityDown" = rep(0, length(variables$nodesDf$nodes)),
                                    "zeroActivity" = rep(0, length(variables$nodesDf$nodes)))
  
  print(solutionMatrix)
  for (i in 1:ncol(solutionMatrix)) {
    
    solMtx <- solutionMatrix[solutionMatrix[, i] > 0, i]
    namesSol <- names(solMtx)
    solMtx <- round(as.numeric(solMtx))
    
    solutionEdgesUp <- variables$edgesDf[variables$edgesDf$edgesUpVars %in% namesSol, ]
    solutionEdgesDown <-  variables$edgesDf[variables$edgesDf$edgesDownVars %in% namesSol, ]
  
    nodesUp <- variables$nodesDf[variables$nodesDf$nodesUpVars %in% namesSol, ]
    nodesDown <- variables$nodesDf[variables$nodesDf$nodesDownVars %in% namesSol, ]
    
    #Nodes activity should be in line with incoming edges  
    solutionEdgesUp <- solutionEdgesUp[solutionEdgesUp$Node2 %in% nodesUp$nodes, ]
    solutionEdgesDown <- solutionEdgesDown[solutionEdgesDown$Node2 %in% nodesDown$nodes, ]

    solution <- rbind(solutionEdgesUp, solutionEdgesDown)
    solution <- solution[, c("Node1", "Sign", "Node2")]
    
    #collect collapsed solution
    weightedSolution <- rbind(weightedSolution, solution)
    
    #For nodes, we need to select values below 0 too.
    allNodesVariables <- variables$nodesDf[, c("nodes", "nodesVars")] 
    nodesActivity <- solutionMatrix[rownames(solutionMatrix) %in% 
                                       variables$nodesDf$nodesVars, ]
    nodesActivity <- as.data.frame(nodesActivity)
    nodesActivity$nodesVars <- rownames(nodesActivity) 
    
    allNodesVariables <- merge(allNodesVariables, nodesActivity, by="nodesVars")
    
    nodesAttributes <- allNodesVariables[, c("nodes", "nodesActivity")] 
    names(nodesAttributes) <- c("Nodes", "Activity")
    
    collapsedAttributes$zeroActivity <- as.numeric(nodesAttributes$Activity == 0)
    collapsedAttributes$activityUp <- as.numeric(nodesAttributes$Activity == 1)
    collapsedAttributes$activityDown <- as.numeric(nodesAttributes$Activity == -1)
    collapsedAttributes$nodesType <- variables$nodesDf$nodesType
    
    allSolutions[[i]] <- solution
    allAttributes[[i]] <- nodesAttributes[nodesAttributes$Activity > 0, c("Nodes", "Activity")]
  }
  
  summarisedSolution <- getWeightedCollapsedSolution(weightedSolution, 
                                                     nSolutions)
  #TODO perturbations are handled differently? 
  nodesAttributes <- getSummaryNodesAttributes(collapsedAttributes,
                                               nSolutions)
  if (nrow(summarisedSolution) != 0) {
    result <- list("weightedSIF" = summarisedSolution, 
                   "nodesAttributes" = nodesAttributes,
                   "sifAll" = allSolutions, 
                   "attributesAll" = allAttributes) 
  } else {
    result <- NULL
    message(getTime(), " No consistent solutions exist.")
  }
  
  return(result)
}

getWeightedCollapsedSolution <- function(weightedSolution, nSolutions) {
  weightedSolution <- count(weightedSolution)
  names(weightedSolution) <- c("Node1", "Sign", "Node2", "Weight")
  weightedSolution$Weight <- ( weightedSolution$Weight / nSolutions ) * 100
 
  return(weightedSolution)
}

getSummaryNodesAttributes <- function(collapsedAttributes, nSolutions) {
  nodesSolution <- data.frame("Node" = collapsedAttributes$nodes, 
                              "NodeType" = collapsedAttributes$nodesType)
  
  nodesSolution$ZeroAct <- ( collapsedAttributes$zeroActivity / nSolutions ) * 100
  nodesSolution$UpAct <- ( collapsedAttributes$activityUp / nSolutions ) * 100
  nodesSolution$DownAct <- ( collapsedAttributes$activityDown / nSolutions ) * 100
  nodesSolution$AvgAct <- nodesSolution$UpAct - nodesSolution$DownAct
  nodesSolution <- nodesSolution[, c("Node", "ZeroAct", "UpAct", "DownAct", 
                                     "AvgAct", "NodeType")]
  
  return(nodesSolution)
}
