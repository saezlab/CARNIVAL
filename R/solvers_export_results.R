## Extract and export the optimisation results from the solution matrix.
##
## Enio Gjerga, Olga Ivanova 2020-2021
exportIlpSolutionFromSolutionMatrix <- function(solutionMatrix, variables) {
  
  nSolutions <- ncol(solutionMatrix)
 
  allSolutions <- list()
  allAttributes <- list()
  weightedSolution <- c()
  
  for (i in 1:ncol(solutionMatrix)) {
    
    solMtx <- solutionMatrix[solutionMatrix[, i] > 0, i]
    namesSol <- names(solMtx)
    
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
    nodesSolution <- unique(c(solution$Node1, solution$Node2))
    nodesSolution <- variables$nodesDf[variables$nodesDf$nodes %in% nodesSolution, ]
    nodesAttributes <- solutionMatrix[, i]
    nodesAttributes <- nodesAttributes[names(nodesAttributes) %in% nodesSolution$nodesVars &
                                         nodesAttributes != 0]
    nodesAttributes <- as.data.frame(nodesAttributes)
    nodesAttributes <- cbind(nodesAttributes, row.names(nodesAttributes))
    names(nodesAttributes) <- c("Activity", "variables")
    
    attributes <- merge(nodesAttributes, variables$nodesDf, by.x = "variables", 
                        by.y = "nodesVars")
    
    attributes <- attributes[c("nodes", "Activity")]
    names(attributes) <- c("Nodes", "Activity")
    
    allSolutions[[i]] <- solution
    allAttributes[[i]] <- attributes
  }
  
  summarisedSolution <- getWeightedCollapsedSolution(weightedSolution, 
                                                     length(allSolutions))
  
  #TODO perturbations are handled differently? 
  nodesAttributes <- getSummaryNodesAttributes(solutionMatrix, variables, nSolutions)
  
  result <- list("weightedSIF" = summarisedSolution, 
                 "nodesAttributes" = nodesAttributes,
                 "sifAll" = allSolutions, 
                 "attributesAll" = allAttributes) 
}

getWeightedCollapsedSolution <- function(weightedSolution, nSolutions) {
  weightedSolution <- count(weightedSolution)
  names(weightedSolution) <- c("Node1", "Sign", "Node2", "Weight")
  weightedSolution$Weight <- ( weightedSolution$Weight / nSolutions ) * 100
 
  return(weightedSolution)
}

getSummaryNodesAttributes <- function(solutionMatrix, variables, nSolutions) {
  solutionMatrix <- as.data.frame(solutionMatrix)
  
  nodesVars <- c(variables$nodesDf$nodesVars, variables$nodesDf$nodesUpVars, 
                 variables$nodesDf$nodesDownVars)
  
  namesSol <- rownames(solutionMatrix)
  nodesSolutions <- solutionMatrix[namesSol %in% nodesVars, ]
  nodesSolutions <- t(nodesSolutions)
  nodesSolutions <- as.data.frame(nodesSolutions)
  colnames(nodesSolutions) <- namesSol[namesSol %in% nodesVars]
  
  summaryNodes <- apply(nodesSolutions, 2, function(x) {sum(as.numeric(x))})
  
  nodesSolution <- summaryNodes[names(summaryNodes) %in% variables$nodesDf$nodesVars]
  nodesSolution <- as.data.frame(nodesSolution)
  nodesSolution <- cbind(nodesSolution, row.names(nodesSolution))
  names(nodesSolution) <- c("Activity", "Node")
  
  nodesSolution$ZeroAct <- ( as.numeric(!nodesSolution$Activity) / nSolutions ) * 100
  nodesSolution$UpAct <- ( as.numeric(nodesSolution$Activity == 1) / nSolutions ) * 100
  nodesSolution$DownAct <- ( as.numeric(nodesSolution$Activity == -1) / nSolutions ) * 100
  nodesSolution$AvgAct <- nodesSolution$UpAct - nodesSolution$DownAct
  
  nodesSolution <- merge(variables$nodesDf, nodesSolution, by.x = "nodesVars", by.y = "Node")
  nodesSolution <- nodesSolution[, c("nodes", "ZeroAct", "UpAct", "DownAct", "AvgAct", "nodesType")]
  names(nodesSolution)[1] <- "Node" 
  
  return(nodesSolution)
}