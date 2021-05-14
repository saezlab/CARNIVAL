## Extract and export the optimisation results from the solution matrix.
##
## Enio Gjerga, Olga Ivanova 2020-2021
exportIlpSolutionFromSolutionMatrix <- function(solutionMatrix, variables) {
  
  nSolutions <- ncol(solutionMatrix)
  summarisedSolution <- getWeightedCollapsedSolution(solutionMatrix, variables, nSolutions)
  
  allSolutions <- list()
  allAttributes <- list()
  
  for (i in 1:ncol(solutionMatrix)) {
    
    solMtx <- solutionMatrix[solutionMatrix[, i] > 0, i]
    namesSol <- names(solMtx)
    
    sol <- variables$edgesDf[variables$edgesDf$edgesUpVars %in% namesSol | 
                             variables$edgesDf$edgesDownVars %in% namesSol , ]
    
    #For nodes, we need to select values below 0 too.
    nodesAttributes <- solutionMatrix[, i]
    nodesAttributes <- nodesAttributes[names(nodesAttributes) %in% variables$nodesDf$nodesVars & nodesAttributes != 0]
    nodesAttributes <- as.data.frame(nodesAttributes)
    nodesAttributes <- cbind(nodesAttributes, row.names(nodesAttributes))
    names(nodesAttributes) <- c("Activity", "variables")
    
    attributes <- merge(nodesAttributes, variables$nodesDf, by.x = "variables", by.y="nodesVars")
    
    #TODO is the sign always the same in the solution as in PKN?
    sol <- sol[c("Node1", "Sign", "Node2")]
    attributes <- attributes[c("nodes", "Activity")]
    names(attributes) <- c("Nodes", "Activity")
    
    allSolutions[[i]] <- sol
    allAttributes[[i]] <- attributes
  }
  
  #TODO perturbations are handled differently? 
  #TODO divide all values to N of solutions
  nodesAttributes <- getSummaryNodesAttributes(solutionMatrix, variables, nSolutions)
  
  result <- list("weightedSIF" = summarisedSolution, 
                 "nodesAttributes" = nodesAttributes,
                 "sifAll" = allSolutions, 
                 "attributesAll" = allAttributes) 
}

getWeightedCollapsedSolution <- function(solutionMatrix, variables, nSolutions) {
  solutionMatrix <- as.data.frame(solutionMatrix)
  
  weights <- apply(solutionMatrix, 1, function(x) {
    sum(as.numeric(as.character(x)))
  })  
  
  weights <- weights[weights > 0]
  namesWeights <- names(weights)
  weights <- as.data.frame(weights)
  weights <- cbind(weights, row.names(weights))
  names(weights) <- c("Weight", "variables")
  
  edgesVars <- c(variables$edgesDf$edgesUpVars, variables$edgesDf$edgesDownVars)
  
  sol1 <- variables$edgesDf[variables$edgesDf$edgesUpVars %in% namesWeights , ]
  sol2 <- variables$edgesDf[variables$edgesDf$edgesDownVars %in% namesWeights , ]
  
  edgesUpWeights <- weights[weights$variables %in% variables$edgesDf$edgesUpVars, ]
  edgesDownWeights <- weights[weights$variables %in% variables$edgesDf$edgesDownVars, ]
  
  sol1 <- merge(sol1, edgesUpWeights, by.x = "edgesUpVars", by.y = "variables")
  sol2 <- merge(sol2, edgesDownWeights, by.x = "edgesDownVars", by.y = "variables")
  
  sol <- rbind(sol1, sol2)
  sol <- sol[c("Node1", "Sign", "Node2", "Weight")]
  
  sol$Weight <- (sol$Weight / nSolutions) * 100
  
  return(sol)
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