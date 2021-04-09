## Extract and export the optimisation results from the cplex solution file 
## (XML) as files and variables for further plotting functions - CBC
##
## Enio Gjerga, Olga Ivanova 2020-2021

exportIlpSolutionFromSolutionMatrix <- function(solutionMatrix, 
                                                variables, 
                                                dataPreprocessed) {
  
  summarisedSolution <- getWeightedCollapsedSolution(solutionMatrix, variables)
  
  allSolutions <- list()
  allAttributes <- list()
  
  for (i in 1:ncol(solutionMatrix)) {
    
    solMtx <- solutionMatrix[solutionMatrix > 0, i]
    namesSol <- names(solMtx)
    
    sol <- variables$edgesDf[variables$edgesDf$edgesUpVars %in% namesSol | 
                             variables$edgesDf$edgesDownVars %in% namesSol , ]
    
    #For nodes, we need to select values below 0 too.
    nodesAttributes <- solutionMatrix[, i]
    nodesAttributes <- nodesAttributes[names(nodesAttributes) %in% variables$nodesDf$nodesVars & nodesAttributes != 0]
    nodesAttributes <- as.data.frame(nodesAttributes)
    nodesAttributes <- cbind(nodesAttributes, row.names(nodesAttributes))
    names(nodesAttributes) <- c("activity", "variables")
    
    attributes <- merge(nodesAttributes, variables$nodesDf, by.x = "variables", by.y="nodesVars")
    
    #TODO is the sign always the same in the solution as in PKN?
    sol <- sol[c("Node1", "Sign", "Node2")]
    attributes <- attributes[c("nodes", "activity")]
    
    allSolutions[[i]] <- sol
    allAttributes[[i]] <- attributes
  }
  
  #TODO perturbations are handled differently? 
  #TODO divide all values to N of solutions
  nodesAttributes <- getSummaryNodesAttributes(solutionMatrix, variables)
  
  result <- list("weightedSIF" = summarisedSolution, 
                 "nodesAttributes" = nodesAttributes,
                 "sifAll" = allSolutions, 
                 "attributesAll" = allAttributes) 
}

getWeightedCollapsedSolution <- function(solutionMatrix, variables) {
  solutionMatrix <- as.data.frame(solutionMatrix)
  
  weights <- apply(solutionMatrix, 1, function(x) {
    sum(as.numeric(as.character(x)))
  })  
  
  weights <- weights[weights > 0]
  namesWeights <- names(weights)
  weights <- as.data.frame(weights)
  weights <- cbind(weights, row.names(weights))
  names(weights) <- c("Weight", "variables")
  
  sol1 <- variables$edgesDf[variables$edgesDf$edgesUpVars %in% namesWeights , ]
  sol2 <- variables$edgesDf[variables$edgesDf$edgesDownVars %in% namesWeights , ]
  
  edgesUpWeights <- weights[weights$variables %in% variables$edgesDf$edgesUpVars, ]
  edgesDownWeights <- weights[weights$variables %in% variables$edgesDf$edgesDownVars, ]
  
  sol1 <- merge(sol1, edgesUpWeights, by.x = "edgesUpVars", by.y = "variables")
  sol2 <- merge(sol2, edgesDownWeights, by.x = "edgesDownVars", by.y = "variables")
  
  sol <- rbind(sol1, sol2)
  sol <- sol[c("Node1", "Sign", "Node2", "Weight")]
  
  return(sol)
}

getSummaryNodesAttributes <- function(solutionMatrix, variables) {
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
  
  nodesUpSolution <- summaryNodes[names(summaryNodes) %in% variables$nodesDf$nodesUpVars]
  nodesUpSolution <- as.data.frame(nodesUpSolution)
  nodesUpSolution <- cbind(nodesUpSolution, row.names(nodesUpSolution))
  names(nodesUpSolution) <- c("Up", "Node")
  
  nodesDownSolution <- summaryNodes[names(summaryNodes) %in% variables$nodesDf$nodesDownVars]
  nodesDownSolution <- as.data.frame(nodesDownSolution)
  nodesDownSolution <- cbind(nodesDownSolution, row.names(nodesDownSolution))
  names(nodesDownSolution) <- c("Down", "Node")
  
  s1 <- merge(variables$nodesDf, nodesSolution, by.x="nodesVars", by.y="Node")
  s2 <- merge(variables$nodesDf, nodesUpSolution, by.x="nodesUpVars", by.y="Node")
  s3 <- merge(variables$nodesDf, nodesDownSolution, by.x="nodesDownVars", by.y="Node")
  
  sAttributes <- merge(s1, s2, by="nodes")
  sAttributes <- merge(sAttributes, s3)
  sAttributes <- sAttributes[c("nodes", "Activity", "Up", "Down")]
  
  return(sAttributes)
}