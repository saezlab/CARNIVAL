## Extract and export the optimisation results from the solution matrix.
##
## Enio Gjerga, Olga Ivanova 2020-2021
## fixed by A. Gabor
#' @importFrom  dplyr group_by bind_rows mutate summarise rename
exportIlpSolutionFromSolutionMatrix <- function(solutionMatrix_chr, variables) {
  
  solutionMatrix <- apply(solutionMatrix_chr,2,as.numeric)
  rownames(solutionMatrix) <- rownames(solutionMatrix_chr)
  
  nSolutions <- ncol(solutionMatrix)
  allSolutions <- list()
  allAttributes <- list()
  allCollapsedAttributes <- list()
  weightedSolution <- c()
  
  
  for (i in 1:ncol(solutionMatrix)) {
    
    collapsedAttributes <- data.frame("nodes" = variables$nodesDf$nodes, 
                                      "activityUp" = rep(0, length(variables$nodesDf$nodes)), 
                                      "activityDown" = rep(0, length(variables$nodesDf$nodes)),
                                      "zeroActivity" = rep(0, length(variables$nodesDf$nodes)))
    
    solMtx <- solutionMatrix[abs(solutionMatrix[, i]) > 0, i]
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
                                      variables$nodesDf$nodesVars, i]
    nodesActivity <- as.data.frame(nodesActivity)
    nodesActivity$nodesVars <- rownames(nodesActivity) 
    
    allNodesVariables <- merge(allNodesVariables, nodesActivity, by="nodesVars")
    
    nodesAttributes <- allNodesVariables[, c("nodes", "nodesActivity")] 
    names(nodesAttributes) <- c("Nodes", "Activity")
    
    collapsedAttributes <- merge(collapsedAttributes, nodesAttributes,by.x = "nodes",by.y = "Nodes")
    
    collapsedAttributes$zeroActivity <- as.numeric(collapsedAttributes$Activity == 0)
    collapsedAttributes$activityUp <- as.numeric(collapsedAttributes$Activity == 1)
    collapsedAttributes$activityDown <- as.numeric(collapsedAttributes$Activity == -1)
    # add node types 
    collapsedAttributes <- merge(collapsedAttributes,variables$nodesDf[,c("nodes","nodesType")],by="nodes")
    collapsedAttributes$Activity = NULL
    collapsedAttributes$solution = i
    
    allSolutions[[i]] <- solution
    allAttributes[[i]] <- nodesAttributes[abs(nodesAttributes$Activity) > 0, c("Nodes", "Activity")]
    allCollapsedAttributes[[i]] <- collapsedAttributes
  }
  
  summarisedSolution <- getWeightedCollapsedSolution(weightedSolution, 
                                                     nSolutions)
  #TODO perturbations are handled differently? 
  
  
  nodesAttributes <- getSummaryNodesAttributes(allCollapsedAttributes,
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
  
  sumWeightedSolution <- weightedSolution %>%
    dplyr::group_by(Node1, Sign, Node2) %>%
    dplyr::summarise(Weight = dplyr::n()/nSolutions*100,.groups ="drop")
  
  return(sumWeightedSolution)
}

getSummaryNodesAttributes <- function(collapsedAttributes_list, nSolutions) {
  nodesSolution = dplyr::bind_rows(collapsedAttributes_list) %>%
    
    dplyr::group_by(nodes,nodesType) %>%
    dplyr::summarise(ZeroAct = sum(zeroActivity)/nSolutions*100,
                     UpAct = sum(activityUp)/nSolutions*100,
                     DownAct = sum(activityDown)/nSolutions*100,
                     AvgAct = UpAct-DownAct,.groups ="drop" ) %>%
    dplyr::rename(Node = "nodes",
                  NodeType = "nodesType")
  
  
  return(nodesSolution)
}