## This code writes the list of constraints (5) of the ILP problem for one 
## condition.
## 
## Enio Gjerga, 2020

createConstraints_5 <- function(variables=variables) {
  
  vars <- variables$variables
  constraints1 <- rep("", length(variables$idxEdgesDown))
  
  idx1 <- which(variables$signs == 1)
  idx2 <- which(variables$signs == -1)
  
  constraints1[idx1] <- paste0(
    vars[variables$idxEdgesDown[idx1]], 
    " + ",
    vars[match(
      paste0(
        "Species ",
        unlist(
          strsplit(
              gsub(
                variables$exp[variables$idxEdgesDown[idx1]], 
                pattern = "ReactionDown ", 
                replacement = ""),  
            split = "="))[c(TRUE, FALSE)]), variables$exp)], 
    " - ",
    variables$uTable[match(
      vars[variables$idxEdgesDown[idx1]], 
      variables$uTable[, 2]), 1], " <= 0")
  
  constraints1[idx2] <- paste0(
    vars[variables$idxEdgesDown[idx2]], 
    " - ",
    vars[match(
      paste0(
        "Species ",
        unlist(
          strsplit(
              gsub(
                variables$exp[variables$idxEdgesDown[idx2]], 
                pattern = "ReactionDown ", 
                replacement = ""), 
            split = "="))[c(TRUE, FALSE)]), variables$exp)], 
    " - ",
    variables$uTable[match(
      vars[variables$idxEdgesDown[idx2]], 
      variables$uTable[, 2]), 1], " <= 0")
  
  return(constraints1)
  
}


createConstraints_5_newIntRep <- function(variables, priorKnowledgeNetwork) {
  variablesMerged <- merge(variables$edgesDf, variables$nodesDf, by.x="Node1", by.y="nodes")
  
  edgesUpActivation <- variablesMerged[variablesMerged$Sign == 1, ]
  sourceNodes <- edgesUpActivation$nodesVars
  
  constraints_5 <- createConstraintFreeForm(edgesUpActivation$edgesDownVars, "+", 
                                            sourceNodes, 
                                            "-", 
                                            edgesUpActivation$edgesUpVars,
                                            "<=", 0) 
  
  edgesUpInhibition <- variablesMerged[variablesMerged$Sign == -1, ]
  sourceNodes <- edgesUpInhibition$nodesVars
  
  constraints_5 <- c(constraints_5, createConstraintFreeForm(edgesUpInhibition$edgesDownVars, "-",
                                                             sourceNodes, 
                                                             "-", 
                                                             edgesUpInhibition$edgesUpVars,
                                                             "<=", 0))
  return(constraints_5)
}