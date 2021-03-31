## This code writes the list of constraints (4) of the ILP problem for one 
## condition.
## 
## Enio Gjerga, 2020

createConstraints_4 <- function(variables=variables) {
  
  vars <- variables$variables
  constraints4 <- rep("", length(variables$idxEdgesUp))
  
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)
  
  constraints4[idx1] <- paste0(
    vars[variables$idxEdgesUp[idx1]], 
    " - ",
    vars[match(
      paste0(
        "Species ",
        unlist(
          strsplit(
              gsub(
                variables$exp[variables$idxEdgesUp[idx1]], 
                pattern = "ReactionUp ", 
                replacement = ""), 
              split = "="))[c(TRUE, FALSE)]), 
      variables$exp)], 
    " - ",
    variables$uTable[match(
      vars[variables$idxEdgesUp[idx1]], 
      variables$uTable[, 1]), 2], " <= 0")
  
  constraints4[idx2] <- paste0(
    vars[variables$idxEdgesUp[idx2]], 
    " + ",
    vars[match(
      paste0(
        "Species ",
        unlist(
          strsplit(
              gsub(
                variables$exp[variables$idxEdgesUp[idx2]], 
                pattern = "ReactionUp ", 
                replacement = ""),
              split = "="))[c(TRUE, FALSE)]), 
      variables$exp)], 
    " - ",
    variables$uTable[match(
      vars[variables$idxEdgesUp[idx2]], 
      variables$uTable[, 1]), 2], " <= 0")
  
  return(constraints4)
  
}

createConstraints_4_newIntRep <- function(variables, priorKnowledgeNetwork) {
  variablesMerged <- merge(variables$edgesDf, variables$nodesDf, by.x="Node1", by.y="nodes")
  
  edgesUpActivation <- variablesMerged[variablesMerged$Sign == 1, ]
  sourceNodes <- edgesUpActivation$nodesVars
  
  constraints_4 <- createConstraintFreeForm(edgesUpActivation$edgesUpVars, "-", 
                                            sourceNodes, 
                                            "-", 
                                            edgesUpActivation$edgesDownVars,
                                            "<=", 0) 
  
  edgesUpInhibition <- variablesMerged[variablesMerged$Sign == -1, ]
  sourceNodes <- edgesUpInhibition$nodesVars
  
  constraints_4 <- c(constraints_4, createConstraintFreeForm(edgesUpInhibition$edgesUpVars, "+",
                                                              sourceNodes, 
                                                              "-", 
                                                              edgesUpInhibition$edgesDownVars,
                                                              "<=", 0))
  return(constraints_4)
}
