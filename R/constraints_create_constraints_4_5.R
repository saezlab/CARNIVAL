
createConstraints_4_5_newIntRep <- function(variables, priorKnowledgeNetwork) {
  variablesMerged <- merge(variables$edgesDf, variables$nodesDf, by.x="Node1", by.y="nodes")
  
  edgesUpActivation <- variablesMerged[variablesMerged$Sign == 1, ]
  sourceNodes <- edgesUpActivation$nodesVars
  
  constraints_4 <- createConstraintFreeForm(edgesUpActivation$edgesUpVars, "-", 
                                            sourceNodes, 
                                            "-", 
                                            edgesUpActivation$edgesDownVars,
                                            "<=", 0) 
  
  constraints_5 <- createConstraintFreeForm(edgesUpActivation$edgesDownVars, "+", 
                                            sourceNodes, 
                                            "-", 
                                            edgesUpActivation$edgesUpVars,
                                            "<=", 0) 
  
  
  edgesUpInhibition <- variablesMerged[variablesMerged$Sign == -1, ]
  sourceNodes <- edgesUpInhibition$nodesVars
  
  constraints_4 <- c(constraints_4, createConstraintFreeForm(edgesUpInhibition$edgesUpVars, "+",
                                                             sourceNodes, 
                                                             "-", 
                                                             edgesUpInhibition$edgesDownVars,
                                                             "<=", 0))
  
  constraints_5 <- c(constraints_5, createConstraintFreeForm(edgesUpInhibition$edgesDownVars, "-",
                                                             sourceNodes, 
                                                             "-", 
                                                             edgesUpInhibition$edgesUpVars,
                                                             "<=", 0))
  return(list("c4"=constraints_4, "c5"=constraints_5))
}

