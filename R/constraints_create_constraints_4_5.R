## This code writes the list of constraints (4) and (5) of the ILP problem. 
## 
## Enio Gjerga, Olga Ivanova 2020-2021

createConstraints_4_5_v2 <- function(variables, constraintName = c("c4", "c5")) {
  
  variablesMerged <- merge(variables$edgesDf, variables$nodesDf, by.x="Node1", by.y="nodes")
  constraints_4 <- c()
  constraints_5 <- c()
  
  edgesUpActivation <- variablesMerged[variablesMerged$Sign == 1, ]
  edgesUpInhibition <- variablesMerged[variablesMerged$Sign == -1, ]
  
  if(nrow(edgesUpActivation) > 0){
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
  }
  
  if(nrow(edgesUpInhibition) > 0){
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
  }
  
  constraint4_5 <- list("c4" = constraints_4, "c5" = constraints_5)
  names(constraint4_5) <- constraintName
  return(constraint4_5)
}

