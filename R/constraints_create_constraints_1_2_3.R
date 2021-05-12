## This code writes the list of constraints (1), (2) and (3) of the ILP problem. 
## 
## Enio Gjerga, Olga Ivanova 2020-2021

createConstraints_1_2_v2 <- function(variables, constraintName = c("c1", "c2")) {
  
  variablesMerged <- merge(variables$edgesDf, variables$nodesDf, by.x="Node1", by.y="nodes")
  constraints_1 <- c()
  constraints_2 <- c()
  
  edgesUpActivation <- variablesMerged[variablesMerged$Sign == 1, ]
  
  if(nrow(edgesUpActivation) > 0){
    sourceNodes <- edgesUpActivation$nodesVars
    constraints_1 <- c(constraints_1, createConstraint(edgesUpActivation$edgesUpVars, "-", 
                                      sourceNodes, ">=", 0))
    constraints_2 <- c(constraints_2, createConstraint(edgesUpActivation$edgesDownVars, "+", 
                                      sourceNodes, ">=", 0))
  }
  
  edgesUpInhibition <- variablesMerged[variablesMerged$Sign == -1, ]
  
  if(nrow(edgesUpInhibition) > 0){
    sourceNodes <- edgesUpInhibition$nodesVars
    constraints_1 <- c(constraints_1, createConstraint(edgesUpInhibition$edgesUpVars, "+", 
                                                       sourceNodes, ">=", 0))
    constraints_2 <- c(constraints_2, createConstraint(edgesUpInhibition$edgesDownVars, "-", 
                                                       sourceNodes, ">=", 0))
  }
  
  constraints1_2 <- list(constraints_1, constraints_2)
  names(constraints1_2) <- constraintName
  return(constraints1_2)
}

createConstraints_3_v2 <- function(variables = variables,
                                          constraintName = "c3") {
  
  constraints_3 <- createConstraint(variables$edgesDf$edgesUpVars, "+", 
                                    variables$edgesDf$edgesDownVars, "<=", 1)

  constraints_3 <- list(constraints_3)
  names(constraints_3) <- constraintName
  return(constraints_3)
}
