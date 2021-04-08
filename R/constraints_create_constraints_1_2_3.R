## This code writes the list of constraints (1) of the ILP problem for one 
## conditions.
## 
## Enio Gjerga, Olga Ivanova 2020-2021

#TODO merge constraint 1 and 2 

createConstraints_1_2_newIntRep <- function(variables, priorKnowledgeNetwork) {
  variablesMerged <- merge(variables$edgesDf, variables$nodesDf, by.x="Node1", by.y="nodes")
  
  edgesUpActivation <- variablesMerged[variablesMerged$Sign == 1, ]
  sourceNodes <- edgesUpActivation$nodesVars
  
  constraints_1 <- createConstraint(edgesUpActivation$edgesUpVars, "-", 
                                    sourceNodes, ">=", 0)
  constraints_2 <- createConstraint(edgesUpActivation$edgesDownVars, "+", 
                                    sourceNodes, ">=", 0) 
  
  
  edgesUpInhibition <- variablesMerged[variablesMerged$Sign == -1, ]
  sourceNodes <- edgesUpInhibition$nodesVars
  
  constraints_1 <- c(constraints_1, createConstraint(edgesUpInhibition$edgesUpVars, "+", 
                                                     sourceNodes, ">=", 0))
  constraints_2 <- c(constraints_2, createConstraint(edgesUpInhibition$edgesDownVars, "-", 
                                                     sourceNodes, ">=", 0))
  
  return(list("c1"=constraints_1, "c2" = constraint_2))
}

createConstraints_3_newIntRep <- function(variables = variables) {
  constraints3 <- createConstraint(variables$edgesDf$edgesUpVars, "+", 
                                   variables$edgesDf$edgesDownVars, "<=", 1)
  
  return(constraints3)