## This function writes the constraints preventing self-activation of nodes in 
## the network due to positive feedback loops.
## 
## Enio Gjerga, Olga Ivanova 2020-2021

createLoopConstraints_newIntRep <- function(variables, constraintName = "c9") {
  distanceConstrant <- 100
  distanceConstrantForConstraint <- 101

  variablesMergedNode1 <- merge(variables$edgesDf, variables$nodesDf, by.x="Node1", by.y="nodes") 
  variablesMergedNode2 <- merge(variablesMergedNode1, variables$nodesDf, by.x="Node2", by.y="nodes") 
  
  cLoop <- createConstraintFreeForm(distanceConstrantForConstraint, 
                                    variablesMergedNode2$edgesUpVars, "+", 
                                    variablesMergedNode2$nodesDistanceVars.x, "-",
                                    variablesMergedNode2$nodesDistanceVars.y, "<=", 
                                    distanceConstrant)
  
  cLoop <- c(cLoop, createConstraintFreeForm(distanceConstrantForConstraint, 
                                             variablesMergedNode2$edgesDownVars, "+", 
                                             variablesMergedNode2$nodesDistanceVars.x, "-",
                                             variablesMergedNode2$nodesDistanceVars.y, "<=", 
                                             distanceConstrant))
  
  cLoop <- list(cLoop)
  names(cLoop) <- constraintName
  return(cLoop)
}