## This function writes the constraints preventing self-activation of nodes in 
## the network due to positive feedback loops.
## 
## Enio Gjerga, Olga Ivanova 2020-2021

createLoopConstraints_v2 <- function(variables, perturbations, constraintName = "c9", 
                                     nodesType = c("perturbations" = "P", 
                                                  "measured" = "M")) {
  distanceConstant <- 100
  distanceConstantForConstraint <- 101

  variablesMergedNode1 <- merge(variables$edgesDf, variables$nodesDf, by.x="Node1", by.y="nodes") 
  variablesMergedNode2 <- merge(variablesMergedNode1, variables$nodesDf, by.x="Node2", by.y="nodes") 
  
  #TODO Keep it here, this will be used after fixing a bug when one node is both P/M - perturbation and a measurement
  #variablesMergedNode2 <- variablesMergedNode2[variablesMergedNode2$nodesType.y != nodesType['perturbations'], ]
  variablesMergedNode2 <- variablesMergedNode2[!variablesMergedNode2$Node2 %in% names(perturbations), ]
  
  cLoop <- createConstraintFreeForm(distanceConstantForConstraint, 
                                    variablesMergedNode2$edgesUpVars, "+", 
                                    variablesMergedNode2$nodesDistanceVars.x, "-",
                                    variablesMergedNode2$nodesDistanceVars.y, "<=", 
                                    distanceConstant)
  
  cLoop <- c(cLoop, createConstraintFreeForm(distanceConstantForConstraint, 
                                             variablesMergedNode2$edgesDownVars, "+", 
                                             variablesMergedNode2$nodesDistanceVars.x, "-",
                                             variablesMergedNode2$nodesDistanceVars.y, "<=", 
                                             distanceConstant))
  
  cLoop <- list(cLoop)
  names(cLoop) <- constraintName
  return(cLoop)
}