createConstraints_8_newIntRep <- function(variables, perturbations, constraintName) {
  constraint_8 <- c()
  
  # "Sync" all predicted states values with perturbation data 
  lapply(variables$nodesDf$nodes, function(x) {
    row <-  variables$nodesDf[variables$nodesDf$nodes == x, ]
    constraint_8 <<- c(constraint_8, createConstraintFreeForm(row$nodesUpVars, "-",
                                                              row$nodesDownVars, "+",
                                                              row$nodesActStateVars, "-",
                                                              row$nodesVars,
                                                              "=", 0))
  })
  
  # Defines activations state for unperturbed nodes
  unperturbedNodes <- variables$nodesDf[!variables$nodesDf$nodes %in% names(perturbations), ]
  lapply(unperturbedNodes$nodes, function(x) {
    
    var <- variables$nodesDf[variables$nodesDf$nodes == x, ]$nodesActStateVars
    constraint_8 <<- c(constraint_8, createConstraintFreeForm(var, "=", 0))
  })
  
  # Defines activation state for perturbation nodes
  lapply(names(perturbations), function(x) {
    var <- variables$nodesDf[variables$nodesDf$nodes == x, ]$nodesVars
    constraint_8 <<- c(constraint_8, createConstraintFreeForm(var, "=", perturbations[[x]]))
    
    varActState <- variables$nodesDf[variables$nodesDf == x, ]$nodesActStateVars
    constraint_8 <<- c(constraint_8, createConstraintFreeForm(var, "-", varActState, "=", 0))
  })
  
  constraint_8 <- list(constraint_8)
  names(constraint_8) <- constraint_8
  return(constraint_8)
}
