## This code writes the list of constraints (8) of the ILP problem.
##
## Enio Gjerga, Olga Ivanova 2020-2021

createConstraints_8_v2 <- function(variables, perturbations, constraintName=c("c8")) {

  # "Sync" all predicted states values with perturbation data
  constraint_8 <- lapply(variables$nodesDf$nodes, function(x) {
                                row <- variables$nodesDf[variables$nodesDf$nodes == x, ]
                                createConstraintFreeForm(row$nodesUpVars, "-",
                                                         row$nodesDownVars, "+",
                                                         row$nodesActStateVars, "-",
                                                         row$nodesVars,
                                                         "=", 0)
                          })
  constraint_8 <- unlist(constraint_8)

  # Defines activations state for unperturbed nodes
  unperturbedNodes <- variables$nodesDf[!variables$nodesDf$nodes %in% names(perturbations), ]
  constraint_8Temp <- lapply(unperturbedNodes$nodes, function(x) {
                                  var <- variables$nodesDf[variables$nodesDf$nodes == x, ]$nodesActStateVars
                                  createConstraintFreeForm(var, "=", 0)
                            })
  constraint_8 <- c(constraint_8, unlist(constraint_8Temp))

  # Defines activation state for perturbation nodes
  constraint_8Temp <- lapply(names(perturbations), function(x) {
                                  var <- variables$nodesDf[variables$nodesDf$nodes == x, ]$nodesVars
                                  createConstraintFreeForm(var, "=", perturbations[[x]])
                            })
  constraint_8 <- c(constraint_8, unlist(constraint_8Temp))

  parentNodes <- setdiff(variables$edgesDf$Node1, variables$edgesDf$Node2)
  constraint_8Temp <- lapply(parentNodes, function(x) {
                                  var <- variables$nodesDf[variables$nodesDf$nodes == x,
                                                         c("nodesVars", "nodesActStateVars")]
                                  createConstraintFreeForm(var$nodesVars, "-",
                                                           var$nodesActStateVars,
                                                           "=", 0)
                             })
  constraint_8 <- c(constraint_8, unlist(constraint_8Temp))

  constraint_8 <- list(constraint_8)
  names(constraint_8) <- constraintName
  return(constraint_8)
}
