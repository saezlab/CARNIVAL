### This script is a unit test for the function createNodesVariables
#
# Matteo Spatuzzi 2021

library(testthat)
#Create Dummy Data

nodenames = c(paste("Node", seq(1,5), sep = ""))

perturbations_1 = c(1,-1, 1) 
names(perturbations_1) = c(nodenames[1], nodenames[2], nodenames[3])

measurements_1 = c(5)
names(measurements_1) = c(nodenames[5])

priorKnowledgeNetwork_1 = matrix(c(nodenames[1], 1, nodenames[4], 
                                   nodenames[2], 1, nodenames[4],
                                   nodenames[3], -1, nodenames[1],
                                   nodenames[3], -1, nodenames[2],
                                   nodenames[4], 1, nodenames[5]), byrow = T, ncol = 3) %>% as.data.frame()
colnames(priorKnowledgeNetwork_1) = c("Node1", "sign", "Node2")

### Run function on Dummy Data

NodesVariables = createNodesVariables(priorKnowledgeNetwork = priorKnowledgeNetwork_1,
                     perturbations = perturbations_1, 
                     measurements = measurements_1)

### Expected Result

NodesVariables_expected = data.frame(nodes=c(paste("Node", seq(1,5), sep = "")), 
                          nodesVars=c(paste("n", seq(1,5), sep = "")), 
                          nodesUpVars = c(paste("nU", seq(1,5), sep = "")),
                          nodesDownVars = c(paste("nD", seq(1,5), sep = "")),
                          nodesActStateVars = c(paste("nAc", seq(1,5), sep = "")),
                          nodesDistanceVars = c(paste("nDs", seq(1,5), sep = "")),
                          nodesType = c("P", "P", "P", "", "M"))

### Test

test_that("Comparison of the results", {
  expect_equal(NodesVariables_expected, NodesVariables)
})



NodesVariables = createNodesVariables(priorKnowledgeNetwork = priorKnowledgeNetwork_1,
                                      perturbations = perturbations_1, 
                                      measurements = measurements_1, prefixes = c("nodes" = "N", "nodesUp" = "NU", 
                                                                                  "nodesDown" = "ND", 
                                                                                  "nodesActivationState" = "NAc",
                                                                                  "nodesDistance" = "NDs"))

### Expected Result

NodesVariables_expected = data.frame(nodes=c(paste("Node", seq(1,5), sep = "")), 
                                     nodesVars=c(paste("N", seq(1,5), sep = "")), 
                                     nodesUpVars = c(paste("NU", seq(1,5), sep = "")),
                                     nodesDownVars = c(paste("ND", seq(1,5), sep = "")),
                                     nodesActStateVars = c(paste("NAc", seq(1,5), sep = "")),
                                     nodesDistanceVars = c(paste("NDs", seq(1,5), sep = "")),
                                     nodesType = c("P", "P", "P", "", "M"))

### Test

test_that("Comparison of the results", {
  expect_equal(NodesVariables_expected, NodesVariables)
})

