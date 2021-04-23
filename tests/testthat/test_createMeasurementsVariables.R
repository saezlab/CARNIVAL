### This script is a unit test for the function createMeasurementsVariables
#
# Matteo Spatuzzi 2021

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

nodes <- data.frame(nodes=c(paste("Node", seq(1,5), sep = "")), 
                                              nodesVars=c(paste("n", seq(1,5), sep = "")), 
                                              nodesUpVars = c(paste("nU", seq(1,5), sep = "")),
                                              nodesDownVars = c(paste("nD", seq(1,5), sep = "")),
                                              nodesActStateVars = c(paste("nAc", seq(1,5), sep = "")),
                                              nodesDistanceVars = c(paste("nDs", seq(1,5), sep = "")),
                                              nodesType = c("P", "P", "P", "", "M"))
### Run function

measurementsVariables <- createMeasurementsVariables(measurements_1, nodes, priorKnowledgeNetwork_1)


### Expected Result
measurementsVariables_expected <- data.frame(nodes=c("Node5"), 
                                             value=c("5"), 
                                             measurementsVars=c("absDiff1"), 
                                             nodesVars=c("n5"))

### Test

test_that("Comparison of the results", {
  expect_equal(MeasurementsVariables_expected, MeasurementsVariables)
})
