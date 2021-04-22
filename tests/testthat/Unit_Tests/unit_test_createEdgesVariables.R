### This script is a unit test for the function createEdgesVariables
#
# Matteo Spatuzzi 2021

library(testthat)

#Create Dummy Data

priorKnowledgeNetwork_1 = matrix(c(nodenames[1], 1, nodenames[4], 
                                   nodenames[2], 1, nodenames[4],
                                   nodenames[3], -1, nodenames[1],
                                   nodenames[3], -1, nodenames[2],
                                   nodenames[4], 1, nodenames[5]), byrow = T, ncol = 3) %>% as.data.frame()
colnames(priorKnowledgeNetwork_1) = c("Node1", "sign", "Node2")

### Run function

EdgesVariables <- createEdgesVariables(priorKnowledgeNetwork_1)


### Expected Result

EdgesVariables_expected <- data.frame(Node1=c("Node1", "Node2", "Node3", "Node3", "Node4"), 
                                      Sign=c(1,1,-1,-1,1), 
                                      Node2=c("Node4", "Node4", "Node1", "Node2", "Node5"), 
                                      edgesUpVars=c("eU1", "eU2", "eU3", "eU4", "eU5"), 
                                      edgesDownVars=c("eD1", "eD2", "eD3", "eD4", "eD5"))

### Test

test_that("Comparison of the results", {
  expect_equal(NodesVariables_expected, NodesVariables)
})
              
### Run function

EdgesVariables <- createEdgesVariables(priorKnowledgeNetwork_1, c("edgeUp" = "EU", 
                                                                  "edgeDown" = "ED"))


### Expected Result

EdgesVariables_expected <- data.frame(Node1=c("Node1", "Node2", "Node3", "Node3", "Node4"), 
                                      Sign=c(1,1,-1,-1,1), 
                                      Node2=c("Node4", "Node4", "Node1", "Node2", "Node5"), 
                                      edgesUpVars=c("EU1", "EU2", "EU3", "EU4", "EU5"), 
                                      edgesDownVars=c("ED1", "ED2", "ED3", "ED4", "ED5"))

### Test

test_that("Comparison of the results", {
  expect_equal(NodesVariables_expected, NodesVariables)
})


                                      