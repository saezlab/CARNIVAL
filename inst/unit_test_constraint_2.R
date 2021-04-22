library(testthat)

### New function for Dummy Network with only one sign

createDummyVariablesSigned <- function(nNode = 3, sign=1) {
  
  variables <- c()
  
  variables$nodesDf <- data.frame(nodes=c(paste("node", seq(1,nNode), sep = "")), 
                                  nodesVars=c(paste("n", seq(1,nNode), sep = "")), 
                                  nodesUpVars = c(paste("nU", seq(1,nNode), sep = "")),
                                  nodesDownVars = c(paste("nD", seq(1,nNode), sep = "")),
                                  nodesActStateVars = c(paste("nAc", seq(1,nNode), sep = "")),
                                  nodesDistanceStateVars = c(paste("nDs", seq(1,nNode), sep = "")),
                                  nodesType = c("P", "", "M"))
  
  variables$edgesDf <- data.frame(Node1=c("node1", "node2"), 
                                  Sign=c(rep(sign, 2)), 
                                  Node2=c("node2", "node3"), 
                                  edgesUpVars=c("eU1", "eU2"), 
                                  edgesDownVars=c("eD1", "eD2")
  )
  
  variables$measurementsDf <- data.frame(nodes = "node3", 
                                         value = 5, 
                                         measurementsVars = "absDiff", 
                                         nodesVars = "n3" )
  
  return(variables)
}

# Dummy 1

dummyData <- createDummyVariablesSigned(3,1)
constraint3_expected <- c("eU1 + eD1 <= 1", "eU2 + eD2 <= 1")
constraint <- createConstraints_3_v2(variables=dummyData)

# Testing constraint 3
test_that("Comparison of the results", {
  expect_equal(constraint3_expected, constraint$c3)
})

# Dummy 2

dummyData <- createDummyVariablesSigned(3,-1)
constraint3_expected <- c("eU1 + eD1 <= 1", "eU2 + eD2 <= 1")
constraint <- createConstraints_3_v2(variables=dummyData)

# Testing constraint 3
test_that("Comparison of the results", {
  expect_equal(constraint3_expected, constraint$c3)
})

### New function for Dummy Network with both signs

createDummyVariables <- function(nNode = 3) {
  
  variables <- c()
  
  variables$nodesDf <- data.frame(nodes=c(paste("node", seq(1,nNode), sep = "")), 
                                  nodesVars=c(paste("n", seq(1,nNode), sep = "")), 
                                  nodesUpVars = c(paste("nU", seq(1,nNode), sep = "")),
                                  nodesDownVars = c(paste("nD", seq(1,nNode), sep = "")),
                                  nodesActStateVars = c(paste("nAc", seq(1,nNode), sep = "")),
                                  nodesDistanceStateVars = c(paste("nDs", seq(1,nNode), sep = "")),
                                  nodesType = c("P", "", "M", "", "M"))
  
  variables$edgesDf <- data.frame(Node1=c("node1", "node2", "node2", "node4"), 
                                  Sign=sample(c(1,-1), 4, replace = TRUE), 
                                  Node2=c("node2", "node3", "node4", "node5"), 
                                  edgesUpVars=c("eU1", "eU2", "eU3", "eU4"), 
                                  edgesDownVars=c("eD1", "eD2", "eD3", "eD4")
  )
  
  variables$measurementsDf <- data.frame(nodes = "node5", 
                                         value = 5, 
                                         measurementsVars = "absDiff", 
                                         nodesVars = "n3" )
  
  return(variables)
}


set.seed(100)
dummyData <- createDummyVariables(5)
constraint3_expected <- c("eU1 + eD1 <= 1", "eU2 + eD2 <= 1", "eU3 + eD3 <= 1", "eU4 + eD4 <= 1")
constraint <- createConstraints_3_v2(variables=dummyData)

# Testing constraint 3
test_that("Comparison of the results", {
  expect_equal(constraint3_expected, constraint$c3)
})

set.seed(500)
dummyData <- createDummyVariables(5)
constraint3_expected <- c("eU1 + eD1 <= 1", "eU2 + eD2 <= 1", "eU3 + eD3 <= 1", "eU4 + eD4 <= 1")
constraint <- createConstraints_3_v2(variables=dummyData)

# Testing constraint 3
test_that("Comparison of the results", {
  expect_equal(constraint3_expected, constraint$c3)
})
