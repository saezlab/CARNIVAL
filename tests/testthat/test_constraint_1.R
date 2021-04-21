## This function runs unit tests on the function createConstraints_1_2_v2
## 
## Matteo Spatuzzi 2021

library(testthat)

#### Create dummy data ####
createDummyVariables <- function(nNode, sign=1) {
  variables <- c()
  variables$nodesDf <- data.frame(nodes=c("node1", "node2"), nodesVars=c("x1", "x2"))
  variables$edgesDf <- data.frame(Node1=c("node1"), Sign=c(sign), Node2=c("node2"), 
                                  edgesUpVars=c("e1"))
  
  return(variables)
}

dummyData <- createDummyVariables()
constraint1_expected <- "e1 - x1 >= 0"
constraint1 <- createConstraints_1_2_v2(variables=dummyData)

# Testing constraint 1
test_that("Comparison of the results", {
  expect_equal(constraint1_expected, constraint1$c1)
})


