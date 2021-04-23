# Dummy 1
dummyData <- createDummyVariablesSigned(nNode = 3, sign = 1)
constraint8_expected <- c("nU1 - nD1 + nAc1 - n1 = 0", "nU2 - nD2 + nAc2 - n2 = 0", 
                          "nU3 - nD3 + nAc3 - n3 = 0", "nAc2 = 0",                 
                          "nAc3 = 0", "n1 = 1","n1 - nAc1 = 0")
constraint <- createConstraints_8_v2(variables = dummyData, perturbations = c("node1" = 1))

test_that("Constraint 8 check:", {
  expect_setequal(constraint8_expected, constraint$c8)
})

# Dummy 2
dummyData <- createDummyVariablesSigned(nNode = 3, sign = -1)
constraint8_expected <- c("nU1 - nD1 + nAc1 - n1 = 0", "nU2 - nD2 + nAc2 - n2 = 0", 
                          "nU3 - nD3 + nAc3 - n3 = 0", "nAc2 = 0",
                          "nAc3 = 0", "n1 = 1", "n1 - nAc1 = 0")
constraint <- createConstraints_8_v2(variables = dummyData, perturbations = c("node1" = 1))

test_that("Constraint 8 check:", {
  expect_setequal(constraint8_expected, constraint$c8)
})

dummyData <- createDummyVariablesMixed(nNode = 5, seed = 100)
constraint8_expected <- c("nU1 - nD1 + nAc1 - n1 = 0", "nU2 - nD2 + nAc2 - n2 = 0", 
                          "nU3 - nD3 + nAc3 - n3 = 0", 
                          "nU4 - nD4 + nAc4 - n4 = 0", "nU5 - nD5 + nAc5 - n5 = 0", 
                          "nAc2 = 0", "nAc3 = 0", "nAc4 = 0", 
                          "nAc5 = 0", "n1 = 1", "n1 - nAc1 = 0")
constraint <- createConstraints_8_v2(variables=dummyData, perturbations = c("node1" = 1))

test_that("Constraint 8 check:", {
  expect_setequal(constraint8_expected, constraint$c8)
})

dummyData <- createDummyVariablesMixed(nNode = 5, seed = 400)
constraint8_expected <- c("nU1 - nD1 + nAc1 - n1 = 0", "nU2 - nD2 + nAc2 - n2 = 0", 
                          "nU3 - nD3 + nAc3 - n3 = 0", 
                          "nU4 - nD4 + nAc4 - n4 = 0", "nU5 - nD5 + nAc5 - n5 = 0", 
                          "nAc2 = 0", "nAc3 = 0", "nAc4 = 0", 
                          "nAc5 = 0", "n1 = 1", "n1 - nAc1 = 0")
constraint <- createConstraints_8_v2(variables=dummyData, perturbations = c("node1" = 1))

test_that("Constraint 8 check:", {
  expect_setequal(constraint8_expected, constraint$c8)
})
