# Dummy 1
dummyData <- createDummyVariablesSigned(nNode = 3, sign = 1)
constraint3_expected <- c("eU1 + eD1 <= 1", "eU2 + eD2 <= 1")
constraint <- createConstraints_3_v2(variables=dummyData)

# Testing constraint 1
test_that("Constraint 3 check:", {
  expect_equal(constraint3_expected, constraint$c3)
})

# Dummy 2
dummyData <- createDummyVariablesSigned(nNode = 3, sign = -1)
constraint3_expected <- c("eU1 + eD1 <= 1", "eU2 + eD2 <= 1")
constraint <- createConstraints_3_v2(variables=dummyData)

# Testing constraint 1
test_that("Constraint 3 check:", {
  expect_equal(constraint3_expected, constraint$c3)
})


dummyData <- createDummyVariablesMixed(nNode = 5, seed = 100)
constraint3_expected <- c("eU1 + eD1 <= 1", "eU2 + eD2 <= 1", "eU3 + eD3 <= 1", "eU4 + eD4 <= 1")
constraint <- createConstraints_3_v2(variables = dummyData)

# Testing constraint 3
test_that("Constraint 3 check:", {
  expect_equal(constraint3_expected, constraint$c3)
})


dummyData <- createDummyVariablesMixed(nNode = 5, seed = 500)
constraint3_expected <- c("eU1 + eD1 <= 1", "eU2 + eD2 <= 1", "eU3 + eD3 <= 1", "eU4 + eD4 <= 1")
constraint <- createConstraints_3_v2(variables=dummyData)

# Testing constraint 3
test_that("Constraint 3 check:", {
  expect_equal(constraint3_expected, constraint$c3)
})

