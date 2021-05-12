# Dummy data with activation edges
dummyData <- createDummyVariablesSigned(nNode = 3, sign = 1)
constraint1_expected <- c("eU1 - n1 >= 0", "eU2 - n2 >= 0")
constraint2_expected <- c("eD1 + n1 >= 0", "eD2 + n2 >= 0")
constraint <- createConstraints_1_2_v2(variables = dummyData)

# Testing constraint 1
test_that("Constraint 1 check:", {
  expect_equal(constraint1_expected, constraint$c1)
})

# Testing constraint 2
test_that("Constraint 2 check:", {
  expect_equal(constraint2_expected, constraint$c2)
})


# Dummy data with inhibition edges
dummyData <- createDummyVariablesSigned(nNode = 3, sign = -1)
constraint1_expected <- c("eU1 + n1 >= 0", "eU2 + n2 >= 0")
constraint2_expected <- c("eD1 - n1 >= 0", "eD2 - n2 >= 0")
constraint <- createConstraints_1_2_v2(variables = dummyData)

# Testing constraint 1
test_that("Constraint 1 check:", {
  expect_equal(constraint1_expected, constraint$c1)
})

# Testing constraint 2
test_that("Constraint 2 check:", {
  expect_equal(constraint2_expected, constraint$c2)
})


dummyData <- createDummyVariablesMixed(nNode = 5, seed = 100)
constraint1_expected <- c("eU1 + n1 >= 0", "eU2 - n2 >= 0", "eU4 + n4 >= 0","eU3 + n2 >= 0")
constraint2_expected <- c("eD1 - n1 >= 0", "eD2 + n2 >= 0", "eD4 - n4 >= 0","eD3 - n2 >= 0")
constraint <- createConstraints_1_2_v2(variables = dummyData)

# Testing constraint 1
test_that("Comparison of the results", {
  expect_setequal(constraint1_expected, constraint$c1)
})

# Testing constraint 2
test_that("Comparison of the results", {
  expect_setequal(constraint2_expected, constraint$c2)
})


dummyData <- createDummyVariablesMixed(nNode = 5, seed = 400)
constraint1_expected <- c("eU4 - n4 >= 0","eU1 - n1 >= 0", "eU2 + n2 >= 0", "eU3 - n2 >= 0")
constraint2_expected <- c("eD4 + n4 >= 0","eD1 + n1 >= 0", "eD2 - n2 >= 0", "eD3 + n2 >= 0")
constraint <- createConstraints_1_2_v2(variables = dummyData)

# Testing constraint 1
test_that("Constraint 1 check:", {
  expect_setequal(constraint1_expected, constraint$c1)
})

# Testing constraint 2
test_that("Constraint 2 check dummy:", {
  expect_setequal(constraint2_expected, constraint$c2)
})

