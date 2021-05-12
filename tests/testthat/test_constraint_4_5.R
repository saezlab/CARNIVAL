# Dummy 1
dummyData <- createDummyVariablesSigned(nNode = 3, sign = 1)
constraint4_expected <- c("eU1 - n1 - eD1 <= 0", "eU2 - n2 - eD2 <= 0")
constraint5_expected <- c("eD1 + n1 - eU1 <= 0", "eD2 + n2 - eU2 <= 0")
constraint <- createConstraints_4_5_v2(variables=dummyData)

# Testing constraint 4
test_that("Constraint 4 check:", {
  expect_setequal(constraint4_expected, constraint$c4)
})

# Testing constraint 5
test_that("Constraint 5 check:", {
  expect_setequal(constraint5_expected, constraint$c5)
})

# Dummy 2
dummyData <- createDummyVariablesSigned(3,-1)
constraint4_expected <- c("eU1 + n1 - eD1 <= 0", "eU2 + n2 - eD2 <= 0")
constraint5_expected <- c("eD1 - n1 - eU1 <= 0", "eD2 - n2 - eU2 <= 0")
constraint <- createConstraints_4_5_v2(variables=dummyData)

# Testing constraint 4
test_that("Constraint 4 check:", {
  expect_setequal(constraint4_expected, constraint$c4)
})

# Testing constraint 5
test_that("Constraint 5 check:", {
  expect_setequal(constraint5_expected, constraint$c5)
})

dummyData <- createDummyVariablesMixed(nNode = 5, seed = 100)

constraint4_expected <- c("eU1 + n1 - eD1 <= 0", "eU2 - n2 - eD2 <= 0", 
                          "eU3 + n2 - eD3 <= 0", "eU4 + n4 - eD4 <= 0")
constraint5_expected <- c("eD1 - n1 - eU1 <= 0", "eD2 + n2 - eU2 <= 0",
                          "eD3 - n2 - eU3 <= 0", "eD4 - n4 - eU4 <= 0")
constraint <- createConstraints_4_5_v2(variables = dummyData)

# Testing constraint 4
test_that("Constraint 4 check:", {
  expect_setequal(constraint4_expected, constraint$c4)
})

# Testing constraint 5
test_that("Constraint 5 check:", {
  expect_setequal(constraint5_expected, constraint$c5)
})

dummyData <- createDummyVariablesMixed(nNode = 5, seed = 500)
constraint4_expected <- c("eU4 - n4 - eD4 <= 0", "eU1 - n1 - eD1 <= 0", 
                          "eU2 - n2 - eD2 <= 0", "eU3 - n2 - eD3 <= 0")
constraint5_expected <- c("eD4 + n4 - eU4 <= 0", "eD1 + n1 - eU1 <= 0",
                          "eD2 + n2 - eU2 <= 0", "eD3 + n2 - eU3 <= 0")
constraint <- createConstraints_4_5_v2(variables = dummyData)

# Testing constraint 4
test_that("Constraint 4 check:", {
  expect_setequal(constraint4_expected, constraint$c4)
})

# Testing constraint 5
test_that("Constraint 5 check:", {
  expect_setequal(constraint5_expected, constraint$c5)
})

