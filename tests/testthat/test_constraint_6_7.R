# Dummy 1
dummyData <- createDummyVariablesSigned(nNode = 3, sign = 1)
constraint6_expected <- c("nU1 <= 0", "nU2 - eU1 <= 0", "nU3 - eU2 <= 0")
constraint7_expected <- c("nD1 <= 0", "nD2 - eD1 <= 0", "nD3 - eD2 <= 0")
constraint <- createConstraints_6_7_v2(variables=dummyData)

test_that("Constraint 6 check:", {
  expect_setequal(constraint6_expected, constraint$c6)
})

test_that("Constraint 7 check:", {
  expect_setequal(constraint7_expected, constraint$c7)
})

# Dummy 2
dummyData <- createDummyVariablesSigned(nNode = 3, sign = -1)
constraint6_expected <- c("nU1 <= 0", "nU2 - eU1 <= 0", "nU3 - eU2 <= 0")
constraint7_expected <- c("nD1 <= 0", "nD2 - eD1 <= 0", "nD3 - eD2 <= 0")
constraint <- createConstraints_6_7_v2(variables = dummyData)

test_that("Constraint 6 check:", {
  expect_setequal(constraint6_expected, constraint$c6)
})

test_that("Constraint 7 check:", {
  expect_setequal(constraint7_expected, constraint$c7)
})

dummyData <- createDummyVariablesMixed(nNode = 5, seed = 100)
constraint6_expected <- c("nU1 <= 0", "nU2 - eU1 <= 0", "nU3 - eU2 <= 0", 
                          "nU4 - eU3 <= 0", "nU5 - eU4 <= 0")
constraint7_expected <- c("nD1 <= 0", "nD2 - eD1 <= 0", "nD3 - eD2 <= 0", 
                          "nD4 - eD3 <= 0", "nD5 - eD4 <= 0")
constraint <- createConstraints_6_7_v2(variables=dummyData)

test_that("Constraint 6 check:", {
  expect_setequal(constraint6_expected, constraint$c6)
})


test_that("Constraint 7 check:", {
  expect_equal(constraint7_expected, constraint$c7)
})


dummyData <- createDummyVariablesMixed(nNode = 5, seed = 400)
constraint6_expected <- c("nU1 <= 0", "nU2 - eU1 <= 0", "nU3 - eU2 <= 0", 
                          "nU4 - eU3 <= 0", "nU5 - eU4 <= 0")
constraint7_expected <- c("nD1 <= 0", "nD2 - eD1 <= 0", "nD3 - eD2 <= 0", 
                          "nD4 - eD3 <= 0", "nD5 - eD4 <= 0")
constraint <- createConstraints_6_7_v2(variables=dummyData)

test_that("Constraint 6 check:", {
  expect_setequal(constraint6_expected, constraint$c6)
})

test_that("Constraint 7 check:", {
  expect_setequal(constraint7_expected, constraint$c7)
})

