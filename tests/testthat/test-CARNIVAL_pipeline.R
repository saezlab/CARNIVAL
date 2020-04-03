library(CARNIVAL)

# get input data
load(file = system.file("toy_inputs_ex1.RData",
                        package="CARNIVAL"))
load(file = system.file("toy_measurements_ex1.RData",
                        package="CARNIVAL"))
load(file = system.file("toy_network_ex1.RData",
                        package="CARNIVAL"))

# get expected result
load(file = system.file("result_expected.RData",
                        package="CARNIVAL"))

# obtain actual reesult
result_actual = runCARNIVAL(inputObj = toy_inputs_ex1, 
                            measObj = toy_measurements_ex1, 
                            netObj = toy_network_ex1)

#testing
test_that("Comparison of the results", {
  expect_equal(result_actual, result_expected)
})