test_that("searchForCPLEXSolver works", {
  expect_type(searchForCPLEXSolver(),"character")
})

test_that("testRunCplex fails with random file", {
    expect_false(testRunCplex(tempfile()))
})


test_that("testRunLpSolve works", {
    expect_true(testRunLpSolve())
})


test_that("testRunCbc works", {
    
    expect_false(testRunCbc("random"))
    cbcPath = "~/Documents/SaezGroup/LocalGitRepo/cbc_optimizers/cbc-osx/cbc"    
    skip_if(!file.exists(cbcPath))
    expect_true(testRunCbc(cbcPath))
})
