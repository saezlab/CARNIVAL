
## Creating test:
## the following code adds log files from the ./cplex_logs to the internal data. 
#
# cplex_logs <- list.files("./cplex_logs/",full.names = TRUE)
# cplex_test_log_list <- lapply(cplex_logs,readr::read_lines)
# 
# usethis::use_data(cplex_test_log_list, internal = TRUE)
## end


test_that("testing CPLEX parser", {
    N_tests <- length(cplex_test_log_list)
    parsed_logs <- lapply(cplex_test_log_list, parse_CPLEX_log)
    
    expect_equal(length(parsed_logs), N_tests)
})

