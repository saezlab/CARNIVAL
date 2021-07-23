#' Executes gurobi solver on provided .lp file.
#'
#' @param carnivalOptions
#'
#' @return
#' @keywords internal
#'
solveWithGurobi <- function(carnivalOptions) {

    resultFile <- carnivalOptions$filenames$resultFile
    resultFile <- stringr::str_sub(resultFile, end=-5)
    lpFile <- carnivalOptions$filenames$lpFilename
    
    additional_params = ""
    if (("distributed" %in% names(carnivalOptions)) &&  
      ("WorkerPassword" %in% names(carnivalOptions)) && 
      (carnivalOptions$distributed)) {
    
        node_port <- 61000
        if ("NodePort" %in% names(carnivalOptions)) {
          node_port <- carnivalOptions$NodePort
        }
    
        nodes <- system(paste0("scontrol show hostnames $SLURM_JOB_NODELIST | ", 
                               "tr '\n' ',' | ", 
                               "sed \"s/,/:", node_port, ",/g\""), 
                        intern=TRUE)
    
    
        if (length(nodes)) {
          additional_params <- paste0(" WorkerPool=", nodes,
                                      " WorkerPassword=", carnivalOptions$WorkerPassword,
                                      " DistributedMIPJobs=${SLURM_JOB_NUM_NODES}")
    
        }
    }
    
    gurobi_command <- paste0(carnivalOptions$solverPath,
                             " MIPGAP=", carnivalOptions$mipGap,
                             " TimeLimit=", carnivalOptions$timelimit,
                             " PoolGap=", carnivalOptions$poolrelGap,
                             " SolutionLimit=", carnivalOptions$limitPop,
                             " PoolSolutions=", carnivalOptions$poolCap,
                             " Threads=", carnivalOptions$threads,
                             " SolFiles=", resultFile,
                             " PoolSearchMode=2 NumericFocus=2",
                             " ResultFile=", paste0(resultFile, ".sol"),
                             additional_params,
                             " ", lpFile)
    
    system(gurobi_command)
    
    # Return name of the result file without ".sol" extension.
    # Used to get all of the solution files as Gurobi saves 
    # each solution in separate file
    return(resultFile)
}

getSolutionMatrixGurobi <- function(sol_name_prefix) {

    # Set the name of the main solution file
    main_sol <- paste0(sol_name_prefix, ".sol")

    # Get the objective function value of the main solution
    opt_val <- as.numeric(tail(scan(main_sol, nlines=2, what=character(), quiet=T), 1))

    # Get the names of the alternative solutions 
    # (includes intermediate, non-optimal solutions)
    sol_names <- list.files(path=dirname(sol_name_prefix), 
                            pattern=paste0(basename(sol_name_prefix), "_"), 
                            full.names=T)

    # Initialise the solution matrix
    solMatrix <- read.csv2(main_sol, sep=" ", comment.char="#", header=F, 
                           row.names=1, col.names=c("Names", "Solution-0"))

    # Loop through the other solution files
    for (i in 1:length(sol_names)-1) {
        # Name of the solution file
        x <- paste0(sol_name_prefix, "_", i, ".sol")

        # Objective function value of this solution 
        obj_val <- as.numeric(tail(scan(x, nlines=2, what=character(), quiet=T), 1))

        # If the solution has the same objective function value as the main,
        # add it to the solution matrix
        if (abs(obj_val - opt_val) < 1e-5) {
            sol <- read.csv2(x, sep=" ", 
                             comment.char="#", 
                             header=F, 
                             row.names=1, 
                             col.names=c("Names", paste0("Solution-", i)))
            solMatrix <- cbind(solMatrix, sol)
        }
    }

    # Remove the main solution from solution matrix
    # as it is the same as one of the numbered ones
    solMatrix <- as.matrix(solMatrix[-1])

    # Return the solution matrix
    return(solMatrix)
}
