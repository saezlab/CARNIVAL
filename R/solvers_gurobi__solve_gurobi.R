#' Executes gurobi solver on provided .lp file.
#'
#' @param carnivalOptions
#'
#' @return
#' @keywords internal
#'
solveWithGurobi <- function(carnivalOptions) {

  resultFile <- carnivalOptions$filenames$resultFile
  resultFile <- stringr::str_replace(resultFile, ".txt", ".sol")
  lpFile <- carnivalOptions$filenames$lpFilename

  additional_params = ""
  if (("distributed" %in% names(carnivalOptions)) &  
    ("WorkerPassword" %in% names(carnivalOptions)) & 
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
                           " ResultFile=", resultFile,
                           additional_params,
                           " ", lpFile)

  system(gurobi_command)

  solutionMatrix <- read.csv2(resultFile, sep = ",", comment.char="#",
                              col.names=c("name", "solution"), header=F)

  return(solutionMatrix)
}

getSolutionMatrixGurobi <- function(solutionMatrix) {
  variablesNames <- solutionMatrix$name
  solutionMatrix <- as.matrix(solutionMatrix$solution)
  rownames(solutionMatrix) <- variablesNames

  return(solutionMatrix)
}
