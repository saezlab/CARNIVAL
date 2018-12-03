#'\code{write_constraints_objFunction_all}
#'
#'@param variables Contains the list of variables as used to formulate the ILP problem, explanations for each variable and a list of useful indeces.
#'@param dataMatrix Contains the matrix which stores the data for running CARNIVAL and a set of identifiers for Targets, Measured and Un-measured nodes.
#'@return This function returns the list of constraints associated with the 'Absolute Difference' variables and which measure the mis-fit between inferred and measured data.

write_constraints_objFunction_all <- function(variables=variables, dataMatrix=dataMatrix) {

  # ============================================ #
  # === Load write_constraints_objFunction.R === #
  # ============================================ #

  write_constraints_objFunction <- function(variables=variables, dataMatrix=dataMatrix, conditionIDX=conditionIDX){

    measurements <- as.vector(t(dataMatrix$dataMatrixSign))

    idx2 <- which(measurements==1)
    idx3 <- which(measurements==-1)

    cc1 <- rep("", length(measurements))
    cc2 <- rep("", length(measurements))

    cc1[idx2] <- paste0(variables$variables[idx2], " - absDiff", idx2, "_", conditionIDX, " <= 1")
    cc2[idx2] <- paste0(variables$variables[idx2], " + absDiff", idx2, "_", conditionIDX, " >= 1")

    cc1[idx3] <- paste0(variables$variables[idx3], " - absDiff", idx3, "_", conditionIDX, " <= -1")
    cc2[idx3] <- paste0(variables$variables[idx3], " + absDiff", idx3, "_", conditionIDX, " >= -1")

    constraints0 <- c(cc1, cc2)

    return(constraints0[-which(constraints0=="")])

  }

  # ============================================ #
  # ============================================ #
  # ============================================ #

  constraints0 <- c()

  for (i in 1:nrow(dataMatrix$dataMatrix)) {

    dM <- dataMatrix
    dM$dataMatrix <- as.matrix(t(dataMatrix$dataMatrix[i, ]))
    dM$dataMatrixSign <- as.matrix(t(dataMatrix$dataMatrixSign[i, ]))

    var <- variables[[i]]

    constraints0 <- c(constraints0, write_constraints_objFunction(variables = var, dataMatrix = dM, conditionIDX = i))

  }

  return(constraints0)

}
