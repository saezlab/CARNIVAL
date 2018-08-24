write_constraints_3_all <- function(variables=variables) {

  # ======================================= #
  # ====== Load write_constraints_3.R ===== #
  # ======================================= #

  write_constraints_3 <- function(variables=variables) {

    constraints3 <- paste0(variables$variables[variables$idxEdgesUp], " + ", variables$variables[variables$idxEdgesDown], " <= 1")

    return(constraints3)

  }

  # ======================================= #
  # ======================================= #
  # ======================================= #

  constraints3 <- c()

  for(i in 1:length(variables)){

    var <- variables[[i]]

    constraints3 <- c(constraints3, write_constraints_3(variables = var))

  }

  return(constraints3)

}
