write_constraints_1_all <- function(variables=variables){

  # ======================================= #
  # ====== Load write_constraints_1.R ===== #
  # ======================================= #

  write_constraints_1 <- function(variables=variables, conditionIDX=conditionIDX) {

    constraints1 <- rep("", length(variables$idxEdgesUp))

    idx1 <- which(variables$signs==1)
    idx2 <- which(variables$signs==-1)

    constraints1[idx1] <- paste0(variables$variables[variables$idxEdgesUp[idx1]], " - ",
                                 variables$variables[match(paste0("Species ",
                                                                  unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesUp[idx1]], pattern = "ReactionUp ", replacement = ""), pattern = paste0(" in experiment ", conditionIDX), replacement = ""), split = "="))[c(TRUE, FALSE)],
                                                                  " in experiment ", conditionIDX), variables$exp)], " >= 0")

    constraints1[idx2] <- paste0(variables$variables[variables$idxEdgesUp[idx2]], " + ",
                                 variables$variables[match(paste0("Species ",
                                                                  unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesUp[idx2]], pattern = "ReactionUp ", replacement = ""), pattern = paste0(" in experiment ", conditionIDX), replacement = ""), split = "="))[c(TRUE, FALSE)],
                                                                  " in experiment ", conditionIDX), variables$exp)], " >= 0")

    return(constraints1)

  }

  # ======================================= #
  # ======================================= #
  # ======================================= #

  constraints1 <- c()

  for(i in 1:length(variables)){

    var <- variables[[i]]

    constraints1 <- c(constraints1, write_constraints_1(variables = var, conditionIDX = i))

  }

  return(constraints1)

}
