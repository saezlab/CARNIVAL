#'\code{write_constraints_1_all}
#'
#'@param variables Contains the list of variables as used to formulate the ILP problem, explanations for each variable and a list of useful indeces.
#'@return This code writes the list of constraints (2) of the ILP problem for all the conditions.

write_constraints_2_all <- function(variables=variables){

  # ======================================= #
  # ====== Load write_constraints_2.R ===== #
  # ======================================= #

  write_constraints_2 <- function(variables=variables, conditionIDX=conditionIDX){

    constraints1 <- rep("", length(variables$idxEdgesDown))

    idx1 <- which(variables$signs==1)
    idx2 <- which(variables$signs==-1)

    constraints1[idx1] <- paste0(variables$variables[variables$idxEdgesDown[idx1]], " + ",
                                 variables$variables[match(paste0("Species ",
                                                                  unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesDown[idx1]], pattern = "ReactionDown ", replacement = ""), pattern = paste0(" in experiment ", conditionIDX), replacement = ""), split = "="))[c(TRUE, FALSE)],
                                                                  " in experiment ", conditionIDX), variables$exp)], " >= 0")

    constraints1[idx2] <- paste0(variables$variables[variables$idxEdgesDown[idx2]], " - ",
                                 variables$variables[match(paste0("Species ",
                                                                  unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesDown[idx2]], pattern = "ReactionDown ", replacement = ""), pattern = paste0(" in experiment ", conditionIDX), replacement = ""), split = "="))[c(TRUE, FALSE)],
                                                                  " in experiment ", conditionIDX), variables$exp)], " >= 0")

    return(constraints1)

  }

  # ======================================= #
  # ======================================= #
  # ======================================= #


  constraints2 <- c()

  for(i in 1:length(variables)){

    var <- variables[[i]]

    constraints2 <- c(constraints2, write_constraints_2(variables = var, conditionIDX = i))

  }

  return(constraints2)

}
