## This code writes the list of constraints (5) of the ILP problem for all the 
## conditions.
## 
## Enio Gjerga, 2020

write_constraints_5_all <- function(variables=variables) {

  constraints5 <- c()

  for(i in seq_len(length(variables))){

    var <- variables[[i]]

    constraints5 <- c(constraints5, write_constraints_5(variables = var, 
                                                        conditionIDX = i))

  }

  return(constraints5)

}
