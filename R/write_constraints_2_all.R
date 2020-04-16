## This code writes the list of constraints (2) of the ILP problem for all the 
## conditions.
## 
## Enio Gjerga, 2020

write_constraints_2_all <- function(variables=variables){

  constraints2 <- c()

  for(i in seq_len(length(variables))){

    var <- variables[[i]]

    constraints2 <- c(constraints2, write_constraints_2(variables = var, 
                                                        conditionIDX = i))

  }

  return(constraints2)

}
