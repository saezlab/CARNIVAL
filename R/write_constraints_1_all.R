## This code writes the list of constraints (1) of the ILP problem for all the 
## conditions.
## 
## Enio Gjerga

write_constraints_1_all <- function(variables=variables){

  constraints1 <- c()

  for(i in seq_len(length(variables))){

    var <- variables[[i]]

    constraints1 <- c(constraints1, write_constraints_1(variables = var, 
                                                        conditionIDX = i))

  }

  return(constraints1)

}
