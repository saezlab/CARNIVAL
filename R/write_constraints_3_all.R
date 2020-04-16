## This code writes the list of constraints (3) of the ILP problem for all the 
## conditions.
##
## Enio Gjerga, 2020

write_constraints_3_all <- function(variables=variables) {

  constraints3 <- c()

  for(i in seq_len(length(variables))){

    var <- variables[[i]]

    constraints3 <- c(constraints3, write_constraints_3(variables = var))

  }

  return(constraints3)

}
