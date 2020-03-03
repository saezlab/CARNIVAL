## This code writes the list of constraints (4) of the ILP problem for all the 
## conditions.
## 
## Enio Gjerga, 2020

write_constraints_4_all <- function(variables=variables) {

  constraints4 <- c()

  for(i in 1:length(variables)){

    var <- variables[[i]]

    constraints4 <- c(constraints4, write_constraints_4(variables = var, 
                                                        conditionIDX = i))

  }

  return(constraints4)

}
