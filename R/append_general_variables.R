#'\code{append_general_variables}
#'
#' Appending the general variables for each condition
#' 
#' Enio Gjerga, 2020

append_general_variables <- function(variables = variables){
  
  for(ii in 1:length(variables)){
    
    cnt <- 
      length(variables[[ii]]$idxNodes) + 
      length(variables[[ii]]$idxNodesUp) + 
      length(variables[[ii]]$idxNodesDown) + 
      length(variables[[ii]]$idxEdgesUp) + 
      length(variables[[ii]]$idxEdgesDown) + 1
    var2append <- paste0("xb", 
                         cnt:(cnt+length(variables[[ii]]$idxEdgesUp)-1), 
                         "_", ii)
    exp2append <- gsub(pattern = "ReactionUp ", replacement = "Reaction ", 
                       x = variables[[ii]]$exp[variables[[ii]]$idxEdgesUp], 
                       " in experiment ", ii)
    idxEdges <- (length(
      variables[[ii]]$variables)+1):(length(variables[[ii]]$variables) + 
                                       length(var2append))
    namesVar <- names(variables[[ii]])
    variables[[ii]]$variables <- c(variables[[ii]]$variables, var2append)
    variables[[ii]]$exp <- c(variables[[ii]]$exp, exp2append)
    
    variables[[ii]][[length(variables[[ii]])+1]] <- idxEdges
    
    names(variables[[ii]]) <- c(namesVar, "idxEdges")
    
  }
  
  var = variables
  
  var[[length(var)+1]] <- list()
  var[[length(var)]][[length(var[[length(var)]])+1]] <-
    paste0("y", 1:length(variables$Condition_1$idxEdgesUp))
  var[[length(var)]][[length(var[[length(var)]])+1]] <- 
    gsub(pattern = " in experiment 1", replacement = "", 
         x = gsub(
           pattern = "ReactionUp ", replacement = "Reaction ", 
           x = variables$Condition_1$exp[variables$Condition_1$idxEdgesUp]))
  
  names(var) <- c(names(variables), "Reaction Variables")
  names(var$`Reaction Variables`) <- c("Variables", "Explanation")
  
  return(var)
  
}