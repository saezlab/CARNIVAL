write_experimental_conditions_constraints <- function(variables = variables){
  
  constraint1 = c()
  for(jj in 1:length(variables$`Reaction Variables`$Explanation)){
    
    for(ii in 1:(length(variables)-1)){
      
      tt <- strsplit(x = strsplit(x = variables[[ii]]$exp[variables[[ii]]$idxEdgesUp[jj]], split = " ", fixed = TRUE)[[1]][2], split = "=", fixed = TRUE)[[1]][2]
      ss <- strsplit(x = strsplit(x = variables[[ii]]$exp[variables[[ii]]$idxEdgesUp[jj]], split = " ", fixed = TRUE)[[1]][2], split = "=", fixed = TRUE)[[1]][1]
      
      var_u_p <- variables[[ii]]$variables[variables[[ii]]$idxEdgesUp[jj]]
      var_x_p <- variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("SpeciesUP ", tt, " in experiment ", ii))]
      
      c1 = paste0("andP_", ss, "_", tt, "_", ii, " - ", var_u_p, " - ", var_x_p, " >= -1")
      c2 = paste0("andP_", ss, "_", tt, "_", ii, " - ", var_u_p, " <= 0")
      c3 = paste0("andP_", ss, "_", tt, "_", ii, " - ", var_x_p, " <= 0")
      
      var_u_m <- variables[[ii]]$variables[variables[[ii]]$idxEdgesDown[jj]]
      var_x_m <- variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("SpeciesDown ", tt, " in experiment ", ii))]
      
      c4 = paste0("andM_", ss, "_", tt, "_", ii, " - ", var_u_m, " - ", var_x_m, " >= -1")
      c5 = paste0("andM_", ss, "_", tt, "_", ii, " - ", var_u_m, " <= 0")
      c6 = paste0("andM_", ss, "_", tt, "_", ii, " - ", var_x_m, " <= 0")
      
      constraint1 = c(constraint1, c(c1, c2, c3, c4, c5, c6))
      
    }
    
  }
  
  constraint2 = c()
  for(jj in 1:length(variables$`Reaction Variables`$Explanation)){
    
    for(ii in 1:(length(variables)-1)){
      
      ss <- strsplit(x = strsplit(x = variables[[ii]]$exp[variables[[ii]]$idxEdges[jj]], split = " ", fixed = TRUE)[[1]][2], split = "=", fixed = TRUE)[[1]][1]
      tt <- strsplit(x = strsplit(x = variables[[ii]]$exp[variables[[ii]]$idxEdges[jj]], split = " ", fixed = TRUE)[[1]][2], split = "=", fixed = TRUE)[[1]][2]
      
      c1 <- paste0(variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]], " - andP_", ss, "_", tt, "_", ii, " - andM_", ss, "_", tt, "_", ii, " <= 0")
      c2 <- paste0(variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]], " - andP_", ss, "_", tt, "_", ii, " >= 0")
      c3 <- paste0(variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]], " - andM_", ss, "_", tt, "_", ii, " >= 0")
      
      constraint2 = c(constraint2, c(c1, c2, c3))
      
    }
    
  }
  
  constraint3 = c()
  for(jj in 1:length(variables$`Reaction Variables`$Explanation)){
    
    c1 = variables$`Reaction Variables`$Variables[jj]
    c2 = c()
    
    for(ii in 1:(length(variables)-1)){
      
      c1 = paste0(c1, " - ", variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]])
      c2 = c(c2, paste0(variables$`Reaction Variables`$Variables[jj], " - ", variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]], " >= ", 0))
      
    }
    
    c1 <- paste0(c1, " <= 0")
    constraint3 <- c(constraint3, c(c1, c2))
    
  }
  
  
  # constraints = c()
  # 
  # constraints1 = c()
  # for(ii in 1:(length(variables)-1)){
  #   
  #   c1 = paste0(variables[[ii]]$variables[variables[[ii]]$idxEdges], " - ", variables[[ii]]$variables[variables[[ii]]$idxEdgesUp], " - ", variables[[ii]]$variables[variables[[ii]]$idxEdgesDown], " = 0")
  #   constraints1 <- c(constraints1, c1)
  #   
  # }
  # 
  # constraints2 = c()
  # for(ii in 1:length(variables$`Reaction Variables`$Variables)){
  #   
  #   for(jj in 1:(length(variables)-1)){
  #     
  #     c2 = variables$`Reaction Variables`$Variables[ii]
  #     c2 <- paste0(c2, " - ", variables[[jj]]$variables[variables[[jj]]$idxEdges[ii]])
  #     
  #     constraints2 = c(constraints2, paste0(c2, " <= 0"))
  #     
  #   }
  #   
  # }
  # 
  # constraints = c(constraints1, constraints2)
  
  #
  # constraint1 = c()
  # for(ii in 1:(length(variables)-1)){
  #   
  #   for(jj in 1:length(variables[[ii]]$idxEdges)){
  #     
  #     reaction = strsplit(x = variables[[ii]]$exp[variables[[ii]]$idxEdges[jj]], split = " ", fixed = TRUE)[[1]][2]
  #     # ss <- strsplit(x = reaction, split = "=", fixed = TRUE)[[1]][1]
  #     tt <- strsplit(x = reaction, split = "=", fixed = TRUE)[[1]][2]
  #     
  #     reacVar = variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("ReactionUp ", reaction, " in experiment ", ii))]
  #     targetVar = variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("SpeciesUP ", tt, " in experiment ", ii))]
  #     
  #     c1 = paste0(variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]], " - ", reacVar, " - ", targetVar, " >= 1")
  #     c2 = paste0(variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]], " - ", reacVar, " <= 0")
  #     c3 = paste0(variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]], " - ", targetVar, " <= 0")
  #     
  #     reacVar = variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("ReactionDown ", reaction, " in experiment ", ii))]
  #     targetVar = variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("SpeciesDown ", tt, " in experiment ", ii))]
  #     
  #     c4 = paste0(variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]], " - ", reacVar, " - ", targetVar, " >= 1")
  #     c5 = paste0(variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]], " - ", reacVar, " <= 0")
  #     c6 = paste0(variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]], " - ", targetVar, " <= 0")
  #     
  #     constraint1 = c(constraint1, c(c1, c2, c3, c4, c5, c6))
  #     
  #   }
  #   
  # }
  
  #
  # constraint3 = c()
  # for(ii in 1:length(variables$`Reaction Variables`$Variables)){
  # 
  #   for(jj in 1:(length(variables)-1)){
  # 
  #     c2 = variables$`Reaction Variables`$Variables[ii]
  #     c2 <- paste0(c2, " - ", variables[[jj]]$variables[variables[[jj]]$idxEdges[ii]])
  # 
  #     constraint3 = c(constraint3, paste0(c2, " >= 0"))
  # 
  #   }
  # 
  # }

  constraints = c(constraint1, constraint2, constraint3)
  
  return(constraints)
  
}