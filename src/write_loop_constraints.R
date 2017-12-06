write_loop_constraints <- function(variables=variables, distVariables=distVariables, pknList=pknList, inputs=inputs) {
  
  M <- 100
  
  constraints1 <- c()
  constraints2 <- c()
  constraints3 <- c()
  constraints4 <- c()
  constraints5 <- c()

  for(ii in 1:length(variables)){
    
    for(jj in 1:ncol(inputs)){
      
      # ##
      cc <- paste0(variables[[ii]]$variables[variables[[ii]]$idxNodesUp], " - ", variables[[ii]]$dist[[jj]], " <= 0")

      constraints1 <- c(constraints1, cc)

      ##
      cc <- paste0(variables[[ii]]$variables[variables[[ii]]$idxNodesDown], " - ", variables[[ii]]$dist[[jj]], " <= 0")

      constraints2 <- c(constraints2, cc)
      
      ##
      cc <- paste0(M, " ", variables[[ii]]$variables[variables[[ii]]$idxEdgesUp],
                   " + ", distVariables[sapply(strsplit(sapply(strsplit(variables[[ii]]$exp[variables[[ii]]$idxEdgesUp], split = " "), "[[", 2), split = "="), "[[", 1), colnames(inputs)[[jj]]],
                   " - ", distVariables[sapply(strsplit(sapply(strsplit(variables[[ii]]$exp[variables[[ii]]$idxEdgesUp], split = " "), "[[", 2), split = "="), "[[", 2), colnames(inputs)[[jj]]],
                   " <= ", M-1)
      
      constraints3 <- c(constraints3, cc)
      
      ##
      cc <- paste0(M, " ", variables[[ii]]$variables[variables[[ii]]$idxEdgesDown],
                   " + ", distVariables[sapply(strsplit(sapply(strsplit(variables[[ii]]$exp[variables[[ii]]$idxEdgesDown], split = " "), "[[", 2), split = "="), "[[", 1), colnames(inputs)[[jj]]],
                   " - ", distVariables[sapply(strsplit(sapply(strsplit(variables[[ii]]$exp[variables[[ii]]$idxEdgesDown], split = " "), "[[", 2), split = "="), "[[", 2), colnames(inputs)[[jj]]],
                   " <= ", M-1)
      
      constraints4 <- c(constraints4, cc)
      
      ##
      cc <- paste0(variables[[ii]]$dist[[jj]], " <= ", M)
      
      constraints5 <- c(constraints5, cc)
      
    }
    
  }
  
  constraints5 <- unique(constraints5)
  
  for(ii in 1:ncol(inputs)){
    
    constraints1 <- constraints1[-grep(pattern = paste0("dist_", colnames(inputs)[ii], "_", colnames(inputs)[ii]), x = constraints1)]
    constraints2 <- constraints2[-grep(pattern = paste0("dist_", colnames(inputs)[ii], "_", colnames(inputs)[ii]), x = constraints2)]
    constraints3 <- constraints3[-grep(pattern = paste0("dist_", colnames(inputs)[ii], "_", colnames(inputs)[ii]), x = constraints3)]
    constraints4 <- constraints4[-grep(pattern = paste0("dist_", colnames(inputs)[ii], "_", colnames(inputs)[ii]), x = constraints4)]
    constraints5 <- constraints5[-grep(pattern = paste0("dist_", colnames(inputs)[ii], "_", colnames(inputs)[ii]), x = constraints5)]
    
  }

  # return(c(constraints1, constraints2, constraints3, constraints4, constraints5))
  return(c(constraints3, constraints4, constraints5))

}
